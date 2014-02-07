#lang racket
(require parser-tools/lex)
(require parser-tools/yacc)

;-----------
; Oper defs
;-----------

(define-lex-abbrev <plus>  #\+)
(define-lex-abbrev <minus> #\-)
(define-lex-abbrev <times> #\*)
(define-lex-abbrev <div>   #\/)
(define-lex-abbrev <exp>   #\^)

(define-lex-abbrev <pls_mns>
  (union <plus> <minus>))
(define-lex-abbrev <tms_div>
  (union <times> <div>))

;-------------
; Number defs
;-------------
(define-lex-abbrev <int>
  (union
   (concatenation #\0)
   (concatenation (char-range #\1 #\9)
                  (repetition 0 +inf.0
                              (char-range #\0 #\9)))))
(define-lex-abbrev <pt_float>
  (concatenation (repetition 0 1 <int>)
                 #\.
                 (repetition 1 +inf.0
                             (char-range #\0 #\9))))
(define-lex-abbrev <float_exp>
  (concatenation (union #\e #\E)
                 (repetition 0 1 <pls_mns>)
                 (repetition 1 +inf.0
                             (char-range #\0 #\9))))
(define-lex-abbrev <float>
  (union <pt_float>
         (concatenation (union <int> <pt_float>)
                        <float_exp>)))

(define-lex-abbrev <num>
  (union <int> <float>))

(define-lex-abbrev <var>
  (union (char-range #\a #\z)
         (char-range #\A #\Z)))

;----------------
; LEXER
;----------------
(define-tokens group-a (VAR NUM))
(define-empty-tokens group-b (PLUS MINUS TIMES DIV EXP))
(define-empty-tokens group-c (LPAR RPAR EOF))

(define arith-lexer
  (lexer
   [<num> (token-NUM (string->number lexeme))]
   [<var> (token-VAR (string->symbol lexeme))]
   [<plus>  (token-PLUS)]
   [<minus> (token-MINUS)]
   [<times> (token-TIMES)]
   [<div>   (token-DIV)]
   [<exp>   (token-EXP)]
   [#\( (token-LPAR)]
   [#\) (token-RPAR)]
   [whitespace (arith-lexer input-port)]
   [(eof) (token-EOF)]))

(define arith-parser
  (parser
   (tokens group-a group-b group-c)
   (start Npm-expr)
   (end EOF)
   (error void)
   (precs (left PLUS MINUS) (left TIMES DIV) (left EXP))
   ; Idea -- Two possible states in the tree:
   ;   1) Nblah -- this expression may possibly expand with a leading unary minus
   ;   2) blah  -- this expression CANNOT expand with a leading unary minus
   (grammar
    ; atom ::= NUM
    ;       |  VAR
    ;       |  (Nexpr)
    (atom ((NUM) $1)
          ((VAR) $1)
          ((LPAR Npm-expr RPAR) $2))
    ; Natom ::= atom
    ;        |  -atom
    (Natom ((atom) $1)
           ((MINUS atom) `(- ,$2)))
    
    ; pm-expr ::= pm-expr + td-expr
    ;          |  pm-expr - td-expr
    ;          |  td-expr
    (pm-expr ((pm-expr PLUS  td-expr) `(+ ,$1 ,$3))
             ((pm-expr MINUS td-expr) `(- ,$1 ,$3))
             ((td-expr) $1))
    
    ; td-expr ::= td-expr * exp-expr
    ;          |  td-expr / exp-expr
    ;          |  td-expr exp-expr         (implicit multiplication on adjascent concatenation)
    ;          |  exp-expr
    (td-expr ((td-expr TIMES exp-expr) `(* ,$1 ,$3))
             ((td-expr DIV   exp-expr) `(/ ,$1 ,$3))
             ((td-expr exp-expr) `(* ,$1 ,$2))
             ((exp-expr) $1))
    
    ; exp-expr ::= exp-expr ^ Natom
    ;           |  atom
    (exp-expr ((exp-expr EXP Natom) `(^ ,$1 ,$3))
              ((atom) $1))
    
    ; Npm-expr ::= Npm-expr + td-expr
    ;           |  Npm-expr - td-expr
    ;           |  Ntd-expr
    (Npm-expr ((Npm-expr PLUS  td-expr) `(+ ,$1 ,$3))
              ((Npm-expr MINUS td-expr) `(- ,$1 ,$3))
              ((Ntd-expr) $1))
    
    ; Ntd-expr ::= Ntd-expr * exp-expr
    ;           |  Ntd-expr / exp-expr
    ;           |  Ntd-expr exp-expr         (implicit multiplication on adjascent concatenation)
    ;           |  Nexp-expr
    (Ntd-expr ((Ntd-expr TIMES exp-expr) `(* ,$1 ,$3))
              ((Ntd-expr DIV   exp-expr) `(/ ,$1 ,$3))
              ((Ntd-expr exp-expr) `(* ,$1 ,$2))
              ((Nexp-expr) $1))
    
    ; Nexp-expr ::= Nexp-expr ^ Natom
    ;            |  Natom
    (Nexp-expr ((Nexp-expr EXP Natom) `(^ ,$1 ,$3))
               ((Natom) $1)))))

;-----------------------
; TREE TRANSFORMS
;-----------------------
(define (elim-unary parsed)
  (match parsed
    [`(- ,$1) `(* -1 ,(elim-unary $1))]
    [`(,op ,$1 ,$2) `(,op ,(elim-unary $1) ,(elim-unary $2))]
    [x x]))

(define (elim-minus-div parsed)
  (match parsed
    [`(- ,a ,b) `(+ ,(elim-minus-div a) (* -1 ,(elim-minus-div b)))]
    [`(/ ,a ,b) `(* ,(elim-minus-div a) (^ ,(elim-minus-div b) -1))]
    [`(+ ,a ,b) `(+ ,(elim-minus-div a) ,(elim-minus-div b))]
    [`(* ,a ,b) `(* ,(elim-minus-div a) ,(elim-minus-div b))]
    [`(^ ,a ,b) `(^ ,(elim-minus-div a) ,(elim-minus-div b))]
    [x x]))

(define (tree-crusher parsed)
  (define (crush+ args)
    (foldl (lambda (args acc)
             (match args
               [(? number? v) (append acc (list v))]
               [(? symbol? v) (append acc (list v))]
               [`(+ ,a ...) (append acc a)]
               [x (append acc (list x))]))
           `(+) args))
  (define (crush* args)
    (foldl (lambda (args acc)
             (match args
               [(? number? v) (append acc (list v))]
               [(? symbol? v) (append acc (list v))]
               [`(* ,a ...) (append acc a)]
               [x (append acc (list x))]))
           `(*) args))
      
  (match parsed
    [`(+ ,a ...) (crush+ (map tree-crusher a))]
    [`(* ,a ...) (crush* (map tree-crusher a))]
    [`(^ ,a ,b) `(^ ,(tree-crusher a) ,(tree-crusher b))]
    [x x]))

(define (compress-chains parsed)
  (define (compress+ args)
    (let [(val (foldl (lambda (args acc)
                        (match args
                          [(? number? n) `(,(+ (car acc) n) ,(cadr acc))]
                          [x `(,(car acc) ,(append (cadr acc) (list x)))]))
                      '(0 ()) args))]
      (match val
        [`(0 ,vars) `(+ ,@vars)]
        [`(0 (,x)) x]
        [`(,n ()) n]
        [`(,n ,vars) `(+ ,n ,@vars)])))
  
  (define (compress* args)
    (let [(val (foldl (lambda (args acc)
                        (match args
                          [(? number? n) `(,(* (car acc) n) ,(cadr acc))]
                          [x `(,(car acc) ,(append (cadr acc) (list x)))]))
                      '(1 ()) args))]
      (match val
        [`(1 ,vars) `(* ,@vars)]
        [`(1 (,x)) x]
        [`(,n ()) n]
        [`(,n ,vars) `(* ,n ,@vars)])))
  
  (define (compress^ args)
    (match args
      [`(,(? number? a) ,(? number? b)) (expt a b)]
      [`(,a ,b) `(^ ,a ,b)]))
  
  (match parsed
    [`(+ ,a ...) (compress+ (map compress-chains a))]
    [`(* ,a ...) (compress* (map compress-chains a))]
    [`(^ ,a ...) (compress^ (map compress-chains a))]
    [x x]))

;-------------------
; INPUT YAYAY
;-------------------
(define (str->tree str)
  (let [(input (open-input-string str))]
    (let [(lexed (lambda () (arith-lexer input)))]
      (let [(parsed (arith-parser lexed))]
        (pretty-write parsed)
        (let [(unary-stripped (elim-unary parsed))]
          (pretty-write unary-stripped)
          (let [(min-div-stripped (elim-minus-div unary-stripped))]
            (pretty-write min-div-stripped)
            (let [(crushed (tree-crusher min-div-stripped))]
              (pretty-write crushed)
              (let [(compressed (compress-chains crushed))]
                (pretty-write compressed)))))))))

;--------------
; TESTS
;--------------
(define (lex-it! input) (lambda () (arith-lexer (open-input-string input))))
(define (lex-it!* input)
  (let [(tok (arith-lexer input))]
    (pretty-write tok)
    (when (not (eq? (token-EOF) tok))
        (lex-it!* input))))
(define (parse-it! input) (let [(input (open-input-string input))](arith-parser (lambda () (arith-lexer input)))))