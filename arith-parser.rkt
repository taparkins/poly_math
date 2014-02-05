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
   (start op-expr)
   (end EOF)
   (error void)
   (precs (left PLUS MINUS) (left TIMES DIV) (left EXP))
   (grammar
    ; atom ::= NUM
    ;       |  VAR
    (atom ((NUM) $1)
          ((VAR) $1))
    ; op-expr ::= op-expr OP op-expr
    ;          |  -expr [with some precedence setup]
    ;          |  par-expr
    ;          |  atom
    (op-expr ((op-expr PLUS  op-expr) `(+ ,$1 ,$3))
             ((op-expr MINUS op-expr) `(- ,$1 ,$3))
             ((op-expr TIMES op-expr) `(* ,$1 ,$3))
             ((op-expr DIV   op-expr) `(/ ,$1 ,$3))
             ((op-expr EXP   op-expr) `(^ ,$1 ,$3))
             ((MINUS op-expr) (prec TIMES) `(- ,$2))  ; Effectively treated as (* -1 op-expr), so precedence equals *
             ((par-expr) $1)
             ((atom) $1))
    ; par-expr ::= (expr)
    (par-expr ((LPAR op-expr RPAR) $2)))))

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