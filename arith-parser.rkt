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
(define-empty-tokens group-b (PLUS MINUS TIMES DIV))
(define-empty-tokens group-c (LPAR RPAR DIV))

(define arith-lexer
  (lexer
   [<num> (token-NUM (string->number lexeme))]
   [<var> (token-VAR (string->symbol lexeme))]
   [<plus>  (token-PLUS)]
   [<minus> (token-MINUS)]
   [<times> (token-TIMES)]
   [<div>   (token-DIV)]
   [#\( (token-LPAR)]
   [#\) (token-RPAR)]
   [whitespace (arith-lexer input-port)]
   [(eof) (token-EOF)]))

(define arith-parser
  (parser
   (tokens group-a group-b)
   (start expr)
   (end EOF)
   (error void)
   (precs (left TIMES DIV) (left PLUS MINUS))
   (grammar
    ; expr ::= expr op expr
    ;       |  num
    (expr ((expr PLUS expr)  `(+ ,$1 ,$3))
          ((expr MINUS expr) `(- ,$1 ,$3))
          ((expr TIMES expr) `(* ,$1 ,$3))
          ((expr DIV expr)   `(/ ,$1 ,$3))
          ((NUM) $1)
          ((VAR) $1)))))

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