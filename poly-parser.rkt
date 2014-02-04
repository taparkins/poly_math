#lang racket
(require parser-tools/lex)
(require parser-tools/yacc)

;-----------
; Oper defs
;-----------

(define-lex-abbrev <pls_mns>
  (union #\+ #\-))

(define-lex-abbrev <plus>  #\+)
(define-lex-abbrev <minus> #\-)

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

;-----------
; Var defs
;-----------

(define-lex-abbrev <exp>
  (concatenation #\^ <num>))
(define-lex-abbrev <var>
  #\x)

;===========
; LEXER
;===========
; Token definitions - required by parser
(define-tokens group-a (VAR EXP NUM))
(define-empty-tokens group-b (PLUS MINUS EOF))

(define poly_lexer
  (lexer
   [<var> (token-VAR #\x)]
   [<exp> (token-EXP (string->number (substring lexeme 1)))] ; we trim the carat for convenience later
   [<num> (token-NUM (string->number lexeme))]
   [<plus> (token-PLUS)]
   [<minus> (token-MINUS)]
   [whitespace (poly_lexer input-port)]   ; ignore whitespace
   [(eof) (token-EOF)]))

;===========
; PARSER
;===========

(define poly_parser
  (parser
   (tokens group-a group-b)
   (start poly)
   (end EOF)
   (error void)
   (grammar
    ; <var-term> ::= VAR
    ;             |  VAR EXP
    (var-term ((VAR) 1)
              ((VAR EXP) $2))
    ; <base-term> ::= NUM
    ;              |  var-term
    ;              |  NUM var-term
    (base-term ((NUM) `(,$1 0))
               ((var-term) `(1 ,$1))
               ((NUM var-term) `(,$1 ,$2)))
    ; <sign-term> ::= <base-term>
    ;              |  <minus> <base-term>
    (sign-term ((base-term) `(+ ,$1))
               ((MINUS base-term) `(- ,$2)))
    ; <poly-term> ::= <pls_mns> <base-term>
    (poly-term ((PLUS base-term) `(+ ,$2))
               ((MINUS base-term) `(- ,$2)))
    ; <poly*> ::= <poly-term>
    ;          |  <poly-term> <poly*>
    (poly* ((poly-term) `(,$1))
           ((poly-term poly*) (cons $1 $2)))
    ; <poly> ::= <sign-term>
    ;         |  <sign-term> <poly*> 
    (poly ((sign-term poly*) (cons $1 $2))
          ((sign-term) `(,$1))))))

;======================
; TREE TRANSFORMS
;======================
(define (compress-signs parsed)
  (define (compress-signs* parsed* acc)
    (if (eq? parsed* '())
        acc
        (let [(translated 
               (match (car parsed*)
                 [`(+ (,$1 ,$2)) `(,$1 ,$2)]
                 [`(- (,$1 ,$2)) `(,(- $1) ,$2)]
                 [else void]))]
          (compress-signs* (cdr parsed*) (append acc `(,translated))))))
  (compress-signs* parsed '()))

(define (sum-powers parsed)
  (define (sum-powers* remaining acc)
    (if (eq? remaining '())
        acc
        (let [(elem (car remaining))]
          (define (merge remaining acc)
            (cond
              [(eq? remaining '()) (append acc `(,elem))]
              [(eq? (cadar remaining) (cadr elem))
               (append
                (cdr remaining)
                `(,`(,(+ (caar remaining) (car elem)) ,(cadr elem)))  ; Oh god the quasiquotes D:
                acc)]
              [else 
               (merge (cdr remaining) (append acc `(,(car remaining))))]))
          (sum-powers* (cdr remaining) (merge acc '())))))
  (sum-powers* parsed '()))

(define (elim-zero-coefficients parsed)
  (define (elim-zero-coefficients* remaining acc)
    (if (eq? remaining '())
        acc
        (let [(check (car remaining))]
          (if (= (car check) 0)
              (elim-zero-coefficients* (cdr remaining) acc)
              (elim-zero-coefficients* (cdr remaining) (append acc `(,check)))))))
  (elim-zero-coefficients* parsed '()))

;=============================
; MAIN ACCESS FUNCTION
;=============================
(define (str->poly string)
  (let [(input (open-input-string string))]
    (let [(lexed (lambda () (poly_lexer input)))]
      (let ()
        (define (lex-it!* input)
          (let [(tok (poly_lexer input))]
            (pretty-write tok)
            (when (not (eq? (token-EOF) tok))
              (lex-it!* input))))
        ;(lex-it!* (open-input-string string))
        (let [(parsed (poly_parser lexed))]
          ;(pretty-write parsed)
          (let [(sign-compressed (compress-signs parsed))]
            ;(pretty-write sign-compressed)
            (let [(power-summed (sum-powers sign-compressed))]
              ;(pretty-write power-summed)
              (let [(zero-elimmed (elim-zero-coefficients power-summed))]
                ;(pretty-write zero-elimmed)
                (sort
                 zero-elimmed
                 poly<)))))))))
  
 (define (poly< a b)
   (< (cadr a) (cadr b)))
 
 (define (poly> a b)
   (> (cadr a) (cadr b)))

(provide str->poly
         compress-signs
         sum-powers
         elim-zero-coefficients
         poly<
         poly>)

;===========
; INPUT
;===========
;(define (lex-it! lexer input) (lambda () (lexer input)))
;(define (lex-it!* lexer input)
;  (let [(tok (lexer input))]
;    (pretty-write tok)
;    (when (not (eq? (token-EOF) tok))
;        (lex-it!* lexer input))))
;(define lexed (lex-it! poly_lexer (open-input-string "x^2 -3x + 2x + x + 1")))
;(lex-it!* poly_lexer (open-input-string "1"))
;(let [(parsed (poly_parser lexed))]
;  (pretty-write parsed)
;  (let [(sign-compressed (compress-signs parsed))]
;    (pretty-write sign-compressed)
;    (let [(power-summed (sum-powers sign-compressed))]
;      (pretty-write power-summed)
;      (let [(zero-elimmed (elim-zero-coefficients power-summed))]
;        (pretty-write zero-elimmed)))))