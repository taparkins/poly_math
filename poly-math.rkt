#lang racket

(require "poly-parser.rkt")

; Objective of this file:
; -----------------------
;  1) Provide functions for basic operations between polynomials on the Reals:
;   - Addition / Subtraction
;   - Multiplication
;   - Division with remainder
;   - ???
;
;  2) Provide intro to building this system for work over more general fields (with custom + and * opers provided)
;
;  3) Generally be an amusing side project
;
;
; Abstraction of a polynomial:
; ----------------------------
;  Polynomials shall be represented as a list of tuples, sorted by their second component.
; These tuples are interpreted as (a b), where a is the coefficient of the x term with
; degree b. If a == 0, the tuple is assumed to be excluded (but everything should work
; regardless).
;
;  So, as an example:
;   x^3 + 2x^4 - 1 --> ((-1 0) (1 3) (2 4))
;
;  Overloaded versions of these functions will be provided allowing the user to provide
; string versions of polynomials rather than these ordered lists of tuples garbage. This is
; all accomplished through the awesome poly-paser.rkt file I already wrote. :)

(define (add-polys p1 p2)
  (define (add-polys* p1* p2* acc)
    (cond
      [(eq? p1* '()) (append acc p2*)]
      [(eq? p2* '()) (append acc p1*)]
      [else
       (let [(a (car p1*)) (b (car p2*))]
         (cond
           [(eq? (cadr a) (cadr b))
            (add-polys* (cdr p1*) (cdr p2*) (append acc (list `(,(+ (car a) (car b)) ,(cadr b)))))]
           [(> (cadr a) (cadr b))
            (add-polys* p1* (cdr p2*) (append acc (list b)))]
           [else ;(< (cadr a) (cadr b))
            (add-polys* (cdr p1*) p2* (append acc (list a)))]))]))
  (elim-zero-coefficients (add-polys* p1 p2 '())))

(define (mult-polys p1 p2)
  (define (mult-polys* p1* acc)
    (define (mult-polys** term p2** acc*)
      (if (eq? p2** '())
          acc*
          (mult-polys** term 
                        (cdr p2**) 
                        (append acc* (list `(,(* (car term) (caar p2**))
                                             ,(+ (cadr term) (cadar p2**))))))))
    (if (eq? p1* '())
        acc
        (mult-polys* (cdr p1*)
                     (append acc (mult-polys** (car p1*) p2 '())))))
  (sort
   (elim-zero-coefficients
    (sum-powers (mult-polys* p1 '())))
   poly<))

(define (sub-polys p1 p2)
  (add-polys p1
             (mult-polys `((-1 0)) p2)))

; Returns two values: q, r where p1 = (p2 * q) + r
(define (div-polys p1 p2)
  (let [(p1 (sort p1 poly>))
        (p2 (sort p2 poly>))]
    (define (long-div p1* acc)
      (cond
        [(eq? p1* '()) (values acc '())]
        [(poly> (car p2) (car p1*)) (values acc p1*)]
        [else
         (let [(term `(,`(,(* (/ 1.0 (caar p2)) (caar p1*))
                          ,(- (cadar p1*) (cadar p2)))))]
           (long-div (sub-polys p1*
                               (mult-polys term p2))
                     (append acc term)))]))
    (let-values ([(q r) (long-div p1 '())])
      (values
       (sort q poly<)
       (sort (elim-zero-coefficients (sum-powers r)) poly<)))))

; Returns q where p1 = (p2 * q) + r
(define (quot-polys p1 p2)
  (let-values ([(q r) (div-polys p1 p2)])
    q))

; Returns r where p1 = (p2 * q) + r
(define (mod-polys p1 p2)
  (let-values ([(q r) (div-polys p1 p2)])
    r))

; Displays a polynomial in a nice, string way
(define (poly->str p)
  ; Get string for x term with given power
  (define (var->str pow)
    (cond
      [(= pow 0) ""]
      [(= pow 1) "x"]
      [else (string-append "x^" (number->string pow))]))
  ; Get string for coefficient
  (define (coef->str coef pow)
    (if (and (= 1 coef) (not (= 0 pow)))
        ""
        (number->string coef)))
  ; Get trailing terms for string (e.g. after fencepost)
  (define (poly->str* p* acc)
    ; Get string for given polynomial term
    (define (term->str p-term)
      (let [(sign (if (< (car p-term) 0) "-" "+"))
            (coef (abs (car p-term)))
            (pow (cadr p-term))]
        (let [(str-var (var->str pow))
              (str-coef (coef->str coef pow))]
          (string-append " " sign " " str-coef str-var))))
    (if (eq? p* '())
        acc
        (poly->str* (cdr p*) (string-append acc (term->str (car p*))))))
  ; Fencepost!
  (let [(term (car p))]
    (let [(coef (car term))
          (pow (cadr term))]
      (poly->str* (cdr p) (string-append (coef->str coef pow) (var->str pow))))))
       