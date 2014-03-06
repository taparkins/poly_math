#lang racket
(require "arith-parser.rkt")

; Generates a random arithmetic expression to be parsed by
; the system in arith-parser.rkt. 'size' is fairly nebulous,
; but it loosely corresponds to the number of terms in the
; expression. Somewhere between 5 and 10 is pretty small,
; whereas larger than 100 gets very noticeable.
;
; This operation can optionally be seeded, allowing for more
; control in fuzz testing. By default, it is seeded by
; (current-milliseconds).
(define (gen-arith-expr size [seed (current-milliseconds)])
  (random-seed (modulo seed (expt 2 31)))
  (define (gen-arith-expr* size)
    ; Idea: we'll generate some sort of operation, and recur with
    ; splits of the size on each operand. 
    
    ; Helper: generate variable
    (define (gen-var index)
      (match index
        [0  'a][1  'b][2  'c][3  'd][4  'e][5  'f]
        [6  'g][7  'h][8  'i][9  'j][10 'k]
        [11 'l][12 'm][13 'n][14 'o][15 'p]
        [16 'q][17 'r][18 's][19 't][20 'u]
        [21 'v][22 'w][23 'x][24 'y][25 'z]))
    
    ; If our size is sufficiently small, return a number, a variable, or nothing:
    (match size
      [0 '0]
      [1
       ; =>
       (match (random 2)
         ;[0 (number->string (random))] ; number
         [0 (number->string (random 100))] ; integer
         ;[2 (symbol->string (gen-var (random 26)))] ; variable
         [1 (symbol->string (gen-var (random 3)))] ; variable
         [_ "0"])]
      
      ; Pick an operation:
      [_
       ; =>
       (let* ([left-len (floor (/ size 2))]
              [right-len (- size left-len)])
         (match (random 5)
           [0 (string-append (gen-arith-expr* left-len) "+" (gen-arith-expr* right-len))] ; +
           [1 (string-append (gen-arith-expr* left-len) "-" (gen-arith-expr* right-len))] ; -
           [2 (string-append (gen-arith-expr* left-len) "*" (gen-arith-expr* right-len))] ; *
           [3 (string-append (gen-arith-expr* left-len) "/" (gen-arith-expr* right-len))] ; /
           [4 (string-append "(" (gen-arith-expr* left-len) ")^(" (gen-arith-expr* right-len) ")")] ; ^
           ))]))
  (gen-arith-expr* size))

(let ([expr (gen-arith-expr 10)])
  (display expr)
  (str->tree expr))