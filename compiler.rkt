#lang racket/base
(provide compile-file)

(require racket/match
         "parser.rkt")

(define (compile-file forms)
  (displayln "const std = @import(\"std\");

  pub fn main() !void {
  ")
  (for ([form forms])
    (compile form))
  (displayln "}"))

(define (compile form)
  (match form
    [(v:defvar start end id exp)
     (printf "const ~a = ~a;\n"
             id
             (compile-expr exp))]
    [else (printf "~a;\n" (compile-expr form))]))

(define (compile-expr form)
  (match form
    [(v:num _ _ v) (format "~a" v)]
    [(v:str _ _ s) (format "~v" s)]
    [(v:id _ _ id) (format "~a" id)]
    [else (error 'unsupported-expr "~a" form)]))
