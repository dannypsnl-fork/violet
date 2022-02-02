#lang racket/base
(provide compile-file)

(require racket/match
         "ast.rkt")

(define (compile-file forms)
  (displayln "const std = @import(\"std\");

  pub fn main() !void {
  ")
  (for ([form forms])
    (compile form))
  (displayln "}"))

(define (compile form)
  (match form
    [(struct* defvar ([id id]
                      [exp exp]))
     (printf "const ~a = ~a;\n"
             id
             (compile-expr exp))]
    [else (printf "~a;\n" (compile-expr form))]))

(define (compile-expr form)
  (match form
    [(struct* num ([num v])) (format "~a" v)]
    [(struct* str ([str s])) (format "~v" s)]
    [(struct* id ([id id])) (format "~a" id)]
    [else (printf "~a" form)]))
