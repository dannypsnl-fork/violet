#lang racket/base
(provide compile-file)

(require racket/match
         syntax/parse)

(define (compile-file forms)
  (displayln "const std = @import(\"std\");

  pub fn main() !void {
  ")
  (for ([form forms])
    (compile form))
  (displayln "}"))

(define (compile form)
  (syntax-parse form
    [(define x:id exp:expr)
     (printf "\nconst ~a = ~a;\n"
             #'x
             (compile-expr #'exp))]
    [else (compile-expr form)]))

(define (compile-expr form)
  (syntax-parse form
    [x:number (format "~v" #'x)]
    [x:str (format "~v" #'x)]
    [x:id (format "~v" #'x)]
    [else (printf "~a" form)]))
