#lang racket/base
(require parser-tools/yacc
         "lexer.rkt")

(struct v:num (num)
  #:transparent)

(define p
  (parser [start sexpr-list]
          [end EOF]
          [error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
                   (printf "tok-ok?: ~a, token: ~a ~a, position: ~a ~a\n"
                           tok-ok? tok-name tok-value start-pos end-pos))]
          [src-pos]
          [tokens symbol datum end]
          [grammar
           (sexpr [(NUM) $1]
                  [(ID) $1]
                  [(STR) $1]
                  [(|(| sexpr-list |)|)
                   $2]
                  [(|'| sexpr) (cons 'quote $2)])
           (sexpr-list [(sexpr) (list $1)]
                       [(sexpr sexpr-list) (cons $1 $2)])]))

(module+ test
  (define (lex-this lexer input-port)
    (port-count-lines! input-port)
    (lambda () (lexer input-port)))
  (define (parse str)
    (p (lex-this l (open-input-string str))))

  (parse "(1 2 3)")
  (parse "(1 (2 3 4) 5 6 7)")
  (parse "'(1 2 3)")
  (parse "'1")
  (parse "(println \"hello\")")

  )
