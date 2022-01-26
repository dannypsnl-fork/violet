#lang racket/base
(provide parse-file)

(require parser-tools/yacc
         "lexer.rkt")

(struct v:form (start end) #:transparent)
(struct v:num v:form (num)
  #:transparent)
(struct v:quote v:form (exp)
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
           (sexpr [(NUM) (v:num $1-start-pos $1-end-pos
                                (string->number $1))]
                  [(ID) $1]
                  [(STR) $1]
                  [(|(| sexpr-list |)|)
                   $2]
                  [(|'| sexpr) (v:quote $1-start-pos $1-end-pos
                                        $2)])
           (sexpr-list [(sexpr) (list $1)]
                       [(sexpr sexpr-list) (cons $1 $2)])]))

(define (lex-this lexer input-port)
  (port-count-lines! input-port)
  (lambda () (lexer input-port)))

(define (parse-file file-port)
  (p (lex-this l file-port)))

(module+ test
  (define (parse str)
    (p (lex-this l (open-input-string str))))

  (parse "(1 2 3)")
  (parse "(1 (2 3 4) 5 6 7)")
  (parse "'(1 2 3)")
  (parse "'1")
  (parse "(println \"hello\")")

  )
