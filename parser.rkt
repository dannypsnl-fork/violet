#lang racket/base
(provide parse-file)

(require parser-tools/yacc
         "lexer.rkt"
         (prefix-in v: "ast.rkt"))

(define p
  (parser [start sexpr-list]
          [end EOF]
          [error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
                   (printf "~a-~a: tok-ok?: ~a, token: ~a ~a\n"
                           start-pos end-pos
                           tok-ok? tok-name tok-value))]
          [src-pos]
          [tokens keyword symbol datum end]
          [grammar
           (sexpr [(NUM) (v:num $1-start-pos $1-end-pos $1)]
                  [(ID) (v:id $1-start-pos $1-end-pos $1)]
                  [(STR) (v:str $1-start-pos $1-end-pos $1)]
                  [(|(| :define ID sexpr |)|) (v:defvar $1-start-pos $5-end-pos $3 $4)]
                  [(|(| sexpr-list |)|) (v:list $1-start-pos $3-end-pos $2)]
                  [(|'| sexpr) (v:quote $1-start-pos $1-end-pos $2 )])
           (sexpr-list [(sexpr) (list $1)]
                       [(sexpr sexpr-list) (cons $1 $2)])]))

(define (lex-this lexer input-port)
  (port-count-lines! input-port)
  (lambda () (lexer input-port)))

(define (parse-file file-port)
  (define origin-forms (p (lex-this l file-port)))
  (for/list ([f origin-forms])
    f))

(module+ test
  (define (parse str)
    (p (lex-this l (open-input-string str))))

  (parse "(1 2 3)")
  (parse "(1 (2 3 4) 5 6 7)")
  (parse "'(1 2 3)")
  (parse "'1")
  (parse "(println \"hello\")")
  (parse "(define foo (+ 1 2 3))")

  )
