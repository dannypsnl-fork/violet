#lang racket/base
(provide parse-file)

(require (only-in parser-tools/lex
                  position-line
                  position-col)
         parser-tools/yacc
         "lexer.rkt")

(struct pos (line column)
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "~v:~v"
             (pos-line v) (pos-column v)))
  #:transparent)
(define (make-pos tok-p)
  (pos (position-line tok-p) (position-col tok-p)))

(struct v:form (start end) #:transparent)

(struct v:num v:form (num)
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "~a" (v:num-num v)))
  #:transparent)
(define (make-v:num str start end)
  (v:num (make-pos start) (make-pos end)
         (string->number str)))

(struct v:id v:form (id)
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "~a" (v:id-id v)))
  #:transparent)
(define (make-v:id str start end)
  (v:id (make-pos start) (make-pos end)
        (string->symbol str)))

(struct v:str v:form (str)
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "~a" (v:str-str v)))
  #:transparent)
(define (make-v:str str start end)
  (v:str (make-pos start) (make-pos end)
         str))

(struct v:quote v:form (exp)
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "(quote ~a)" (v:quote-exp v)))
  #:transparent)
(define (make-v:quote l start end)
  (v:quote (make-pos start) (make-pos end)
           l))

(define p
  (parser [start sexpr-list]
          [end EOF]
          [error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
                   (printf "tok-ok?: ~a, token: ~a ~a, position: ~a ~a\n"
                           tok-ok? tok-name tok-value start-pos end-pos))]
          [src-pos]
          [tokens symbol datum end]
          [grammar
           (sexpr [(NUM) (make-v:num $1 $1-start-pos $1-end-pos)]
                  [(ID) (make-v:id $1 $1-start-pos $1-end-pos)]
                  [(STR) (make-v:str $1 $1-start-pos $1-end-pos)]
                  [(|(| sexpr-list |)|)
                   $2]
                  [(|'| sexpr) (make-v:quote $2 $1-start-pos $1-end-pos)])
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
