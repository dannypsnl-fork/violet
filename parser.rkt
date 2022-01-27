#lang racket/base
(provide parse-file
         v:form
         v:num
         v:id
         v:str
         v:quote
         v:list
         v:defvar
         v:lambda)

(require (only-in parser-tools/lex
                  position-line
                  position-col)
         parser-tools/yacc
         racket/match
         racket/list
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
  (v:quote (make-pos start) (make-pos end) l))

(struct v:list v:form (lst)
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "~a" (v:list-lst v)))
  #:transparent)
(define (make-v:list start end lst)
  (v:list (make-pos start) (make-pos end)
          lst))

(struct v:defvar v:form (id exp)
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "(define ~a ~a)" (v:defvar-id v)
             (v:defvar-exp v)))
  #:transparent)
(struct v:lambda v:form (params body)
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "(lambda ~a ~a)"
             (v:lambda-params v)
             (v:lambda-body v)))
  #:transparent)

(define p
  (parser [start sexpr-list]
          [end EOF]
          [error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
                   (printf "~a-~a: tok-ok?: ~a, token: ~a ~a\n"
                           (make-pos start-pos) (make-pos end-pos)
                           tok-ok? tok-name tok-value))]
          [src-pos]
          [tokens symbol datum end]
          [grammar
           (sexpr [(NUM) (make-v:num $1 $1-start-pos $1-end-pos)]
                  [(ID) (make-v:id $1 $1-start-pos $1-end-pos)]
                  [(STR) (make-v:str $1 $1-start-pos $1-end-pos)]
                  [(|(| sexpr-list |)|) (make-v:list $1-start-pos $3-end-pos $2)]
                  [(|'| sexpr) (make-v:quote $2 $1-start-pos $1-end-pos)])
           (sexpr-list [(sexpr) (list $1)]
                       [(sexpr sexpr-list) (cons $1 $2)])]))

(define (lex-this lexer input-port)
  (port-count-lines! input-port)
  (lambda () (lexer input-port)))

(define (parse-file file-port)
  (define origin-forms (p (lex-this l file-port)))
  (for/list ([f origin-forms])
    (transform-builtin f)))

(define (transform-builtin f)
  (match f
    [(? v:num?) f]
    [(? v:str?) f]
    [(? v:id?) f]
    [(? v:quote?) f]
    [(v:list start end (list (? v:id?) ; define
                             (v:id _ _ name) ; var
                             exp))
     #:when (equal? (v:id-id (first (v:list-lst f)))
                    'define)
     (v:defvar start end name exp)]
    [(v:list start end (list (? v:id?) ; define
                             (v:list _ _ (list (? v:id?) ...)) ; name+params
                             body ...))
     #:when (equal? (v:id-id (first (v:list-lst f)))
                    'define)
     (v:defvar start end
               (first (v:list-lst (second (v:list-lst f)))) ; name
               (v:lambda (v:form-start (first (v:list-lst f))) end
                         (rest (v:list-lst (second (v:list-lst f)))) ; params
                         body))]
    [(v:list start end (list (? v:id?) rest ...))
     #:when (equal? (v:id-id (first (v:list-lst f)))
                    'define)
     (printf "~a-~a: invalid `define` form\n  ~a\n"
             start end
             f)]
    [else f]))

(module+ test
  (define (parse str)
    (p (lex-this l (open-input-string str))))

  (parse "(1 2 3)")
  (parse "(1 (2 3 4) 5 6 7)")
  (parse "'(1 2 3)")
  (parse "'1")
  (parse "(println \"hello\")")

  )
