#lang racket/base
(require "lexer.rkt")

(define (parse-sexpr next)
  (define tok (next))
  (case (token-type tok)
    [(|(|)
     (take-till '|)| next)])
  )

(define (take-till tok-type next)
  (let loop ([r (list)]
             [tok (next)])
    (cond
      [(not (equal? tok-type (token-type tok)))
       (loop (append r (list tok))
             (next))]
      [(equal? '|(| (token-type tok))
       (loop (append r (list (parse-sexpr)))
             (next))]
      [else r])))

(module+ test
  (require rackunit)

  (define (port->tokenlist input-port)
    (define next (tokenize input-port))
    (let loop ([token (next)]
               [tokenlist (list)])
      (case (token-type token)
        [(EOF) tokenlist]
        [else (loop (next)
                    (append tokenlist (list token)))])))

  (define (string->tokenlist string)
    (port->tokenlist (open-input-string string)))

  (check-equal? (length (string->tokenlist "'(1 2 3)"))
                6)

  (define next (tokenize (open-input-string "(1 2 3)")))
  (parse-sexpr next)

  )
