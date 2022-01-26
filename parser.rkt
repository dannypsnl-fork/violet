#lang racket/base
(require (rename-in "lexer.rkt"
                    [peek-token peek]
                    [next-token next]))

(define (parse-sexpr s)
  (define tok (next s))
  (case (token-type tok)
    [(|(|)
     (take-till '|)| s)])
  )

(define (take-till tok-type s)
  (let loop ([r (list)]
             [tok (next s)])
    (cond
      [(not (equal? tok-type (token-type tok)))
       (loop (append r (list tok))
             (next s))]
      [(equal? '|(| (token-type tok))
       (loop (append r (list (parse-sexpr s)))
             (next s))]
      [else r])))

(module+ test
  (require rackunit)

  (define s (lex (open-input-string "(1 2 3)")))
  (check-equal? (length (parse-sexpr s))
                3)

  (set! s (lex (open-input-string "(1 2 3)")))
  (parse-sexpr s)

  )
