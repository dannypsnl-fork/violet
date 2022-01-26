#lang racket/base
(require racket/match
         racket/function)

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "violet"
   #:args args
   (cli args)))

(define (cli args)
  (match args
    [(list "--help") (help)]
    [(list "--version") (version)]
    [_ (help)]))

(define help
  (thunk (displayln "violet -- a command-line tool for Racket")))
(define version
  (thunk (displayln "v0.0.1")))
