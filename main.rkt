#lang racket/base
(require racket/match
         racket/function
         "parser.rkt")

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "violet"
   #:args args
   (cli args)))

(define (cli args)
  (match args
    [(list "parse" filename)
     (parse-file (open-input-file filename))]
    [(list "--help") (help)]
    [(list "--version") (version)]
    [_ (help)]))

(define help
  (thunk (displayln "violet -- a command-line tool for Racket")))
(define version
  (thunk (displayln "v0.0.1")))
