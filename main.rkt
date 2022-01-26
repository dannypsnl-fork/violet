#lang racket/base
(require racket/match
         racket/function
         "parser.rkt"
         "compiler.rkt")

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
    [(list "parse" filename)
     (parse-file (open-input-file filename))]
    [(list filename)
     (compile-file (parse-file (open-input-file filename)))]
    [_ (help)]))

(define help
  (thunk (displayln "violet -- a command-line tool for Racket")))
(define version
  (thunk (displayln "v0.0.1")))
