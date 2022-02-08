#lang racket/base
(require racket/match
         racket/function
         racket/system)

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
    [(list "run")
     (system* (find-executable-path "scheme") "--script" "main.ss")]
    [_ (help)]))

(define help
  (thunk (displayln "violet -- a command-line tool for Racket
  run -- run current main file
  --help
  --version")))
(define version
  (thunk (displayln "v0.0.1")))
