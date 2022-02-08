#lang racket/base
(require racket/match
         racket/function
         racket/system
         racket/file)

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "violet"
   #:args args
   (cli args)))

(define config-dir (build-path (find-system-path 'home-dir)
                               ".violet"))
(define bin-dir (build-path config-dir "bin"))
(define src-dir (build-path config-dir "src"))
(define lib-dir (build-path config-dir "lib"))

(define (init-global)
  (make-directory* bin-dir)
  (make-directory* lib-dir)
  (make-directory* src-dir))

(define (cli args)
  (init-global)
  (match args
    [(list "--help") (help)]
    [(list "--version") (version)]
    [(list "run")
     (println (read (open-input-file "violet.ss")))
     (system* (find-executable-path "scheme") "--script" "main.ss")]
    [_ (help)]))

(define help
  (thunk (displayln "violet -- a command-line tool for Racket
  run -- run current main file
  --help
  --version")))
(define version
  (thunk (displayln "v0.0.1")))
