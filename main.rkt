#lang racket/base
(require racket/match
         racket/function
         racket/system
         racket/file
         racket/list
         net/url)

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "violet"
   #:args args
   (cli args)))

(define chezscheme (find-executable-path "scheme"))

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
    ['("--help") (help)]
    ['("--version") (version)]
    ['("repl") (system* chezscheme)]
    [`("install" ,uri-string)
     (match-define (struct* url
                            ([scheme scheme]
                             [host host]
                             [path path]))
       (string->url uri-string))
     (define paths (map path/param-path path))
     (define installed-path (apply build-path (list* src-dir host paths)))
     (when (equal? host "github.com")
       (system* (find-executable-path "git")
                "clone"
                uri-string
                installed-path))
     (void)]
    ['("run")
     (println (read (open-input-file "violet.ss")))
     (system* chezscheme "--script" "main.ss")
     (void)]
    [_ (help)]))

(define help
  (thunk (displayln "violet -- a command-line tool for Racket
  run -- run current main file
  --help
  --version")))
(define version
  (thunk (displayln "v0.0.1")))
