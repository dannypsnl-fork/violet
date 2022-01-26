#lang racket/base
(provide lex
         next-token
         peek-token
         token-type)

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         racket/stream
         racket/function)

(define-empty-tokens symbol
  (|(|
   |)|
   |'|
   |,|
   |,@|))
(define-tokens datum
  (ID
   STR
   NUM))
(define-empty-tokens end (EOF))

(define l
  (lexer-src-pos
   [(eof) (token-EOF)]
   ["(" (token-|(|)]
   [")" (token-|)|)]
   ["'" (token-|'|)]
   ["," (token-|,|)]
   [",@" (token-|,@|)]
   [(:+ alphabetic)
    (token-ID lexeme)]
   [(:seq "\"" (:* (:~ #\")) "\"")
    (token-STR lexeme)]
   [(:+ numeric)
    (token-NUM lexeme)]
   [whitespace (return-without-pos (l input-port))]))

(define (tokenize input-port)
  (port-count-lines! input-port)
  (lambda ()
    (l input-port)))

(struct lexer-state
  (tokens)
  #:mutable
  #:transparent)

(define (lex input-port)
  (define s (lexer-state (stream)))
  (define next (tokenize input-port))
  (run s next)
  s)

(define (run s next)
  (let loop ([tok (next)])
    (case (token-type tok)
      [(EOF) (set-lexer-state-tokens! s (stream-append (lexer-state-tokens s) (stream tok))) ]
      [else (set-lexer-state-tokens! s (stream-append (lexer-state-tokens s) (stream tok)))
            (run s next)])))

(define (peek-token s)
  (stream-first (lexer-state-tokens s)))
(define (next-token s)
  (define tok (peek-token s))
  (case (token-type tok)
    [(EOF) (void)]
    [else (set-lexer-state-tokens! s (stream-rest (lexer-state-tokens s)))])
  tok)

(define (token-type t)
  (token-name (position-token-token t)))
