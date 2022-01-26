#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-empty-tokens symbol
  (|(|
   |)|
   |'|))
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
   [(:+ alphabetic)
    (token-ID lexeme)]
   [(:seq "\"" (:* (:~ #\")) "\"")
    (token-STR lexeme)]
   [(:+ numeric)
    (token-NUM lexeme)]
   [whitespace (return-without-pos (l input-port))]))

(define (tokenize input-port)
  (lambda ()
    (l input-port)))

(module+ test
  (require rackunit)

  (define (port->tokenlist input-port)
    (port-count-lines! input-port)
    (define next (tokenize input-port))
    (let loop ([token (next)]
               [tokenlist (list)])
      (case (token-name (position-token-token token))
        [(EOF) tokenlist]
        [else (loop (next)
                    (append tokenlist (list token)))])))

  (define (string->tokenlist string)
    (port->tokenlist (open-input-string string)))

  (check-equal? (length (string->tokenlist "'(1 2 3)"))
                6)

  )
