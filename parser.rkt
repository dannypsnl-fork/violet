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

(define target "test.ss")
(parameterize ([file-path target]
               [current-input-port (open-input-file target)])
  (define (next) (l (current-input-port)))

  (let loop ([token (next)])
    (case (token-name (position-token-token token))
      [(EOF) (void)]
      [else (println token)
            (loop (next))]))
  )
