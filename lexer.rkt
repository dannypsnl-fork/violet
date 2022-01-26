#lang racket/base
(provide tokenize
         token-type)

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

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

(define (token-type t)
  (token-name (position-token-token t)))
