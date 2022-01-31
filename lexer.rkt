#lang racket/base
(provide (all-defined-out))

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-empty-tokens keyword
  (:define))
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
   ["define" (token-:define)]
   [(:: (:or (char-set "+-.*/<=>!?:$%_&~^") alphabetic)
        (:* (:or (char-set "+-.*/<=>!?:$%_&~^")
                 numeric
                 alphabetic)))
    (token-ID lexeme)]
   [(:seq "\"" (:* (:~ #\")) "\"")
    (token-STR lexeme)]
   [(:+ numeric)
    (token-NUM lexeme)]
   [whitespace (return-without-pos (l input-port))]))

(define (token-type t)
  (token-name (position-token-token t)))
(define (token-value t)
  (token-value (position-token-token t)))
