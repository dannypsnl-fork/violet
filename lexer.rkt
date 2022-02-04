#lang racket/base
(provide violet-lexer)

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         racket/string)

(define-empty-tokens symbol
  (|(|
   |)|
   |'|
   |,|
   |,@|))
(define-tokens datum
  (IDENTIFIER
   STRING
   NUMBER))

(define violet-lexer
  (lexer-src-pos
   [(eof) eof]
   [(:or whitespace blank iso-control) (void)]
   ["(" (token-|(|)]
   [")" (token-|)|)]
   ["'" (token-|'|)]
   ["," (token-|,|)]
   [",@" (token-|,@|)]
   [(:: (:or (char-set "+-.*/<=>!?:$%_&~^") alphabetic)
        (:* (:or (char-set "+-.*/<=>!?:$%_&~^")
                 numeric
                 alphabetic)))
    (token-IDENTIFIER (string->symbol lexeme))]
   [(:+ numeric)
    (token-NUMBER (string->number lexeme))]
   [(:seq "\"" (:* (:~ #\")) "\"")
    (token-STRING (string-trim lexeme
                               "\""))]))
