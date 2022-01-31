#lang racket/base
(provide (struct-out form)
         (struct-out num)
         (struct-out id)
         (struct-out str)
         (struct-out quote)
         (struct-out list)
         (struct-out defvar)
         (struct-out lambda))

(struct form (start end) #:transparent)

(struct num form (num)
  #:transparent)

(struct id form (id)
  #:transparent)

(struct str form (str)
  #:transparent)

(struct quote form (exp)
  #:transparent)

(struct list form (lst)
  #:transparent)

(struct defvar form (id exp)
  #:transparent)
(struct lambda form (params body)
  #:transparent)
