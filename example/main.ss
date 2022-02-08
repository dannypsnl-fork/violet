(define foo 1)

(define (hello x y z)
  (printf "~a~n"
          (list "hello"
                 x y z foo)))

(hello 1 2 3)
