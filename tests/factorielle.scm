(define factorielle
   (lambda (n)
      (if (= n 0)
	 1
	 (* n (factorielle (- n 1)))
      )
   )
)

(define n 5)
(display "\nFactorielle de ")
(display n)
(display ": ")
(display (factorielle n))
(display "\n")
