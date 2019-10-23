#|
;; Affichage
(define n 5)
(display "\nFactorielle de ")
(display n)
(display ": ")
(display (factorielle n))
(display "\n")
|#

;; Ceci est mon programme du TP 1
;; 1- quad
(define quad
  (lambda (n)
    (* n (* n (* n n)))
  )
)

;; 2- cercle
(define cercle
  (lambda (n)
    (list (* 2 n 3.14)
	  (* n n 3.14)
	  )
    )
  )

;; 3- fac
(define factorielle
  (lambda (n)
    (if (= n 0)
      1
      (* n (factorielle (- n 1)))
      )
    )
  )

;; 4- som_int
(define som_int
  (lambda (n)
    (if (= n 1)
      1
      (+ n (som_int (- n 1)))
      )
    )
  )

;; 5- long
(define long
  (lambda (L)
    (if (null? L)
      0
      (+ 1 (long (cdr L)))
      )
    )
  )

;; 6- mirroir
(define mirroir
  (lambda (L)
    (if (null? L)
      '()		  ;; Liste nulle (non concatenee)
      (append		  ;; On concatene
	(mirroir (cdr L)) ;; Le mirroir de la queue de liste
	(list (car L))    ;; La tete de liste
	)
      )
    )
  )

;; 7- carre
(define carre
  (lambda (L)
    (if (null? L)
      '()
      (append
	(list (* (car L) (car L)))  ;; Le carre de la tete de liste
	(carre (cdr L))		    ;; Le carre de la queue de liste
	)
      )
    )
  )

;; 8- nbPos
(define nbPos
  (lambda (L)
    (if (null? L)
      0
      (if (>= (car L) 0)
	(+ 1 (nbPos (cdr L)))
	(nbPos (cdr L))
	)
      )
    )
  )

;; 9- membre
(define membre 
  (lambda (x L1)
    (if (null? L1)
      #f 
      (if (= x (car L1))
	#t
	(membre x (cdr L1))
      )
    )
  )
)

;; 10- epure
(define epure
  (lambda (L)
    (if (null? L)
      L  ;; ou '()
      (let ((AR (epure (cdr L))))
	(if (membre (car L) AR)
	  AR
	  (cons (car L) AR)
	)
      )
    )
  )
)

#|
;; 10.2- epure
(define epure
  (lambda (L)
    (if (null? L)
      L
      (if (membre (car L) (cdr L))
	(epure (cdr L))
	(cons (car L) (epure (cdr L)))
      )
    )
  )
)
|#

;; 11- nieme
(define nieme
  (lambda (n L)
    (if (null? L)
      -1
      (if (= n 1)
	(car L)
	(nieme (- n 1) (cdr L))
      )
    )
  )
)

;; 12- inserer
(define inserer
  (lambda (n x L)
    (if (= n 1)
      (cons x L)
      (if (null? L)
	L
	(cons (car L) (inserer (- n 1) x (cdr L)))
      )
    )
  )
)

;; 13- union
(define union
  (lambda (L1 L2)
    (epure (append L1 L2))
  )
)

;; 14- inter
(define inter
  (lambda (L1 L2)
    (if (null? L1)
      '() 
      (epure
        (let ((AR (inter (cdr L1) L2)))
	  (if (membre (car L1) L2)
	    (cons (car L1) AR)
	    AR
	  )
	)
      )
    )
  )
)

;; 15- niv0
(define niv0
  (lambda (L)
    (if (null? L)
      L
      (if (list? (car L))
	(append (niv0 (car L)) (niv0 (cdr L)))
	(cons (car L) (niv0 (cdr L)))
      )
    )
  )
)

;; 16- zip 
(define zip
  (lambda (L1 L2)
    (if (null? L1)
      L2
      (if (null? L2)
	L1
	(cons 
	  (list (car L1) (car L2))
	  (zip (cdr L1) (cdr L2))
	)
      )
    )
  )
)

;; 17- prod
;; Sous fonction
;; x: element a matcher
;; L: liste avec laquelle on match x
;; retour: liste des matchs
(define match
  (lambda (x L)
    (if (null? L)
      L
      (cons 
	(list x (car L))
	(match x (cdr L))
      )
    )
  )
)

;; main
(define prod
  (lambda (L1 L2)
    (if (null? L1)
      L1
      (append
	(match (car L1) L2)
	(prod (cdr L1) L2)
      )
    )
  )
)

;; 18- som_list
(define som_list
  (lambda (L)
    (if (null? L)
      0
      (+ (car L) (som_list (cdr L)))
    )
  )
)


;; 19- triang
;; Sous fonction
;; -- entree --
;; iter: rang actuel de l'iteration
;; init: valeur initiale de la fonction
(define subTriang
  (lambda (iter init)
    (if (= iter 1)
      (list 1)
      (if (= iter init)
	(append 
	  (subTriang (- iter 1) init)
	  (list init)
	  (mirroir (subTriang (- iter 1) init))
	)
	(append
	  (subTriang (- iter 1) init)
	  (list iter)
	)
      )
    )
  )
)
    
;; Main
(define triang
  (lambda (n)
    (if (<= n 1)
      (list n)
      (subTriang n n)
    )
  )
)

;; 20- fibo ------------- RECURSIF TERMINAL ------------- 
;; sous fonction fib
;; -- entree --
;; n:	 valeur recherchee
;; u1:	 valeur n+1
;; u2:	 valeur n+1
(define subFib
  (lambda (n u1 u2)
    (if (= n 1)
      u2
      (subFib (- n 1) u2 (+ u1 u2))
    )
  )
)

;; Main
(define fibo
  (lambda (n)
    (subFib n 1 1)
  )
)

;; 21- Moy
;; sous fonction subMoy(L, sum, count)
;; @param L:	  liste a evaluer
;; @param sum:	  somme des elements deja evalues
;; @param count:  nombre d'elements deja evalues
(define subMoy 
  (lambda (L sum count)
    (if (null? L)
      (exact->inexact (/ sum count)) ;;exact->inexact cast en flotant
      (subMoy 
	(cdr L)
	(+ sum (car L))
	(+ count 1)
      )
    )
  )
)

;; Main
(define moy
  (lambda (L)
    (subMoy L 0 0)
  )
)

;; 22- ies
;; Sous procedures
;; Une pour val inferieures
;; Une pour val egales
;; Une pour val superieures

;; Inf
(define subIesInf
  (lambda (L n)
    (if (null? L)
      L
      (if (< (car L) n) ;; Si la val est inferieure a n on l'ajoute
	(cons (car L) (subIesInf (cdr L) n))
	(subIesInf (cdr L) n)
      )
    )
  )
)

;; Egal
(define subIesEgal
  (lambda (L n)
    (if (null? L)
      L
      (if (= (car L) n) ;; Si la val est egale a n on l'ajoute
	(cons (car L) (subIesEgal (cdr L) n))
	(subIesEgal (cdr L) n)
      )
    )
  )
)

;; Sup
(define subIesSup
  (lambda (L n)
    (if (null? L)
      L
      (if (> (car L) n) ;; Si la val est superieure a n on l'ajoute
	(cons (car L) (subIesSup (cdr L) n))
	(subIesSup (cdr L) n)
      )
    )
  )
)



;; Main 
(define ies
  (lambda (L n)
    (if (null? L)
      n
      (list (subIesInf L n) (subIesEgal L n) (subIesSup L n))
    )
  )	
)

;; 23- tri_ins
;; Sous procedure inserer
(define inserer
  (lambda (e L)
    (if (null? L)
      (list e)
      (if (> e (car L))
	(cons
	  (car L)
	  (inserer e (cdr L))
	)
	(cons e L)
      )
    )
  )
)

;; Main
(define tri_ins
  (lambda (L)
    (if (null? L)
      L
      (inserer
	(car L)
	(tri_ins (cdr L))
      )
    )
  )
)

;; Fibonacci Pure
(define fibonacci
  (lambda (n)
    (if (or (= n 1) (= n 0))
      1
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))
    )
  )
)

;; 24- tri_sel
;; -- Principe --
#| 
  Tant que liste non vide
  i = indice minimum
  L2 = supprimer(L i) // Supprime lelement en position i et retourne la liste
  AR sur L2
  
|#


;; sous procedure IndiceMinimum
;; @param L: liste a traiter
;; @param indiceActuel
;; @param indiceMinActuel
;; @return: indice de element de valeur minimale dans la liste
(define indiceMin
  (lambda (L indiceActuel indiceMinActuel minActuel)
    (if (null? L)
      indiceMinActuel
      (if (< (car L) minActuel) ;; Si l'element actuel est + petit
	(indiceMin (cdr L) (+ indiceActuel 1) indiceActuel (car L))  ;; On met a jour le min
	(indiceMin (cdr L) (+ indiceActuel 1) indiceMinActuel minActuel) ;; Sinon on le conserve
      )
    )
  )
)

;; sous procedure supprimerItem
;; @parm L: Liste dont on supprime l'item
;; @param i: indice de l'item a supprimer
;; @return: liste privee de l'element d'indice i
(define supprimerItem
  (lambda (L i)
    (if (null? L)
      L
      (if (= i 0)
	(cdr L)
	(cons (car L) (supprimerItem (cdr L) (- i 1)))
      )
    )
  )
)

;; Main tri_sel TODO ...
;; Les sous fonctions marchent, il ne reste que le main a faire
(define tri_sel
  (lambda (L)
    (if (null? L)
      L
      (cons
	(nieme (indiceMin L 0 0 0)  L)
	(tri_sel (supprimerItem L (indiceMin L 0 0 0)))
      )
    )
  )
)   

;; Affichage
;; :set softtabstop=2
;; :set lisp 
(define n '(10 5 15 20))
(define m '(1 2 3 1 1 3 5 7 1))
(define o '(1 (2) ((3) (4) (5 6)) ((7 8) (9))))
(define i 9)
(define x 10)
(display "\ntri_sel ")
(display m)
(display " ")
(display " ")
;;(display m)
(display ": ")
(display (tri_sel m))
;;(display " || ")
;;(display (fibo i))
(display "\n")


;; guile -s monFichier.scm

