#lang racket

;;Implementati functia ce verifica daca un element apare intr-o lista data.
(define cont(lambda (e l) (if (empty? l) #f
                                  (if (equal? e (car l)) #t
                                      (cont e (cdr l))))))
(cont 5 '(1 2 3 4))
(cont `(1 2) `(1 ,`(1 2) ,`(2 3)))

;; Duplicati aparitiile elementelor unei liste de un numar dat de ori.
(define rep (lambda (n x) (if ( eq? x 0) '() (cons n ( rep n (- x 1))))))
     
(define duplicate (lambda (l x) (if ( empty? l) '()
                                    (append (rep (car l) x) (duplicate (cdr l) x)))))
(duplicate '(a b c) 3)

;;Dandu-se doi indecsi: l, r, si o lista list, extrageti sublista din list dintre cei doi indecsi. 
(define sublist( lambda (l r L) (if (eq? r 0) '()
                                    (if (eq? l 1) (cons (car L) (sublist l (- r 1) (cdr L)))
                                        (sublist (- l 1) (- r 1) (cdr L))))))
(sublist 2 4 '(a b c d e f g))

;;Inserati un element la o anumita pozitie intr-o lista.
(define insert_at(lambda (e p l) (if (eq? p 1) (cons e l)
                                     ( cons (car l) ( insert_at e (- p 1) (cdr l))))))
(insert_at 'e' 2 '(a b c d f))

;;Determinati daca un numar dat este prim.
(define devides (lambda (y x) (eq? (modulo x y) 0))) 
(define helper (lambda (n d) ( if (eq? d 1) #f
                                  (or (devides d n) (helper n (- d 1))))))
(define prime? (lambda (n) (not (helper n (- n 1)))))
(prime? 7)
(prime? 8)

;;Dandu-se un interval definit prin capetele acestuia, construiti o lista cu toate numerele prime din acel interval.
(define primes (lambda (l r) ( if (eq? l r) '()
                                  (if (prime? l) (cons l (primes (+ l 1) r))
                                      (primes (+ l 1) r)))))
(primes 3 12)