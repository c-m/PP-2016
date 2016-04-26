#lang racket
(require 2htdp/image)

(let sierpinski ([n 4])
  (if (zero? n)
      (triangle 2 'solid 'red)
      (let ([t (sierpinski (- n 1))])
        (above t (beside t t)))))

; side effects
(define a 10)
a
(set! a (+ a 1))
a

; Definiti si apelati adunarea in forma curry
(define add-uncurry
 (lambda (x y)
  (+ x y)))

(define add-curry
  (lambda (x)
    (lambda (y)
      (+ x y))))

; Implementati o functie care primeste un operator binar in forma uncurry si intoarce acelasi operator in forma curry
(define (makeCurry op) (lambda (a) (lambda (b) (op a b))))

; Implementati o functie care primeste un operator binar in forma curry, si intoarce operatorul in forma uncurry
(define (makeUncurry op) (lambda (a b) ((op a) b)))
((makeUncurry add-curry) 3 56)

; eq? vs equal?
(eq? '(1 2 3) '(1 2 3))
(equal? '(1 2) '(1 2))

; explorare
(car '(1 2 3))
(cdr '(1 2 3))
(caar '((1 2)))
(cddr '(1 2 3))

; folds
(foldl cons '() '(1 2 3))
(foldr cons '() '(1 2 3))

; weakly typed
'(1 (2 3) "lista")
(define fw (lambda(x) (if x "lista" '())))

; runtime typing
(define value #t)
(if value 100 (+ '() 1))

;factorial classic
(define factorial
  (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))
;fact tail-rec
(define tail-fact
  (lambda (n a)
    (if (= n 0) 
        a
        (tail-fact (- n 1) (* a n)))))

; watch performance
(require racket/trace)
(trace factorial)
(trace tail-fact)
(factorial 5)
(tail-fact 5 1)

;check palindrome
(define reverse 
  (lambda (l) 
    (foldl cons '() l)))

(define ispalin?
  (lambda (l)
    (equal? l (reverse l))))

(ispalin? '(1 2 1))
(ispalin? '(1 2 2))

; Exercitii
;lista imbricata in lista simpla
(define (flatten list)
  (foldr (lambda (elem acc)
           (if (list? elem) (append (flatten elem) acc) (cons elem acc))) '() list))
(flatten '(1 (2 3 (4 (5)) 6) 7 (8 9)))

(define flatten2 (lambda (l) (if (empty? l) '() 
                      (if (list? (car l)) ( append (flatten2 (car l) ) (flatten2 (cdr l)))
                           (cons (car l) (flatten2 (cdr l)))))))
(flatten2 '(1 (2 3 (4 (5)) 6) 7 (8 9)))


;compress list
(define (compress list)
  (foldr (lambda (elem acc)
           (if (and (pair? acc)(= elem (car (car acc)))) 
               (cons (cons elem (car acc)) (cdr acc)) 
               (cons (cons elem '()) acc)
            )) '() list))

(compress '(1 1 1 1 1 2 2 2 3 4 5))

(define consecutiveF( lambda (l) (if (empty? l) '()
                                   (cons (filter  (lambda (x) (eq? x (car l))) l ) (consecutiveF (filter (lambda (x) (not (eq? x (car l)))) (cdr l)))))))

(consecutiveF '(1 1 1 1 1 2 2 2 3 4 5))

;count compress list
(define (countc list)
  (foldr (lambda (elem acc)
           (if (and (pair? acc)(equal? elem (car (car acc)))) 
               (cons (cons elem (cons (+ (car (cdr(car acc))) 1) '())) (cdr acc)) 
               (cons (cons elem '(1)) acc)
            )) '() list))

(countc '(1 1 1 1 1 2 2 2 3 4 5))

;count compress with map
(define (countc1 list) (map (lambda (elem) (cons (car elem) (cons (length elem) '()))) (compress list)))
(countc1 '(1 1 1 1 1 2 2 2 3 4 5))

;<elem, nr aparitii> lista arbitrara
(define (count_arb list) (countc (sort list <)))
(count_arb '(1 2 3 1 4 2 2 5))

;rotatia cu n pozitii stanga
(define (rot_left list n) (append (drop list n) (take list n)))
(rot_left '(1 2 3 4 5 6 7) 2)

;rotatia cu n pozitii dreapta
(define (rot_right list n) (append (take-right list n) (drop-right list n)))
(rot_right
 '(1 2 3 4 5 6 7) 2)