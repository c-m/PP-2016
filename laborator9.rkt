#lang racket
; in scheme, by default eval e aplicativa
(define suma
  (lambda (a b)
    (+ a b)))

(define suma_late
  (lambda (a b)
    (lambda () (+ a b))))
((suma_late 1 2))

;sir infinit de 1
(define ones (cons 1 (lambda() ones)))
(define lazy_take (λ (n stream) (if (= n 0) '() 
                                   (cons (car  stream) (lazy_take (- n 1) ((cdr stream)))))))
(lazy_take 5 ones)
;sir 
(define sum (lambda (x y) (delay(+ x y))))


;sirul numerelor naturale
(define succ (λ (n) (+ n 1)))
(define (make_nats k)(cons k (λ() (make_nats(+ k 1)))))
(define nat_stream (make_nats 0))

(lazy_take 5 nat_stream)

;zipWith pe stream-uri
(define (my_zip op s p) (cons (op (car s) (car p)) (λ() (my_zip op ((cdr s)) ((cdr p))))))
(lazy_take 5 (my_zip + ones nat_stream))

;map on streams
(define (my_map op stream) (cons (op (car stream)) (λ() (my_map op ((cdr stream))))))

;even stream
(define evens (my_map (λ(x) (* 2 x)) nat_stream))
(lazy_take 5 evens)

;fibo stream
(define fibo (cons 1 (λ() (cons 1 (λ() (my_zip + fibo ((cdr fibo))))))))
(lazy_take 8 fibo)