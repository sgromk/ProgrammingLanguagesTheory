
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below


;; int*int*int -> int list
;; Returns a list of numbers from low to high, in increments of stride
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


;; string list * string -> string list
(define (string-append-map los suf)
  (map (lambda (s) (string-append s suf)) los))


;; 'a list * int -> 'a
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))


;; 'a stream * int -> 'a list
(define (stream-for-n-steps s n)
  (let ([ans (s)])
    (if (<= n 0)
        null
        (cons (car ans) (stream-for-n-steps (cdr ans) (- n 1))))))


;; none -> (int * stream)
(define funny-number-stream
         (letrec ([f (lambda (x)
                       (if (= (remainder x 5) 0)
                           (cons (* -1 x) (lambda () (f (+ x 1))))
                           (cons x  (lambda () (f (+ x 1))))))])
           (lambda () (f 1))))


;; none -> (string * stream)
(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" (lambda () (dog))))]
           [dog (lambda () (cons "dog.jpg" (lambda () (dan))))])
    (lambda () (dan))))


;; stream -> (int * stream) stream
(define (stream-add-zero s)
  (letrec ([f (lambda ()
                (let ([ans (s)])
                  (cons (cons 0 (car ans)) (stream-add-zero (cdr ans)))))])
    (lambda () (f))))


;; 'a list * 'b list -> stream
(define (cycle-lists xs ys)
  (letrec ([cycle-helper (lambda (n)
                           (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                                 (lambda () (cycle-helper (+ n 1)))))])
    (lambda () (cycle-helper 0))))


;; 'a * vector -> vector
(define (vector-assoc v vec)
  (letrec ([assoc-helper (lambda (n)
                           (cond [(= (vector-length vec) n) #f]
                                 [(not (pair? (vector-ref vec n))) (assoc-helper (+ n 1))]
                                 [(equal? (car (vector-ref vec n)) v) (vector-ref vec n)]
                                 [#t (assoc-helper (+ n 1))]))])
    (assoc-helper 0)))
  

;; 'a list * int -> fn ('a -> vector)
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [nextMut 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v cache)])
                  (if ans
                      (cdr ans)
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin
                              (vector-set! cache nextMut new-ans)
                              (if (= nextMut (- n 1))
                                  (set! nextMut 0)
                                  (set! nextMut (add1 nextMut)))
                              new-ans)
                            #f)))))])
    f))










