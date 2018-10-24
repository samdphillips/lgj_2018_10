#lang racket/base

(require racket/contract
         racket/match
         racket/math

         lens/common
         lens/data/struct)

(provide
 (contract-out (struct posn ([x real?] [y real?]))

               [posn-x-lens (lens/c posn? real?)]
               [posn-y-lens (lens/c posn? real?)]

               [random-posn (-> real? real? real? real? posn?)]
               [posn+ (-> posn? posn? posn?)]
               [posn* (-> real? posn? posn?)]
               [posn-magnitude (-> posn? real?)]

               [radian+ (-> radian? radian? radian?)]
               ))

(struct/lens posn [x y] #:transparent)

(define (random-posn x0 x1 y0 y1)
  (posn (+ x0 (* (- x1 x0) (random)))
        (+ y0 (* (- y1 y0) (random)))))

(define (posn+ p0 p1)
  (match-define (posn x0 y0) p0)
  (match-define (posn x1 y1) p1)
  (posn (+ x0 x1) (+ y0 y1)))

(define (posn* d p)
  (match-define (posn x y) p)
  (posn (* d x) (* d y)))

(define (posn-magnitude p)
  (match-define (posn x y) p)
  (sqrt (+ (* x x) (* y y))))

(define TWOPI (* pi 2))

(define (radian? v)
  (and (real? v) (<= v TWOPI)))

(define (radian+ r0 r1)
  (let reduce ([r (+ r0 r1)])
    (cond
      [(< r 0)     (reduce (+ TWOPI r))]
      [(> r TWOPI) (reduce (- r TWOPI))]
      [else r])))

