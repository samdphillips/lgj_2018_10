#lang racket/base

(require racket/contract
         racket/math

         lens/common
         lens/data/struct)

(provide
 (contract-out (struct posn ([x real?] [y real?]))

               [posn-x-lens (lens/c posn? real?)]
               [posn-y-lens (lens/c posn? real?)]

               [random-posn (-> real? real? real? real? posn?)]
               [posn+ (-> posn? posn? posn?)]
               [posn+xy (-> posn? real? real? posn?)]
               [posn* (-> real? posn? posn?)]
               [posn-magnitude (-> posn? real?)]
               [posn-norm (-> posn? posn?)]

               [radian+ (-> radian? radian? radian?)]
               ))

(struct/lens posn [x y] #:transparent)

(define (random-posn x0 x1 y0 y1)
  (posn (+ x0 (* (- x1 x0) (random)))
        (+ y0 (* (- y1 y0) (random)))))

(require (for-syntax racket/base))

(define (posn+ p0 p1)
  (posn (+ (posn-x p0) (posn-x p1))
        (+ (posn-y p0) (posn-y p1))))

(define (posn+xy p x y)
  (posn (+ (posn-x p) x)
        (+ (posn-y p) y)))

(define (posn* d p)
  (posn (* d (posn-x p)) (* d (posn-y p))))

(define (posn-magnitude p)
  (let ([x (posn-x p)]
        [y (posn-y p)])
    (sqrt (+ (* x x) (* y y)))))

(define (posn-norm p)
  (let ([m (posn-magnitude p)])
    (posn* (/ 1 m) p)))

(define TWOPI (* pi 2))

(define (radian? v)
  (and (real? v) (<= v TWOPI)))

(define (radian+ r0 r1)
  (let reduce ([r (+ r0 r1)])
    (cond
      [(< r 0)     (reduce (+ TWOPI r))]
      [(> r TWOPI) (reduce (- r TWOPI))]
      [else r])))

