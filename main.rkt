#lang racket/base

(require racket/class
         racket/draw

         lens/common
         lens/data/struct)

(struct/lens posn [x y] #:transparent)

(struct/lens ship [posn dir v] #:transparent)

(define ship-x-lens (lens-thrush ship-posn-lens posn-x-lens))
(define ship-y-lens (lens-thrush ship-posn-lens posn-y-lens))

(define (ship-x s) (lens-view ship-x-lens s))
(define (ship-y s) (lens-view ship-y-lens s))

#|
(struct bullet [posn v] #:transparent)

(struct slime [posn v] #:transparent)
|#

(define (draw-ship s dc)
  (let ([tf (send dc get-transformation)])
    (dynamic-wind
     (lambda ()
       (send dc rotate (ship-dir s))
       (send dc translate (ship-x s) (ship-y s)))
     (lambda ()
       (send dc draw-polygon
             '((-5 . -5) (0 . 10) (5 . -5))))
     (lambda ()
       (send dc set-transformation tf)))))


(let* ([size 200]
       [scale 10]
       [zero (/ size 2)]
       [bm (make-bitmap size size)]
       [dc (send bm make-dc)]
       [tf (send dc get-transformation)])
  (send dc translate zero zero)
  (send dc scale scale scale)
  (draw-ship (ship (posn 0 0) 0 0) dc)
  (send dc set-transformation tf)
  bm)