#lang typed/racket/base

(require racket/match

         typed/racket/class
         typed/racket/draw

         math/base
         math/array)

;; planet

(: make-noise-array (Integer Integer -> (Array Real)))
(define (make-noise-array w h)
  (build-array (vector w h) (lambda (_coord) (random))))

(: int (Real -> Integer))
(define (int v)
  (cast (inexact->exact (floor v)) Integer))

(: smooth-noise-point ((Array Real) Real Real -> Real))
(define (smooth-noise-point noise x y)
  (: ref (Integer Integer -> Real))
  (define (ref x y)
    (array-ref noise (vector x y)))

  (match-define (vector dx dy) (array-shape noise))
  (define fx (- x (floor x)))
  (define fy (- y (floor y)))

  (define x0 (modulo (+ (int x) dx) dx))
  (define y0 (modulo (+ (int y) dy) dy))

  (define x1 (modulo (+ x0 dx -1) dx))
  (define y1 (modulo (+ y0 dy -1) dy))

  (+ (* fx       fy       (ref x0 y0))
     (* (- 1 fx) fy       (ref x1 y0))
     (* fx       (- 1 fy) (ref x0 y1))
     (* (- 1 fx) (- 1 fy) (ref x1 y1))))

(: make-smooth-noise ((Array Real) Integer Integer -> (Array Real)))
(define (make-smooth-noise noise w h)
  (define (nudge [v : Real]) (+ v 0.5))
  (build-array (vector w h)
               (lambda ([i : In-Indexes])
                 (define x (cast (vector-ref i 0) Integer))
                 (define y (cast (vector-ref i 1) Integer))
                 (smooth-noise-point noise (nudge x) (nudge y)))))

(: turbulence ((Array Real) Real Real Real -> Real))
(define (turbulence noise init-size x y)
  (let loop ([size : Real init-size]
             [v : Real 0])
    (if (>= size 1)
        (loop (/ size 2.0)
              (+ v (smooth-noise-point noise (/ x size) (/ y size))))
        (/ v init-size))))

; (: make-perlin-noise ((Array Real) Integer Integer -> (Array Real)))


(define n (make-noise-array 512 512))
#;(define sn (make-smooth-noise n 1024 1024))

#;
(define b
  (let ([bm (make-bitmap 512 128)]
        [pixels (make-bytes (* 512 128 4))])

    #;(define (sigmoid [v : Real])
      (let ([p (expt euler.0 (- (* v 10) 5))])
        (/ p (+ p 1))))
    #;(define sigmoid values)

    (define ((lerp [m : Real] [n : Real]) [v : Real])
      (int (+ m (* (- n m) v))))

    (define r (lerp 216 255))
    (define g (lerp  23 255))
    (define b (lerp 101 255))

    (for* ([y (in-range 128)]
           [x (in-range 512)])
      (define v (sigmoid (turbulence n 32 x y)))
      (define o (+ (* x 4) (* y 512 4)))
      (bytes-set! pixels o 255)
      (bytes-set! pixels (+ 1 o) (r v))
      (bytes-set! pixels (+ 2 o) (g v))
      (bytes-set! pixels (+ 3 o) (b v)))
    (send bm set-argb-pixels 0 0 512 128 pixels)
    bm))
