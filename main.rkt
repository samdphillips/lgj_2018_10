#lang racket/base

(require racket/class
         racket/draw
         racket/gui/base

         lens/common
         lens/data/struct)

(define (lens/get+set lens)
  (values (lambda (x)   (lens-view lens x))
          (lambda (x y) (lens-set lens x y))))

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

(define-syntax-rule
  (with-transform dc ([xf p* ...] ...) body ...)
  (let ([init-tf (send dc get-transformation)])
    (dynamic-wind
     (lambda ()
       (send dc xf p* ...) ...)
     (lambda () body ...)
     (lambda () (send dc set-transformation init-tf)))))

(define (draw-ship s dc)
  (with-transform dc
    ([rotate (ship-dir s)]
     [translate (ship-x s) (ship-y s)])
    (send dc draw-polygon
          '((-5 . -5) (0 . 10) (5 . -5)))))

(define state-ship-lens identity-lens)

(define-values (state-ship state-ship-set)
  (lens/get+set state-ship-lens))

(define game-view%
  (class canvas%
    (init-field model
                ctrl)
    (inherit get-size)

    (define (do-paint canvas dc)
      (define-values (width height) (get-size))
      (send dc set-smoothing 'smoothed)
      (with-transform dc
        ([translate (/ width 2) (/ height 2)]
         [scale 2 2])
        (draw-ship (state-ship (unbox model)) dc)))

    (define/override (on-char k)
      (send ctrl on-key k))

    (super-new [paint-callback do-paint])))

(define game-ctrl%
  (class object%
    (init-field model)

    (define/public (on-key k)
      (println (send k get-key-code)))

    (super-new)))

(define (make-view w h model ctrl)
  (define f
    (new (class frame%
           ;; make the canvas have focus if the window gets focus
           (define/override (on-focus f)
             (super on-focus f)
             (send (car (send this get-children)) focus))
           (super-new))
         [label "slimestroids"]
         [width w]
         [height h]))

  (define c
    (new game-view%
         [parent f]
         [ctrl ctrl]
         [model model]))
  f)

(define (game)
  (define state
    (box (ship (posn 0 0) 0 0)))

  (define ctrl
    (new game-ctrl% [model state]))

  (define v (make-view 500 500 state ctrl))
  (send v show #t))
