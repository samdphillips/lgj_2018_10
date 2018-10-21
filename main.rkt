#lang racket/base

(require racket/class
         racket/draw
         racket/format
         racket/gui/base
         racket/match

         (only-in math pi)

         lens/common
         lens/data/struct)

(define MAX-SHIP-SPEED 100)
(define MAX-ZOOM 4)
(define SHIP-POOF 5)
(define SPACE-FRICTION -1/1000)
(define SHIP-TURN (/ pi 18))
(define INIT-PARTICLE-POWER 100)

(define (lens/get+set lens)
  (values (lambda (x)   (lens-view lens x))
          (lambda (x y) (lens-set lens x y))))

(define ((lens-view^ lens) v)
  (lens-view lens v))

(struct/lens posn [x y] #:transparent)

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

(define (radian+ r0 r1)
  (let reduce ([r (+ r0 r1)])
    (cond
      [(< r 0)     (reduce (+ TWOPI r))]
      [(> r TWOPI) (reduce (- r TWOPI))]
      [else r])))

(struct/lens ship [posn dir v] #:transparent)

(define ship-x-lens (lens-thrush ship-posn-lens posn-x-lens))
(define ship-y-lens (lens-thrush ship-posn-lens posn-y-lens))

(define (ship-x s) (lens-view ship-x-lens s))
(define (ship-y s) (lens-view ship-y-lens s))

(define (ship-exhaust-port s)
  (match-define (ship (posn x y) dir _) s)
  (posn (+ x (* (cos dir) -5))
        (+ y (* (sin dir) -5))))

(struct/lens particle [posn v power] #:transparent)

#|
(struct bullet [posn v] #:transparent)

(struct slime [posn v] #:transparent)
|#

(struct/lens game [ship particles] #:transparent)

(define ((state-transform lens txform) v)
  (lens-transform lens v txform))

(define-syntax-rule
  (with-transformation dc (xf ...) body ...)
  (let ([init-tf (send dc get-transformation)])
    (dynamic-wind
     (lambda ()
       (send* dc xf ...))
     (lambda () body ...)
     (lambda () (send dc set-transformation init-tf)))))

(define-syntax-rule (define-keymap name [key lens txform] ...)
  (define name
    (make-immutable-hasheq
     (list (cons 'key (state-transform lens txform)) ...))))

(define ((lerp start end round?) v)
  (let ([m (/ (- start end) -1)]
        [r (if round? (lambda (v) (inexact->exact (round v))) values)])
    (r (+ start (* m v)))))

(define (draw-ship s dc)
  (with-transformation dc
    ([translate (ship-x s) (ship-y s)]
     [rotate (- (ship-dir s))])
    (send dc draw-polygon
          '((-5 . 5) (-5 . -5) (10 . 0))))

  (match-let ([(posn x y) (ship-exhaust-port s)])
    (let ([orig (send dc get-pen)])
      (send dc set-pen "red" 5 'solid)
      (send dc draw-point x y)
      (send dc set-pen orig))))

(define (draw-particle p dc)
  (match-define (particle (posn x y) _ power) p)
  (when (> power 0)
    (let ([orig (send dc get-pen)])
      (let* ([pwr (/ power INIT-PARTICLE-POWER)]
             [r ((lerp 232 140 #t) pwr)]
             [g ((lerp 174 21 #t) pwr)]
             [b ((lerp 39 2 #t) pwr)]
             [c (make-color r g b pwr)]
             [s ((lerp 1 5 #f) pwr)])
        (send dc set-pen c s 'solid)
        (send dc draw-point x y))
      (send dc set-pen orig))))

(define game-viewport%
  (class object%
    (field [x 0]
           [y 0]
           [scale 4])

    (define/public (update-viewport g d w h)
      (define-syntax-rule (incr! var val)
        (set! var (+ var val)))
      (match-define (game (ship (posn shx shy) _ _) _) g)
      (define dx (- shx x))
      (define dy (- shy y))
      (incr! x (* dx 0.3 d))
      (incr! y (* dy 0.3 d))
      (define w/2 (/ w 2))
      (define h/2 (/ h 2))
      (incr! scale
             (let ([dx (abs dx)]
                   [dy (abs dy)]
                   [iw (if (> w/2 50) (- w/2 100) w/2)]
                   [ih (if (> h/2 50) (- h/2 100) h/2)])
               (let* ([scx (if (zero? dx) MAX-ZOOM (/ iw dx))]
                      [scy (if (zero? dy) MAX-ZOOM (/ ih dy))])
                 (* (- (min MAX-ZOOM scx scy) scale) 2.5 d))))
      (define scalex scale)
      (define scaley (- scale))
      (define tx (- w/2 (* scalex x)))
      (define ty (- h/2 (* scaley y)))
      (vector scalex 0 0 scaley tx ty))

    (super-new)))

(define game-view%
  (class canvas%
    (init-field model
                ctrl)
    (field [last-update (current-milliseconds)])
    (inherit get-size)

    (define viewport
      (new game-viewport%))

    (define refresh-timer
      (new timer%
           [notify-callback
            (lambda () (send this refresh-now))]))

    (define star-field
      (for/list ([x (in-range 1000)])
        (cons (random -1000 1000) (random -1000 1000))))

    (define (do-paint canvas dc)
      (define-values (w h) (get-size))
      (define cur-time (current-milliseconds))
      (define d (/ (- cur-time last-update) 1000))
      (define g (unbox model))
      (send dc set-smoothing 'smoothed)
      (send dc set-background "black")
      (send dc set-text-foreground "white")
      (send dc clear)
      (define view-transform (send viewport update-viewport g d w h))
      (with-transformation dc ([transform view-transform])
        (let ([orig (send dc get-pen)])
          (send dc set-pen "white" 1 'solid)
          (for ([s (in-list star-field)])
            (send dc draw-point (car s) (cdr s)))
          (send dc set-pen orig))
        (for ([p (in-list (game-particles g))])
          (draw-particle p dc))
        (draw-ship (game-ship g) dc))
      (set! last-update cur-time)
      (match-let ([(game (ship (posn x y) dir (posn dx dy)) p*) g])
        (define txt
          (list
           (~a "size: (" w ", " h ")")
           (~a "fps: " (~r (round (/ 1 d))))
           (~a "viewport: " view-transform)
           (~a "posn: (" (~r x) ", " (~r y) ")")
           (~a "vec:  (" (~r dx) ", " (~r dy) ")")
           (~a "dir: " (~r dir))
           (~a "particles: " (length p*))))
        (send dc set-font (make-font #:size 10 #:family 'modern))
        (for ([r (in-naturals)]
              [t (in-list txt)])
          (send dc draw-text t 0 (* 15 r)))))

    (define/override (on-focus f)
      (super on-focus f)
      (send refresh-timer start 16))

    (define/override (on-char k)
      (send ctrl on-key k))

    (super-new [paint-callback do-paint])))

(define game-ctrl%
  (class object%
    (init-field model keymap)

    (field [last-update (current-milliseconds)])

    (define/public (on-key k)
      (let/ec ret
        (let* ([keycode (send k get-key-code)]
               [transform (hash-ref keymap keycode ret)])
          (set-box! model (transform (unbox model))))))

    (define/public (on-update d model)
      (lens-transform/list
       model

       game-ship-lens
       (lambda (ship)
         (lens-transform ship-posn-lens ship
                         (lambda (p)
                           (posn+ p (posn* d (ship-v ship))))))

       game-particles-lens
       (lambda (p*)
         (for/fold ([r* null]) ([p (in-list p*)])
           (let ([pwr (particle-power p)])
             (if (< pwr 0)
                 r*
                 (cons (lens-set particle-power-lens p (sub1 pwr)) r*)))))

       game-particles-lens
       (lambda (p*)
         (for/list ([p (in-list p*)])
           (lens-transform particle-posn-lens p
                           (lambda (o)
                             (posn+ o (particle-v p))))))

       (lens-thrush game-ship-lens ship-v-lens)
       (lambda (p)
         (posn+ (posn* SPACE-FRICTION p) p))
       ))

    (define update-timer
      (new timer%
           [notify-callback
            (lambda ()
              (define cur-time (current-milliseconds))
              (define d (/ (- cur-time last-update) 1000))
              (set-box! model (send this on-update d (unbox model)))
              (set! last-update cur-time))]
           [interval 16]))

    (super-new)))

(define-keymap keymap
  [escape
   identity-lens
   (lambda (g)
     (game (ship (posn 0 0) 0 (posn 0 0)) null))]

  [#\w
   identity-lens
   (lambda (game)
     (lens-transform/list
      game

      game-ship-lens
      (lambda (ship)
        (let ([d (ship-dir ship)])
          (lens-set
           ship-v-lens
           ship
           (let* ([p (posn+ (ship-v ship)
                            (posn (* (cos d) SHIP-POOF)
                                  (* (sin d) SHIP-POOF)))]
                  [m (posn-magnitude p)])
             (cond
               [(< m MAX-SHIP-SPEED) p]
               [else
                (posn+ p (posn (* (cos d) (- MAX-SHIP-SPEED m))
                               (* (sin d) (- MAX-SHIP-SPEED m))))])))))

      game-particles-lens
      (lambda (p*)
        (cons (particle
               (ship-exhaust-port (game-ship game))
               (posn* -1/100 (ship-v (game-ship game)))
               INIT-PARTICLE-POWER)
              p*))))]

  [#\a
   (lens-thrush game-ship-lens ship-dir-lens)
   (lambda (dir)
     (radian+ dir SHIP-TURN))]

  [#\d
   (lens-thrush game-ship-lens ship-dir-lens)
   (lambda (dir)
     (radian+ dir (- SHIP-TURN)))]
  )

(define (run-game)
  (define game-state
    (box (game (ship (posn 0 0) 0 (posn 0 0)) null)))

  (define ctrl
    (new game-ctrl%
         [model game-state]
         [keymap keymap]))

  (define v
    (new (class frame%
           ;; make the canvas have focus if the window gets focus
           (define/override (on-focus f)
             (super on-focus f)
             (send (car (send this get-children)) focus))
           (super-new))
         [label "slimestroids"]
         [width 500]
         [height 500]))

  (define c
    (new game-view%
         [parent v]
         [ctrl ctrl]
         [model game-state]))
  (send v show #t))
