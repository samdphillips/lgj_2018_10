#lang racket/base

(require racket/class
         racket/draw
         racket/format
         racket/gui/base
         racket/match

         (only-in racket/math pi)

         lens/common
         lens/data/struct

         "geometry.rkt"
         "slime.rkt")

(provide run-game)

(define MAX-SHIP-SPEED 200)
(define MAX-ZOOM 4)
(define SHIP-POOF 5)
(define SPACE-FRICTION -1/1000)
(define SHIP-TURN (/ pi 18))
(define INIT-PARTICLE-POWER 100)
(define CAMERA-RATIO 0.9)



(struct/lens game [ship particles slimes] #:transparent)
(struct/lens ship [posn dir v] #:transparent)
(struct/lens particle [posn v power] #:transparent)

(define game-ship-v-lens (lens-thrush game-ship-lens ship-v-lens))

(define (ship-x s) (lens-view (lens-thrush ship-posn-lens posn-x-lens) s))
(define (ship-y s) (lens-view (lens-thrush ship-posn-lens posn-y-lens) s))

(define (ship-exhaust-port s)
  (match-define (ship (posn x y) dir _) s)
  (posn (+ x (* (cos dir) -5))
        (+ y (* (sin dir) -5))))

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

(define ship-power
  (let ([r->v (lerp (/ SHIP-POOF 2) SHIP-POOF #f)])
    (lambda ()
      (r->v (random)))))

(define (draw-ship s dc)
  (match-define (ship (posn x y) dir _) s)
  (with-transformation dc
    ([translate x y]
     [rotate (- dir)])
    (send dc set-pen "black" 1 'solid)
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
           [w 0]
           [h 0]
           [scale MAX-ZOOM])

    (define/public (get-bbox)
      (let ([vw/2 (/ w (* scale 2))]
            [vh/2 (/ h (* scale 2))])
        (cons (posn (- x vw/2) (- y vh/2))
              (posn (+ x vw/2) (+ y vh/2)))))

    (define/public (update-viewport g d nw nh)
      (define-syntax-rule (incr! var val)
        (set! var (+ var val)))
      (set! w nw)
      (set! h nh)
      (match-define (game (ship (posn shx shy) _ _) _ _) g)
      (define dx (- shx x))
      (define dy (- shy y))
      (incr! x (* dx CAMERA-RATIO d))
      (incr! y (* dy CAMERA-RATIO d))
      (define w/2 (/ w 2))
      (define h/2 (/ h 2))
      (incr! scale
             (let ([dx (abs dx)]
                   [dy (abs dy)]
                   [iw (if (> w/2 100) (- w/2 100) w/2)]
                   [ih (if (> h/2 100) (- h/2 100) h/2)])
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
        (random-posn -1000 1000 -1000 1000)))

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
      (define slimes-drawn (box 0))
      (with-transformation dc ([transform view-transform])
        (let ([orig (send dc get-pen)])
          (send dc set-pen "white" 1 'solid)
          (for ([s (in-list star-field)])
            (send dc draw-point (posn-x s) (posn-y s)))
          (send dc set-pen orig))
        (for ([p (in-list (game-particles g))])
          (draw-particle p dc))
        (draw-ship (game-ship g) dc)
        (match-let ([(cons (posn x0 y0) (posn x1 y1))
                     (send viewport get-bbox)]
                    [step (* 15 (/ 1 (get-field scale viewport)))])
          (define (tf v) (* 30 (truncate (/ v 30))))
          (draw-slimes (game-slimes g) dc slimes-drawn
                       step
                       (- (tf x0) 30) (+ (tf x1) 30)
                       (- (tf y0) 30) (+ (tf y1) 30))))
      (set! last-update cur-time)
      (match-let ([(game (ship (posn x y) dir (and v (posn dx dy))) p* s*) g])
        (define txt
          (list
           (~a "size:      (" w ", " h ")")
           (~a "fps:       " (~r (if (zero? d) 0 (round (/ 1 d)))))
           (~a "viewport:  " view-transform)
           (~a "view:      " (send viewport get-bbox))
           (~a "posn:      (" (~r x) ", " (~r y) ")")
           (~a "vec:       (" (~r dx) ", " (~r dy) ")")
           (~a "speed:     " (~r (posn-magnitude v)))
           (~a "dir:       " (~r dir))
           (~a "slimes:    " (~r (unbox slimes-drawn)))
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

    (field [last-update (current-milliseconds)]
           [keys-pressed (make-hasheq)])

    (define (key-active? k)
      (hash-has-key? keys-pressed (send k get-key-code)))

    (define (deactivate-key! k)
      (hash-remove! keys-pressed (send k get-key-release-code)))

    (define (activate-key! k)
      (unless (key-active? k)
        (let ([kc (send k get-key-code)]
              [ts (send k get-time-stamp)])
          (hash-set! keys-pressed kc ts))))

    (define/public (dispatch-keys model)
      (define k*
        (sort
         (for/list ([(k v) (in-hash keys-pressed)]) (cons v k))
         < #:key car))
      (for/fold ([model model]) ([k (in-list k*)])
        ((hash-ref keymap (cdr k) (lambda () values)) model)))

    (define/public (on-key k)
      (if (eq? 'release (send k get-key-code))
          (deactivate-key! k)
          (activate-key! k)))

    (define/public (do-update d model)
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
              (set-box! model (do-update d (dispatch-keys (unbox model))))
              (set! last-update cur-time))]
           [interval 16]))

    (super-new)))

(define-keymap keymap
  [#\r
   identity-lens
   (lambda (g)
     (game (ship (posn 0 0) 0 (posn 0 0))
           null
           (game-slimes g)))]

  [#\w
   identity-lens
   (lambda (game)
     (define pwr (ship-power))
     (lens-transform/list
      game

      game-ship-lens
      (lambda (ship)
        (let ([d (ship-dir ship)])
          (lens-set
           ship-v-lens
           ship
           (posn+xy (ship-v ship)
                    (* (cos d) pwr)
                    (* (sin d) pwr)))))

      game-ship-v-lens
      (lambda (v)
        (let ([m (posn-magnitude v)])
          (if (> m MAX-SHIP-SPEED)
              (posn* MAX-SHIP-SPEED (posn-norm v))
              v)))

      game-particles-lens
      (lambda (p*)
        (cons (particle
               (ship-exhaust-port (game-ship game))
               (posn* -1/100 (ship-v (game-ship game)))
               (* (/ pwr SHIP-POOF) INIT-PARTICLE-POWER))
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

(define (run-game #:slimes [num-slimes 20]
                  #:seed [seed (random (sub1 (expt 2 31)))])
  (random-seed seed)

  (define game-state
    (box
     (game
      (ship (posn 0 0) 0 (posn 0 0))
      null
      (for/list ([_n num-slimes])
        (slime (random-posn -500 500 -500 500)
               (+ 5 (random 15))
               (posn 0 0))))))

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
         [width 800]
         [height 600]))

  (define c
    (new game-view%
         [parent v]
         [ctrl ctrl]
         [model game-state]))
  (send v show #t))
