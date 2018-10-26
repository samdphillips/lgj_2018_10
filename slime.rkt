#lang racket/base

(require racket/contract
         racket/class
         racket/draw
         racket/format
         racket/generator
         racket/match

         math/array

         lens/data/struct
         "geometry.rkt")

(provide
 (contract-out [struct slime
                 ([posn posn?]
                  [r real?]
                  [v posn?])]
               [draw-slimes (-> (listof slime?)
                                (is-a?/c dc<%>)
                                any)]
               ))

(struct/lens slime [posn r v] #:transparent)

(define (sqr x) (* x x))

(define (slime-surface s* x y)
  (for/sum ([s (in-list s*)])
    (match-define (slime (posn sx sy) sr _) s)
    (/ (sqr sr)
       (let ([v (+ (sqr (- x sx)) (sqr (- y sy)))])
         (if (zero? v) 1/1000 v)))))

#;#;
(random-seed 120777)
(define s*
  (for/list ([n 20])
    (slime (random-posn -500 500 -500 500)
           (random 30)
           (posn 0 0))))

(define (make-slime-bitmap s* w h [sample 8])
  (let* ([step (round (/ w sample))]
         [bitmap (make-bitmap w h)]
         [pixels (make-bytes (* w h 4))])
    (let-values ([(lo hi) (let ([half (/ step 2)])
                            (if (odd? step)
                                (values (floor half)
                                        (ceiling half))
                                (values half half)))])
      (for ([i (in-range 0 (* w h 4) 4)]) (bytes-set! pixels i 255))
      (for* ([y (in-range 0 h step)]
             [x (in-range 0 w step)])
        (when (>= (slime-surface s* x y) 1)
          (for* ([y (in-range (max 0 (- y lo)) (min h (+ y hi)))]
                 [x (in-range (max 0 (- x lo)) (min w (+ x hi)))])
            (define o (+ (* x 4) (* y w 4)))
            (bytes-set! pixels (+ 1 o) 255))))
      (send bitmap set-argb-pixels 0 0 w h pixels)
      bitmap)))

(define (slime-array s* samples dim tx ty)
  (let ([step (/ dim samples)])
    (build-array (vector (add1 samples) (add1 samples))
                 (match-lambda
                   [(vector x y)
                    (slime-surface s*
                                   (+ tx (* x step))
                                   (+ ty (* y step)))]))))

(define (lerp-corner a b)
  (/ (- 1 a) (- b a)))

(define (slime-outline sa scale tx ty)
  (match-define (vector w h) (array-shape sa))
  (in-generator
   (for* ([i (in-range (sub1 w))]
          [j (in-range (sub1 h))])
     (define sl
       (array-slice-ref sa (list (list i (add1 i))
                                 (list j (add1 j)))))
     (match-define (list z w y x) (array->list sl))

     (define-syntax-rule (mkp x y)
       (posn (+ tx (* scale x)) (+ ty (* scale y))))
     (define-syntax-rule (top)    (mkp (+ i (lerp-corner z y)) j))
     (define-syntax-rule (bottom) (mkp (+ i (lerp-corner w x)) (add1 j)))
     (define-syntax-rule (left)   (mkp i (+ j (lerp-corner z w))))
     (define-syntax-rule (right)  (mkp (add1 i) (+ j (lerp-corner y x))))
     (define-syntax-rule (ret-line p0 p1) (yield (cons p0 p1)))

     (match (map (lambda (v) (or (and (>= v 1) 1) 0))
                 (list w x y z))
       [(or (list 0 0 0 0)
            (list 1 1 1 1))
        #f]
       [(or (list 1 0 0 0)
            (list 0 1 1 1))
        (ret-line (bottom) (left))]
       [(or (list 0 1 0 0)
            (list 1 0 1 1))
        (ret-line (bottom) (right))]
       [(or (list 1 1 0 0)
            (list 0 0 1 1))
        (ret-line (left) (right))]
       [(or (list 0 0 1 0)
            (list 1 1 0 1))
        (ret-line (top) (right))]
       [(or (list 1 0 1 0)
            (list 0 1 0 1))
        (ret-line (top) (left))
        (ret-line (bottom) (right))]
       [(or (list 0 1 1 0)
            (list 1 0 0 1))
        (ret-line (top) (bottom))]
       [(or (list 0 0 0 1)
            (list 1 1 1 0))
        (ret-line (top) (left))]
       [(or (list 0 1 1 0)
            (list 1 0 0 1))
        (ret-line (top) (bottom))]))))


(define s*
  (list
   (slime (posn 0 0)
          5
          (posn 0 0))
   (slime (posn -20 -20)
          5
          (posn 0 0))
   (slime (posn 20 20)
          5
          (posn 0 0))))

(define (slime-env s*)
  (define-values (minx miny maxx maxy)
    (for/fold ([mx +inf.0] [my +inf.0] [xx -inf.0] [xy -inf.0])
              ([s (in-list s*)])
      (match-define (slime (posn x y) r _) s)
      (let ([xl (- x r)]
            [xr (+ x r)]
            [yt (+ y r)]
            [yb (- y r)])
      (values (min mx xl xr) (min my yt yb)
              (max xx xl xr) (max xy yt yb)))))
  (define dim (max 5 (- maxx minx) (- maxy miny)))
  (values minx miny dim))

(define (draw-slimes s* dc)
  (define samples 15)
  (define-values (minx miny dim) (slime-env s*))
  (define sa (slime-array s* samples dim minx miny))

  (for ([s (in-list s*)])
    (match-define (slime (posn x y) _ _) s)
    (send dc set-pen "orange" 5 'solid)
    (send dc draw-point x y))

  (send dc set-pen "green" 1 'solid)
  (for ([segment (slime-outline sa (/ dim samples) minx miny)])
    (match-define (cons (posn x0 y0) (posn x1 y1)) segment)
    (send dc draw-line x0 y0 x1 y1))

  )