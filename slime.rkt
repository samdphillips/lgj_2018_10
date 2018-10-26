#lang racket/base

(require racket/contract
         racket/class
         racket/draw
         racket/match

         lens/data/struct
         "geometry.rkt")

(provide
 (contract-out [struct slime
                 ([posn posn?]
                  [r real?]
                  [v posn?])]
               [draw-slimes (-> (listof slime?)
                                (is-a?/c dc<%>)
                                real?
                                real?
                                real?
                                real?
                                real?
                                any)]
               ))

(struct/lens slime [posn r v] #:transparent)

(define (sqr x) (* x x))

(define ((slime-surface s*) x y)
  (for/sum ([s (in-list s*)])
    (match-define (slime (posn sx sy) sr _) s)
    (/ (sqr sr)
       (let ([v (+ (sqr (- x sx)) (sqr (- y sy)))])
         (if (zero? v) 1/1000 v)))))

(define (slime-outline f line step i0 i1 j0 j1)
   (for* ([y (in-range j0 j1 step)]
          [x (in-range i0 i1 step)])
     (define x1 (+ step x))
     (define y1 (+ step y))

     (define-values (ul ur ll lr)
       (values (f x  y)
               (f x1 y)
               (f x  y1)
               (f x1 y1)))

     (define-syntax-rule (lerp-corner a b) (/ (- 1 a) (- b a)))
     (define-syntax-rule (mkp x y) (posn x y))
     (define-syntax-rule (top)    (mkp (+ x (lerp-corner ul ur)) y))
     (define-syntax-rule (bottom) (mkp (+ x (lerp-corner ll lr)) y1))
     (define-syntax-rule (left)   (mkp x (+ y (lerp-corner ul ll))))
     (define-syntax-rule (right)  (mkp x1 (+ y (lerp-corner ur lr))))
     (define-syntax-rule (ret p0 p1) (line p0 p1))

     (define (t v) (>= v 1))

     (match* ((t ll) (t lr) (t ur) (t ul))
       [(#f #f #f #f) #f]
       [(#t #t #t #t) #f]
       [(#t #f #f #f) (ret (bottom)   (left))]   ;  1
       [(#f #t #f #f) (ret (bottom)  (right))]   ;  2
       [(#t #t #f #f) (ret   (left)  (right))]   ;  3
       [(#f #f #t #f) (ret    (top)  (right))]   ;  4
       [(#t #f #t #f)
           (ret (top) (left))
           (ret (bottom) (right))]               ;  5
       [(#f #t #t #f)  (ret   (top) (bottom))]   ;  6
       [(#t #t #t #f)  (ret   (top)   (left))]   ;  7
       [(#f #f #f #t)  (ret   (top)   (left))]   ;  8
       [(#t #f #f #t)  (ret   (top) (bottom))]   ;  9
       [(#f #t #f #t)
           (ret    (top)  (left))
           (ret (bottom) (right))]               ; 10
       [(#t #t #f #t) (ret    (top) (right))]    ; 11
       [(#f #f #t #t) (ret   (left) (right))]    ; 12
       [(#t #f #t #t) (ret (bottom) (right))]    ; 13
       [(#f #t #t #t) (ret (bottom)  (left))]    ; 14
       )))

(define (draw-slimes s* dc step i0 i1 j0 j1)
  (send dc set-pen "green" 1 'solid)
  (define (line p0 p1)
    (match-define (posn x0 y0) p0)
    (match-define (posn x1 y1) p1)
    (send dc draw-line x0 y0 x1 y1))
  (slime-outline (slime-surface s*) line step i0 i1 j0 j1))

