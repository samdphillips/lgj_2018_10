#lang racket/base

(require racket/class
         racket/format
         racket/gui/base

         (only-in racket/list take))

(define view%
  (class canvas%
    (field [keys null]
           [prune-size 20])

    (define my-font
      (make-font #:size 10
                 #:family 'modern))

    (define (format-key k)
      (with-method ([key-code (k get-key-code)]
                    [key-release-code (k get-key-release-code)])
        (~a (~a #:width 10 (key-code))
            (key-release-code))))
    
    (define (do-paint c dc)
      (for ([k (in-list keys)]
            [y (in-naturals)])
        (send dc set-font my-font)
        (send dc draw-text (format-key k) 0 (* y 16))))
                    
    (define (prune-keys!)
      (when (> (length keys) prune-size)
        (set! keys (take keys prune-size))))
    
    (define/override (on-char k)
      (prune-keys!)
      (set! keys (cons k keys))
      (send this refresh))      
      
    (super-new [paint-callback do-paint])))

(let* ([w (new frame% [label "test"])]
       [c (new view% [parent w])])
  (send w show #t))
  