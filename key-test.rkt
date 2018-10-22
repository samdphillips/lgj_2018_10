#lang racket/base

(require racket/class
         racket/format
         racket/gui/base

         (only-in racket/list take))

(define view%
  (class canvas%
    (field [log null]
           [keys (make-hasheq)]
           [prune-size 20])

    (define my-font
      (make-font #:size 10
                 #:family 'modern))

    (define (format-key k)
      (with-method ([key-code (k get-key-code)]
                    [key-release-code (k get-key-release-code)])
        (~a (~a #:width 10 (key-code))
            (key-release-code))))

    (define (log! msg)
      (prune-log!)
      (set! log (cons msg log)))

    (define (do-paint c dc)
      (send dc set-font my-font)
      (for ([msg (in-list log)]
            [y (in-naturals)])
        (send dc draw-text msg 0 (* y 16))))

    (define (prune-log!)
      (when (> (length log) prune-size)
        (set! log (take log prune-size))))

    (define (key-active? k)
      (hash-has-key? keys (send k get-key-code)))

    (define (deactivate-key! k)
      (let ([kc (send k get-key-release-code)])
        (hash-remove! keys kc)
        (log! (~a kc " deactivated"))))

    (define (activate-key! k)
      (unless (key-active? k)
        (let ([kc (send k get-key-code)]
              [ts (send k get-time-stamp)])
          (hash-set! keys kc ts)
          (log! (~a kc " activated")))))

    (define (dispatch-keys)
      (define k*
        (sort
         (for/list ([(k v) (in-hash keys)]) (cons v k))
         < #:key car))
      (for ([k (in-list k*)])
        (log! (~a (cdr k) " dispatched"))))

    (define/override (on-char k)
      (log! (format-key k))
      (if (eq? 'release (send k get-key-code))
          (deactivate-key! k)
          (activate-key! k))
      (send this refresh))

    (define update-timer
      (new timer%
           [notify-callback dispatch-keys]
           [interval 16]))

    (super-new [paint-callback do-paint])))

(let* ([w (new frame% [label "test"] [width 500] [height (* 20 16)])]
       [c (new view% [parent w])])
  (send w show #t))
