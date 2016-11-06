;; Event handling.
;;
;; Assumes (load-extension "sdl2") has been done.

(define *event-handlers* '())

(define (add-event-handler event handler)
  (set! *event-handlers* (cons (cons event handler) *event-handlers*)))

(define (lookup-event-handler event handlers)
  (cond ((null? handlers) '())
        ((eqv? (caar handlers) event) (cdar handlers))
        (else (lookup-event-handler event (cdr handlers)))))

(define (handle-event event)
  (if (null? event)
      #t
      (let ((handler (lookup-event-handler event *event-handlers*)))
        (if (null? handler)
            #t
            (handler event)))))
