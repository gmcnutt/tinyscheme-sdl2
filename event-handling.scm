;; Event handling.
;;
;; Assumes (load-extension "sdl2") has been done.

(define *event-handlers* '())

(define (add-event-handler event handler)
  (set! *event-handlers* (cons (cons event handler) *event-handlers*)))

(define (lookup-event-handler event handlers)
  (cond ((null? handlers) '())
        ((eqv? (caar handlers) (car event)) (cdar handlers))
        (else (lookup-event-handler event (cdr handlers)))))

(define (handle-event event)
  (cond ((null? event) #t)
        (else
         (let ((handler (lookup-event-handler event *event-handlers*)))
           (cond ((null? handler) #t)
                 (else
                  (apply handler event)))))))
