;; Macro to emulate python's context managers. Turns this:
;;   (with x ctor dtor ...)
;; Into this:
;;   (let ((x (ctor))) ... (dtor x))
(define-macro (with v ctor dtor . form)
  `(let ((,v ,ctor)) ,@form (,dtor ,v)))

(define (newline)
  (display "\n"))

(define (println . args)
  (map display args)
  (newline))

(define nil '())

(define (for-each proc lst)
  (cond ((not (null? lst))
         (proc (car lst))
         (for-each proc (cdr lst)))))

(load-extension "./sdl2")
(sdl2-init)

(load "event-handling.scm")
(add-event-handler sdl2-quit (lambda (event) #f))

(let* ((window (sdl2-create-window))
       (renderer (sdl2-create-renderer window))
       (lines '((0 0 100 100)))
       (texture (sdl2-load-texture renderer
                 "/home/gmcnutt/Dropbox/projects/art/u6_tileset.png"))
       )
  (define (clear-screen)
    (sdl2-set-render-draw-color renderer 255 255 255 sdl2-alpha-opaque)
    (sdl2-render-clear renderer))
  (define (render)
    (clear-screen)
    (sdl2-render-copy renderer texture '(400 880 16 16) '(0 0 16 16))
    (sdl2-set-render-draw-color renderer 128 128 255 sdl2-alpha-opaque)
    (for-each (lambda (line)
                (apply sdl2-render-draw-line (cons renderer line)))
              lines)
    (sdl2-render-present renderer))
  (define (loop frames event)
    (render)
    (cond ((not (handle-event event)) frames)
          (else
           (loop (+ frames 1)
                 (sdl2-poll-event)))))

  (add-event-handler sdl2-mouse-button-down
                     (lambda (event x y)
                       (set-cdr! lines (cons (list 0 0 x y) (cdr lines)))
                       #t))

  ;; Start the main loop and time the FPS.
  (let ((start (sdl2-get-ticks))
        (frames (loop 0 (sdl2-poll-event)))
        (stop (sdl2-get-ticks)))
    (println (/ (* frames 1000) (- stop start)) " FPS"))

  (sdl2-destroy-texture texture)
  (sdl2-destroy-renderer renderer)
  (sdl2-destroy-window window)
  )

