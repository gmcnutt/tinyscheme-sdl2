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

(load-extension "./sdl2")
(sdl2-init)

(with window (sdl2-create-window) sdl2-destroy-window
      (with renderer (sdl2-create-renderer window) sdl2-destroy-renderer
            (define (render)
              (sdl2-set-render-draw-color renderer 255 255 255 sdl2-alpha-opaque)
              (sdl2-render-clear renderer)
              (sdl2-set-render-draw-color renderer 128 128 255 sdl2-alpha-opaque)
              (sdl2-render-draw-line renderer 0 0 100 100)
              (sdl2-render-present renderer))
            (define (loop frames event)
              (render)
              (cond ((equal? event sdl2-quit) frames)
                    (else (loop (+ frames 1)
                                (sdl2-poll-event)))))
            (let ((start (sdl2-get-ticks))
                  (frames (loop 0 (sdl2-poll-event)))
                  (stop (sdl2-get-ticks)))
              (println "start:" start)
              (println "stop:" stop)
              (println "frames:" frames)
              (println (/ (* frames 1000) (- stop start)) " FPS"))
	    )
      )
