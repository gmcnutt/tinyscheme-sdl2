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


(load-extension "./sdl2")
(sdl2-init)

(with window (sdl2-create-window) sdl2-destroy-window
      (println "window:" window)
      (with renderer (sdl2-create-renderer window) sdl2-destroy-renderer
	    (println "renderer:" renderer)
	    )
      )
