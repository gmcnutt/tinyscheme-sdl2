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
                 "/home/gmcnutt/Dropbox/projects/art/iso-64x64-outside.png"))
       )
  (define (clear-screen)
    (sdl2-set-render-draw-color renderer 255 255 255 sdl2-alpha-opaque)
    (sdl2-render-clear renderer))
  (define (draw-square-grid off_x off_y w h tile_w tile_h)
    (sdl2-set-render-draw-color renderer 128 128 255 sdl2-alpha-opaque)
    (define (screen-x mapx) (+ off_x (* mapx tile_w)))
    (define (screen-y mapy) (+ off_y (* mapy tile_h)))
    (define (draw-rows n)
      (sdl2-render-draw-line renderer
                             (screen-x 0) (screen-y n)
                             (screen-x w) (screen-y n))
      (if (> n 0) (draw-rows (- n 1))))
    (define (draw-cols n)
      (sdl2-render-draw-line renderer
                             (screen-x n) (screen-y 0)
                             (screen-x n) (screen-y h))
      (if (> n 0) (draw-cols (- n 1))))
    (draw-rows h)
    (draw-cols w)
    )
  (define (render-iso-test off_x off_y w h tile_w tile_h)
    (define (screen-x mapx mapy)
      (+ off_x (* w (/ tile_w 2)) (* (- mapx mapy) (/ tile_w 2))))
    (define (screen-y mapx mapy)
      (+ off_y (* (+ mapx mapy) (/ tile_h 2))))
    (define (draw-iso-grid)
      (sdl2-set-render-draw-color renderer 128 255 128 sdl2-alpha-opaque)
      (define (draw-rows n)
        (sdl2-render-draw-line renderer
                               (screen-x 0 n) (screen-y 0 n)
                               (screen-x w n) (screen-y w n))
        (if (> n 0) (draw-rows (- n 1))))
      (define (draw-cols n)
        (sdl2-render-draw-line renderer
                               (screen-x n 0) (screen-y n 0)
                               (screen-x n h) (screen-y n h))
        (if (> n 0) (draw-cols (- n 1))))
      (draw-rows h)
      (draw-cols w)
      )
    (sdl2-render-copy renderer texture '(0 32 64 32)
                      (list (- (screen-x 0 0) (/ tile_w 2))
                            (screen-y 0 0) 64 32))
    (draw-iso-grid)
  )
  (define (render)
    (clear-screen)
    (render-iso-test 0 0 3 3 64 32)
    (sdl2-render-present renderer)
    )
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

