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

(load-extension "./ts_sdl2")
(sdl2-init)

(load-extension "./ts_iso")

(load "event-handling.scm")
(add-event-handler sdl2-quit (lambda (event) #f))

(define (screen-to-map x y)
  (cons (/ (+ (/ x 32) (/ y 16)) 2)
        (/ (- (/ y 16) (/ x 32)) 2)))

(let* ((window (sdl2-create-window))
       (renderer (sdl2-create-renderer window))
       (rocks '((1 . 1)))
       (texture (sdl2-load-texture renderer
                 "/home/gmcnutt/Dropbox/projects/art/iso-64x64-outside.png"))
       )
  (define (clear-screen)
    (sdl2-set-render-draw-color renderer 255 255 255 sdl2-alpha-opaque)
    (sdl2-render-clear renderer))
  (define (render-iso-test off_x off_y map_w map_h tile_w tile_h)
    (let ((tile_w_half (/ tile_w 2))
          (tile_h_half (/ tile_h 2)))
      (define (screen-x mapx mapy)
        (+ off_x (* (- mapx mapy) tile_w_half)))
      (define (screen-y mapx mapy)
        (+ off_y (* (+ mapx mapy) tile_h_half)))
      (iso-fill renderer texture '(0 32 64 32) (list 0 0 map_w map_h))
      (for-each (lambda (loc)
                  (iso-blit renderer texture
                            (list (* 4 64) (* 7 64) 64 64) (car loc) (cdr loc)))
                rocks)
      (sdl2-set-render-draw-color renderer 64 32 64 sdl2-alpha-opaque)
      (iso-grid renderer map_w map_h)
      ))
  (define (render)
    (clear-screen)
    (render-iso-test 0 0 10 10 64 32)
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
                       (let ((loc (iso-screen-to-map x y)))
                         (println x "," y "->" loc)
                         (if (not (null? loc))
                             (set-cdr! rocks (cons loc (cdr rocks)))))
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

