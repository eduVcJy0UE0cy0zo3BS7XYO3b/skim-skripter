(define-module (ren-sexp core)
  #:use-module (dom media)
  #:use-module (dom element)
  #:use-module (dom document)
  #:use-module (dom window)
  #:use-module (dom image)
  #:use-module (hoot ffi)
  #:use-module (fibers)
  #:use-module (repl-environment)
  #:use-module (fibers channels)
  #:use-module (ice-9 match)
  #:use-module (ice-9 atomic)
  #:use-module (ren-sexp settings)
  #:use-module (ren-sexp utils)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp bg)
  #:use-module (ren-sexp sprites)
  #:use-module (ren-sexp keyboard)
  #:use-module (ren-sexp save-system)
  #:use-module (ren-sexp update)
  #:export (black-screen init init-main-menu))

(define (black-screen)
  (%make-bg (make-image "resources/bg/black.png") 1000 "resources/bg/black.png"))

(define (prime-font)
  (download-font!
   "Prime"
   "url(resources/fonts/courierprime.otf/courier-prime.otf)"))

(define (ptsans-font)
  (download-font!
   "PTSans"
   "url(resources/fonts/PT_Sans/PTSans-Regular.ttf)"))

(define (init data)
  (init-settings!)
  (set-data! data)
  (define state-box
    (make-atomic-box `((current-scene . ,(make-scene))
                       (current-story-scene . ,(car data))
                       (counter . 0))))
  (define update-and-draw-callback (init-update state-box))
  (add-key-up-listener! state-box)
  (add-click-listener! state-box)
  (then (load-font (ptsans-font))
	(procedure->external
	 (lambda (font)
	   (add-font! font)
	   (request-animation-frame update-and-draw-callback)))))

(define (init-main-menu data)
  (init-settings!)
  (set-game-script! data) ; Сохраняем скрипт для использования позже
  (set-data! data) ; Устанавливаем DATA для корректной работы
  (define state-box
    (make-atomic-box `((current-scene . ,(make-scene))
                       (current-story-scene . ,(make-scene))
                       (counter . 0))))
  (define update-and-draw-callback (init-update state-box))
  (add-key-up-listener! state-box)
  (add-click-listener! state-box)
  (then (load-font (ptsans-font))
	(procedure->external
	 (lambda (font)
	   (add-font! font)
	   (request-animation-frame update-and-draw-callback)))))
