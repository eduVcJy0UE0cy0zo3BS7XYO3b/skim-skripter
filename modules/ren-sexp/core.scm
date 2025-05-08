(define-module (ren-sexp core)
  #:use-module (dom media)
  #:use-module (goblins)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (dom element)
  #:use-module (dom document)
  #:use-module (dom window)
  #:use-module (dom image)
  #:use-module (hoot ffi)
  #:use-module (fibers)
  #:use-module (repl-environment)
  #:use-module (fibers channels)
  #:use-module (ice-9 match)
  #:use-module (ren-sexp settings)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp bg)
  #:use-module (ren-sexp sprites)
  #:use-module (ren-sexp keyboard)
  #:use-module (ren-sexp update)
  #:export (black-screen init))

(define (black-screen)
  (%make-bg (make-image "resources/bg/black.png") 1000))

(define (prime-font)
  (download-font!
   "Prime"
   "url(resources/fonts/courierprime.otf/courier-prime.otf)"))

(define (ptsans-font)
  (download-font!
   "PTSans"
   "url(resources/fonts/PT_Sans/PTSans-Regular.ttf)"))

(define (init data VAT)
  (init-settings!)
  (define dt (/ 1000.0 60.0))  
  (define empty-scene (make-scene))
  (define* (^state bcom)
    (define scene-history (spawn ^cell (list empty-scene)))
    (define story-list (spawn ^cell data))
    (define current-story-scene (spawn ^cell (car ($ story-list))))
    (define current-history-scene (spawn ^cell (car ($ scene-history))))
    (methods
     ((current-scene)
      ($ current-history-scene))
     ((current-story-scene)
      ($ current-story-scene))
     ((append-empty-scene)
      ($ scene-history
	 (cons empty-scene ($ scene-history))))
     ((current-scene-completed?)
      (let ((current-scene ($ current-history-scene))
	    (next-scene ($ current-story-scene)))
	(equal? current-scene next-scene)))
     ((update-current-scene updated-scene)
      ($ current-history-scene updated-scene)
      1)))
  
  (define state (spawn ^state))
  ;; (add-key-up-listener! state)
  (define update-callback
    (init-update state dt VAT))
  (then (load-font (ptsans-font))
	(procedure->external
	 (lambda (font)
	   (add-font! font)
	   (timeout update-callback dt)))))
