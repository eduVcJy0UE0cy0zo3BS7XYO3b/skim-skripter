(define-module (ren-sexp core)
  #:use-module (dom media)
  #:use-module (dom element)
  #:use-module (dom document)
  #:use-module (dom window)
  #:use-module (dom image)
  #:use-module (hoot ffi)
  #:use-module (ren-sexp settings)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp bg)
  #:use-module (ren-sexp sprites)
  #:use-module (ren-sexp keyboard)
  #:use-module (ren-sexp draw)
  #:use-module (ren-sexp update)
  #:export (black-screen init))

(define (black-screen)
  (%make-bg (make-image "resources/bg/black.png") 1000))

(define (prime-font)
  (download-font!
   "Prime"
   "url(resources/fonts/courierprime.otf/courier-prime.otf)"))

(define (init data)
  (init-settings!)
  
  (define dt (/ 1000.0 60.0))
  (define *state* (make-parameter (list (make-scene))))
  
  (add-key-up-listener! data *state*)
  (define update-callback (init-update data *state* dt))
  (define draw-callback (init-draw data *state*))
  
  (then (load-font (prime-font))
	(procedure->external
	 (lambda (font)
	   (add-font! font)
	   (request-animation-frame draw-callback)
	   (timeout update-callback dt)))))
