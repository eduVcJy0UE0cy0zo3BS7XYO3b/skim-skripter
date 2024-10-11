(define-module (ren-sexp core)
  #:use-module (dom media)
  #:use-module (dom element)
  #:use-module (dom document)
  #:use-module (dom window)
  #:use-module (dom image)
  #:use-module (hoot ffi)
  #:use-module (fibers)
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

(define (init data)
  (init-settings!)
  
  (define dt (/ 1000.0 60.0))
  (define *state* (make-parameter (list (make-scene))))

  (define (make-box in out default)
  (let lp ((old default))
    (match (get-message in)
      (#f (put-message out old))
      (data (lp data)))
    (lp old)))
  
  (define state-in (make-channel))
  (define state-out (make-channel))
  (spawn-fiber (lambda ()
		 (make-box state-in state-out (list (make-scene)))))
  
  (add-key-up-listener! data *state*)
  (define update-callback (init-update data *state* dt))
  
  (then (load-font (ptsans-font))
	(procedure->external
	 (lambda (font)
	   (add-font! font)
	   (timeout update-callback dt)))))
