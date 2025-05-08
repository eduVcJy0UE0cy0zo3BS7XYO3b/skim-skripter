(define-module (ren-sexp update)
  #:use-module (ice-9 match)
  #:use-module ((goblins) #:select ($ <- spawn-vat call-with-vat spawn) #:prefix g:)
  #:use-module (goblins actor-lib let-on)
  #:use-module (dom window)
  #:use-module (ren-sexp text)
  #:use-module (ren-sexp bg)
  #:use-module (ren-sexp sprites)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp carret)
  #:use-module (ren-sexp scene-utils)
  #:use-module (hoot ffi)
  #:use-module (fibers channels)
  #:use-module (ren-sexp draw)
  #:use-module (ren-sexp utils)
  #:export (init-update))

(define (inf-ttl? scene)
  (equal? (scene-ttl scene) 'inf))

(define (next-scene-increment next current)
  (match (list next current)
    ((($ <scene> state* bg* old-text*
	 text* sprites* music* carret* ttl*)
      ($ <scene> state  bg  old-text
	 text  sprites  music  carret  ttl))
     (cond
      ((not (same-bg? bg* bg))
       (next-bg next current))
      ((not (same-sprites? sprites* sprites))
       (next-sprites next current))
      ((not (same-old-text? old-text* old-text))
       (next-old-text next current))
      ((not (same-text? text* text))
       (next-string next current))
      ((and (number? ttl) (equal? ttl* (+ 1 ttl)))
       next)
      ((not (equal? ttl ttl*))
       (next-ttl next current))
      (else next)))))

(define (compute-next-state state scene)
  (let*-on ((next (g:<- state 'current-story-scene))
	    (completed? (g:<- state 'current-scene-completed?)))
	   (if completed?
	       (if (inf-ttl? next)
		   scene
		   (append-empty-scene! state (make-scene)))
	       (next-scene-increment next scene))))

(define (init-update state dt VAT)
  (pk 1)
  (pk (g:$ state 'current-scene))
  (pk 2)

  (define draw-callback (init-draw state VAT))
  ;; (call-with-vat
  ;;  a-vat
  ;;  (lambda ()
     
  ;;    1))
  
  (define (update)
    (g:call-with-vat
     VAT
     (lambda ()
       (let*-on ((scene (g:<- state 'current-scene))
		 (resul (g:<- state 'update-current-scene (compute-next-state state scene)))
		 )
		;; (pk scene)
		;; (pk 4)
		;; (match (scene-state scene)
		;; 	 ('play ($ state 'update-current-scene
		;; 		   (compute-next-state scene state)))
		;; 	 (_ #t))
		(request-animation-frame draw-callback)
		(timeout update-callback dt)
		))))
  (define update-callback (procedure->external update))
  update-callback)
