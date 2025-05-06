(define-module (ren-sexp update)
  #:use-module (hoot match)
  #:use-module (goblins)
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
  (define a (make-scene #:state 'a))
  (define b (make-scene #:state 'b))
  (match (list a b)
    ((($ <scene> state)
      ($ <scene> state))
     (pk (cons b a))))
  
  (pk (list next current))
  (match (cons next current)
    ((($ <scene> state* bg* old-text*
	 text* sprites* music* carret* ttl*)
      .
      ($ <scene> state  bg  old-text
	 text  sprites  music  carret  ttl))
     (cond
      ((not (same-bg? bg* bg))
       (pk 'next-bg)
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
  (let*-on ((next (<- state 'current-story-scene))
	    (completed? (<- state 'current-scene-completed?)))
	   (pk 8)
	   (if completed?
	       (if (inf-ttl? next)
		   scene
		   (append-empty-scene! state (make-scene)))
	       (next-scene-increment next scene))))

(define (init-update state dt)
  (pk 1)
  (pk ($ state 'current-scene))
  (pk 2)
  (define a-vat (spawn-vat))

  (define draw-callback (init-draw state))
  ;; (call-with-vat
  ;;  a-vat
  ;;  (lambda ()
     
  ;;    1))
  
  (define (update)
    (pk 3)
    (call-with-vat
     a-vat
     (lambda ()
       (let*-on ((scene (<- state 'current-scene))
		(_ (<- state 'update-current-scene
		       (compute-next-state state scene)))
		)
	       ;; (pk scene)
	       ;; (pk 4)
	       ;; (match (scene-state scene)
	       ;; 	 ('play ($ state 'update-current-scene
	       ;; 		   (compute-next-state scene state)))
		;; 	 (_ #t))
		(pk 4)
		(timeout update-callback dt)
		;; (request-animation-frame draw-callback)
		))))
  (define update-callback (procedure->external update))
  update-callback)
