(define-module (ren-sexp keyboard)
  #:use-module (ice-9 match)
  #:use-module (dom event)
  #:use-module (dom document)
  #:use-module (hoot ffi)
  #:use-module (fibers channels)
  #:use-module (ren-sexp scene-utils)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp music)
  #:use-module (ren-sexp utils)
  #:export (add-key-up-listener!))

(define (complete-current-scene! local remote state)
  (find-replace local remote state))

(define (complete-or-begin-new-scene! data in out)
  (let* ((_ (put-message in #f))
	 (state (get-message out))
	 (scene (car state))
	 (local&current (local-and-remote-scene state data))
	 (local (car local&current))
	 (remote (cdr local&current)))
    (put-message
     in
     (if (current-scene-completed? local remote)
	 (append-empty-scene! state data (make-scene))
	 (complete-current-scene! local remote state)))))

(define (add-key-up-listener! data in out)
  (add-event-listener!
   (current-document)
   "keydown"
   (procedure->external
    (init-keyboard data in out))))

(define (init-keyboard data in out)
  (lambda (event)
    (let* ((key (string->symbol (keyboard-event-code event)))
	   (_ (put-message in #f))
	   (state (get-message out))
	   (scene (car state)))
      (pk key)
      (match (scene-state scene)
	('play
	 (match key
	   ('Equal	(change-volume scene 0.05))
	   ('Minus	(change-volume scene -0.05))
	   ('KeyM	(mute-toggle scene))
	   ('Space	(complete-or-begin-new-scene! data in out))
	   (_ #t)))
	(_ #t)))))

