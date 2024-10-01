(define-module (ren-sexp keyboard)
  #:use-module (ice-9 match)
  #:use-module (dom event)
  #:use-module (dom document)
  #:use-module (hoot ffi)
  #:use-module (ren-sexp scene-utils)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp music)
  #:use-module (ren-sexp utils)
  #:export (add-key-up-listener!))

(define (complete-current-scene! local remote state)
  (find-replace local remote state))

(define (complete-or-begin-new-scene! data *state*)
  (let* ((state (*state*))
	 (scene (car state))
	 (local&current (local-and-remote-scene state data))
	 (local (car local&current))
	 (remote (cdr local&current)))
    (*state*
     (if (current-scene-completed? local remote)
	 (append-empty-scene! state data (make-scene))
	 (complete-current-scene! local remote state)))))

(define (add-key-up-listener! data *state*)
  (add-event-listener!
   (current-document)
   "keyup"
   (procedure->external
    (init-keyboard data *state*))))

(define (init-keyboard data *state*)
  (lambda (event)
    (let* ((key (string->symbol (keyboard-event-code event)))
	   (scene (car (*state*))))
      (pk key)
      (match (scene-state scene)
	('play
	 (match key
	   ('Equal	(change-volume scene 0.05))
	   ('Minus	(change-volume scene -0.05))
	   ('KeyM	(mute-toggle scene))
	   ('Space	(complete-or-begin-new-scene! data *state*))
	   (_ #t)))
	(_ #t)))))

