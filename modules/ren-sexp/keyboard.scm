(define-module (ren-sexp keyboard)
  #:use-module (ice-9 match)
  #:use-module (goblins)
  #:use-module (dom event)
  #:use-module (dom document)
  #:use-module (hoot ffi)
  #:use-module (fibers channels)
  #:use-module (ren-sexp scene-utils)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp music)
  #:use-module (ren-sexp utils)
  #:export (add-key-up-listener!))

(define (complete-or-begin-new-scene! ^state)
  (let* ((current ($ ^state 'current-scene))
	 (remote ($ ^state 'current-story-scene))
	 (completed? ($ ^state 'current-scene-completed?)))
    (if completed?
	(append-empty-scene! ^state (make-scene))
	remote)))

(define (add-key-up-listener! ^state)
  (add-event-listener!
   (current-document)
   "keydown"
   (procedure->external
    (init-keyboard ^state))))

(define (init-keyboard ^state)
  (lambda (event)
    (let* ((key (string->symbol (keyboard-event-code event)))
	   ;; (_ (put-message in #f))
	   ;; (scene (get-message out))
	   )
      (pk key)
      ;; (match (scene-state scene)
      ;; 	('play
      (match key
	;; ('Equal	(change-volume scene 0.05))
	;; ('Minus	(change-volume scene -0.05))
	;; ('KeyM	(mute-toggle scene))
	('Space	(complete-or-begin-new-scene! ^state))
	(_ #t)))))					

