(define-module (ren-sexp keyboard)
  #:use-module (ice-9 match)
  #:use-module (ice-9 atomic)
  #:use-module (dom event)
  #:use-module (dom document)
  #:use-module (hoot ffi)
  #:use-module (fibers channels)
  #:use-module (ren-sexp scene-utils)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp music)
  #:use-module (ren-sexp utils)
  #:export (add-key-up-listener!))

(define (complete-or-begin-new-scene! state-box)
  (let* ((state (atomic-box-ref state-box))
         (current (assoc-ref state 'current-scene))
	 (remote  (assoc-ref state 'current-story-scene)))
    (if (equal? current remote)
	(append-empty-scene! state-box current remote (make-scene))
	(atomic-update-current-scene state-box remote))))

(define (add-key-up-listener! state-box)
  (add-event-listener!
   (current-document)
   "keydown"
   (procedure->external
    (init-keyboard state-box))))

(define (init-keyboard state-box)
  (lambda (event)
    (let* ((key (string->symbol (keyboard-event-code event)))
	   ;; (_ (put-message in #f))
	   ;; (scene (get-message out))
	   )
      ;; (match (scene-state scene)
      ;; 	('play
      (match key
	;; ('Equal	(change-volume scene 0.05))
	;; ('Minus	(change-volume scene -0.05))
	;; ('KeyM	(mute-toggle scene))
	('Space	(complete-or-begin-new-scene! state-box))
	(_ #t)))))
