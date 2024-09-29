(define-module (ren-sexp core)
  #:use-module (ice-9 match)
  #:use-module (dom document)
  #:use-module (dom canvas)
  #:use-module (dom window)
  #:use-module (dom event)
  #:use-module (dom element)
  #:use-module (dom media)
  #:use-module (dom image)
  #:use-module (dom storage)
  #:use-module (ren-sexp text)
  #:use-module (ren-sexp bg)
  #:use-module (ren-sexp sprites)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp music)
  #:use-module (ren-sexp carret)
  #:use-module (hoot debug)
  #:use-module (hoot numbers)
  #:use-module (hoot ffi)
  #:use-module (ren-sexp utils)
  #:export (black-screen
	    init))

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

(define (local-and-remote-scene state data)
  (define local/current-scene (last state))
  (define local/current-scene-id (- (length state) 1))
  (define remote/current-scene (list-ref data local/current-scene-id))
  (cons local/current-scene remote/current-scene))

(define (current-scene-completed? local remote)
  (equal? remote local))

(define (get-next-increment local remote state)
  (define local* (next-scene-increment remote local))
  (define state* (find-replace local local* state))
  state*)

(define (current-state-completed? state data)
  (let* ((local&current (local-and-remote-scene state data))
	 (local (car local&current))
	 (remote (cdr local&current)))
    (current-scene-completed? local remote)))

(define (inf-ttl? remote)
  (equal? (scene-ttl remote) 'inf))

(define (compute-next-state state data)
  (let* ((local&current (local-and-remote-scene state data))
	 (local (car local&current))
	 (remote (cdr local&current)))
    (if (current-scene-completed? local remote)
	(if (inf-ttl? remote)
	    state
	    (append-empty-scene! (*state*) data (make-scene)))
	(get-next-increment local remote state))))

(define (append-empty-scene! state data empty-scene)
  (unless (eq? (length state)
	       (length data))
    (let* ((curr-scene (last state))
	   (curr-scene-id+1 (length state))
	   (next-scene (list-ref data curr-scene-id+1))
	   
	   (curr-bg (scene-bg curr-scene))
	   (next-bg (scene-bg next-scene))

	   (curr-sprites (scene-sprites curr-scene))
	   (next-sprites (scene-sprites next-scene))

	   (curr-music (scene-music curr-scene))
	   (next-music (scene-music next-scene)))
      
      (define next-scene1 (include-bg empty-scene curr-bg next-bg))
      (define next-scene2 (include-sprites next-scene1 curr-sprites next-sprites))
      (define next-scene3 (include-music next-scene2 curr-music next-music))
      (append state (list next-scene3)))))

(define (complete-current-scene! local remote state)
  (find-replace local remote state))

(define (init-settings!)
  (let ((volume (get-item "volume"))
	(is-mute (get-item "is-mute")))
    (or volume (set-item! "volume" "1.0"))
    (or is-mute (set-item! "is-mute" "#f"))))

(define (black-screen)
  (%make-bg (make-image "resources/bg/black.png") 1000))

(define dt (/ 1000.0 60.0))
(define *state* (make-parameter (list (make-scene))))

(define (complete-or-begin-new-scene! data)
  (let* ((state (*state*))
	 (scene (last state))
	 (local&current (local-and-remote-scene state data))
	 (local (car local&current))
	 (remote (cdr local&current)))
    (*state*
     (if (current-scene-completed? local remote)
	 (append-empty-scene! state data (make-scene))
	 (complete-current-scene! local remote state)))))

(define (init-keyboard data)
  (define (on-key-up)
    (lambda (event)
      (let* ((key (string->symbol (keyboard-event-code event)))
	     (scene (last (*state*))))
	(pk key)
	(match (scene-state scene)
	  ('play
	   (match key
	     ('Equal	(change-volume scene 0.05))
	     ('Minus	(change-volume scene -0.05))
	     ('KeyM	(mute-toggle scene))
	     ('Space	(complete-or-begin-new-scene! data))
	     (_ #t)))
	  (_ #t)))))
  (on-key-up))

(define (init-update data)
  (define (update)
    (define state (*state*))
    (define scene (last (*state*)))
    (match (scene-state scene)
      ('play (*state* (compute-next-state state data)))
      (_ #t))
    
    (timeout update-callback dt))
  (define update-callback
    (procedure->external update))
  update-callback)

(define (init-draw data)
  (let ((carret-context (make-2d-context "carret-canvas"))
	(bg-context (make-2d-context "all-canvas"))
	(text-context (make-2d-context "text-canvas"))
	(old-text-context (make-2d-context "old-text-canvas"))
	(GW   1920.0)
	(GH   1080.0))
    (set-font carret-context)
    (set-font text-context)
    (set-font old-text-context)
    (define *last-pos* (make-parameter 0))
    
    (define (draw prev-time)
      (let* ((current-state (*state*))
	     (scene (last current-state))
	     (text (scene-text scene))
	     (old-text (scene-old-text scene))
             (bg (scene-bg scene))
	     (sprites (scene-sprites scene))
	     (completed?
	      (current-state-completed? current-state data)))
	(unless completed?
	  (draw-bg bg bg-context GW GH)
	  (draw-sprites sprites bg-context)
	  (clear-rect old-text-context 0.0 0.0 GW GH)
	  (define p1
	    (draw-old-text (reverse old-text) old-text-context 50.0))
	  (*last-pos* p1))
	
	(clear-rect text-context 0.0 0.0 GW GH)
	(clear-rect carret-context 0.0 0.0 GW GH)
	(let* ((p2&w2 (draw-text text text-context (*last-pos*)))
	       (p2 (car p2&w2))
	       (w2 (cdr p2&w2)))
	  (unless (equal? text "")
            (draw-carret (make-carret "")
			 carret-context p2 w2 completed?)))
	(request-animation-frame draw-callback)))
    (define draw-callback (procedure->external draw))
    draw-callback))

(define (make-2d-context elem)
  (define canvas (get-element-by-id elem))
  (define context (get-context canvas "2d"))
  (define width    1920.0)
  (define height   1080.0)
  (set-element-width! canvas (exact width))
  (set-element-height! canvas (exact height))
  context)

(define (set-font context)
  (set-fill-color! context "#ffffff")
  (set-border-color! context "black")
  (set-font! context "bold 40px Prime")
  (set-text-align! context "left")
  (set-shadow-blur! context 10)
  (set-shadow-color! context "rgba(0,0,0,0.3)"))

(define (add-key-up-listener! data)
  (add-event-listener!
   (current-document)
   "keyup"
   (procedure->external
    (init-keyboard data))))

(define (prime-font)
  (download-font!
   "Prime"
   "url(resources/fonts/courierprime.otf/courier-prime.otf)"))

(define (init data)
  (init-settings!)
  
  (add-key-up-listener! data)
  (define update-callback (init-update data))
  (define draw-callback (init-draw data))
  
  (then (load-font (prime-font))
	(procedure->external
	 (lambda (font)
	   (add-font! font)
	   (request-animation-frame draw-callback)
	   (timeout update-callback dt)))))
