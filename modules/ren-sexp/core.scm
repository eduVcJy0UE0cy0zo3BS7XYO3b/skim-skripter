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

(define (next-scene-increment src dst)
  (cond
   ((not (equal? (scene-bg src) (scene-bg dst)))
    (next-bg src dst))
   ((not (equal? (scene-sprites src) (scene-sprites dst)))
    (next-sprites src dst))
   ((not (equal? (scene-text src) (scene-text dst)))
    (next-string src dst))
   (else src)))

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

(define (compute-next-state state data)
  (let* ((local&current (local-and-remote-scene state data))
	 (local (car local&current))
	 (remote (cdr local&current)))
    (if (current-scene-completed? local remote)
	state
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
  (define carret-context (make-2d-context "carret-canvas"))
  (define context (make-2d-context "all-canvas"))
  (define text-context (make-2d-context "text-canvas"))
  (define game-width    1920.0)
  (define game-height   1080.0)
  (set-font carret-context)
  (set-font text-context)
  
  (define (draw prev-time)
    (define current-state (*state*))
    (define scene (last current-state))
    (let ((text (scene-text scene))
          (bg (scene-bg scene))
	  (sprites (scene-sprites scene))
	  (completed? (current-state-completed? current-state data)))
      
      (draw-bg bg context game-width game-height)
      (draw-sprites sprites context)
      (clear-rect text-context 0.0 0.0 game-width game-height)
      (clear-rect carret-context 0.0 0.0 game-width game-height)
      (let* ((p&w (draw-text text
			     text-context game-width game-height))
	     (p (car p&w))
	     (w (cdr p&w)))
	(unless (equal? text "")
          (draw-carret (make-carret "")
		       carret-context p w completed?))))
    (request-animation-frame draw-callback))
  (define draw-callback (procedure->external draw))
  draw-callback)

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
