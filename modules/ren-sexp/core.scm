(define-module (ren-sexp core)
  #:pure
  #:use-module (scheme base)
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
  #:use-module (hoot ffi)
  #:use-module (ren-sexp utils)
  #:export (next-scene-increment
	    compute-next-state
	    black-screen
	    local-and-remote-scene
	    current-scene-completed?
	    append-empty-scene!
	    complete-current-scene!
	    init))

(define (next-scene-increment src dst)
  (cond
   ((not (equal? (scene-bg src) (scene-bg dst))) (next-bg src dst))
   ((not (equal? (scene-sprites src) (scene-sprites dst))) (next-sprites src dst))
   ((not (equal? (scene-text src) (scene-text dst))) (next-string src dst))
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

;; (define (image:black)
;;   (make-image "resources/bg/black.png"))

(define (black-screen)
  (%make-bg (make-image "resources/bg/black.png") 1000))

(define (empty-scene1)
  (make-scene 'play (%make-bg (make-image "resources/bg/black.png") 0)
	      "" (list) #f ""))

(define (init data)
  (define empty-scene (empty-scene1))
  (init-settings!)
  (define *state* (make-parameter (list empty-scene)))
  (define cursor-canvas (get-element-by-id "cursor-canvas"))
  (define cursor-context (get-context cursor-canvas "2d"))
  (define cursor-width    1920.0)
  (define cursor-height   1080.0)
  (set-element-width! cursor-canvas (exact cursor-width))
  (set-element-height! cursor-canvas (exact cursor-height))
  
  (define canvas (get-element-by-id "all-canvas"))
  (define context (get-context canvas "2d"))

  (define text-canvas (get-element-by-id "text-canvas"))
  (define text-context (get-context text-canvas "2d"))

  (define game-width    1920.0)
  (define game-height   1080.0)
  (define key:space "Space")
  (define key:mute-toggle "KeyM")
  (define key:decrease-volume "Minus")
  (define key:increase-volume "Equal")

  (define dt (/ 1000.0 60.0))

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
      (clear-rect cursor-context 0.0 0.0 game-width game-height)
      (let* ((p&w (draw-text text text-context game-width game-height))
	     (p (car p&w))
	     (w (cdr p&w)))
	(unless (equal? text "")
         (draw-carret (make-carret "") cursor-context p w completed?))))
    
    (request-animation-frame draw-callback))
  (define draw-callback (procedure->external draw))
  
  (define (on-key-up data)
    (lambda (event)
      (let* ((key (keyboard-event-code event))
	     (state (*state*))
	     (scene (last state))
	     (local&current (local-and-remote-scene state data))
	     (local (car local&current))
	     (remote (cdr local&current)))
	(pk key)
	(match (scene-state scene)
	  ('play
	   (when (equal? key key:increase-volume)
	     (change-volume scene 0.05))
	   (when (equal? key key:decrease-volume)
	     (change-volume scene -0.05))
	   (when (equal? key key:mute-toggle)
	     (pk 'music-toggle)
	     (mute-toggle scene))
	   (when (equal? key key:space)
	     (*state* (if (current-scene-completed? local remote)
			  (append-empty-scene! state data empty-scene)
			  (complete-current-scene! local remote state)))))
	  (_ #t)))))
  
  (define (update)
    (define state (*state*))
    (define scene (last (*state*)))
    (match (scene-state scene)
      ('play (*state* (compute-next-state state data)))
      (_ #t))
    (timeout update-callback dt))
  (define update-callback (procedure->external update))
  
  (set-element-width! canvas (exact game-width))
  (set-element-height! canvas (exact game-height))

  (set-element-width! text-canvas (exact game-width))
  (set-element-height! text-canvas (exact game-height))

  (define Prime (download-font!
		 "Prime"
		 "url(resources/fonts/courierprime.otf/courier-prime.otf)"))

  (add-event-listener! (current-document) "keyup"
                       (procedure->external (on-key-up data)))

  (define (init-call font)
    (add-font! font)
    (request-animation-frame draw-callback)
    (timeout update-callback dt)
    (set-fill-color! text-context "#ffffff")
    (set-border-color! text-context "black")
    (set-font! text-context "bold 40px Prime")
    (set-text-align! text-context "left")
    (set-shadow-blur! text-context 10)
    (set-shadow-color! text-context "rgba(0,0,0,0.3)")

    (set-fill-color! cursor-context "#ffffff")
    (set-border-color! cursor-context "black")
    (set-font! cursor-context "bold 40px Prime")
    (set-text-align! cursor-context "left")
    (set-shadow-blur! cursor-context 10)
    (set-shadow-color! cursor-context "rgba(0,0,0,0.3)"))
  
  (then (load-font Prime)
	(procedure->external init-call)))
