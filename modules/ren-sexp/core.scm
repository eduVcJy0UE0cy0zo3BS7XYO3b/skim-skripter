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
  #:use-module (ren-sexp text)
  #:use-module (ren-sexp bg)
  #:use-module (ren-sexp sprites)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp music)
  #:use-module (hoot debug)
  #:use-module (hoot ffi)
  #:use-module (ren-sexp utils)
  #:export (next-scene-increment
	    compute-next-state
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
  
(define (compute-next-state state data)
  (let* ((local&current (local-and-remote-scene state data))
	 (local (car local&current))
	 (remote (cdr local&current)))
    (if (current-scene-completed? local remote)
	state
	(get-next-increment local remote state))))

(define (append-empty-scene! state data init-scene)
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
      
      (define next-scene1 (include-bg init-scene curr-bg next-bg))      
      (define next-scene2 (include-sprites next-scene1 curr-sprites next-sprites))
      (define next-scene3 (include-music next-scene2 curr-music next-music))
      (append state (list next-scene3)))))

(define (complete-current-scene! local remote state)
  (find-replace local remote state))

(define (init data init-scene)
  (define *state* (make-parameter (list init-scene)))
  (define canvas (get-element-by-id "all-canvas"))
  (define context (get-context canvas "2d"))

  (define text-canvas (get-element-by-id "text-canvas"))
  (define text-context (get-context text-canvas "2d"))

  (define game-width    1920.0)
  (define game-height   1080.0)
  (define key:space "Space")
  (define key:mute-toggle "KeyM")

  (define dt (/ 1000.0 60.0))

  (define (draw prev-time)
    (define scene (last (*state*)))
    (let ((text (scene-text scene))
          (bg (scene-bg scene))
	  (sprites (scene-sprites scene)))
      
      (draw-bg bg context game-width game-height)
      (draw-sprites sprites context)
      (clear-rect text-context 0.0 0.0 game-width game-height)
      (draw-text text text-context))
    
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
	   (when (equal? key key:mute-toggle)
	     (pk 'music-toggle)
	     (mute-toggle scene))
	   (when (equal? key key:space)
	     (*state* (if (current-scene-completed? local remote)
			  (append-empty-scene! state data init-scene)
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
    (timeout update-callback dt))
  
  (then (load-font Prime)
	(procedure->external init-call)))
