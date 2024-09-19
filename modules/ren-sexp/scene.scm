(define-module (ren-sexp scene)
  #:pure
  #:use-module (scheme base)
  #:use-module (ice-9 match)
  #:use-module (ren-sexp utils)

  #:export (<scene>
	    make-scene
	    scene-state
	    scene-bg
	    scene-text
	    scene-sprites
	    scene-music
	    scene-carret
	    
	    scene-update-text
	    scene-update-bg
	    scene-update-sprites
	    scene-update-music
	    scene-update-carret))

(define-record-type <scene>
  (make-scene state bg text sprites music carret)
  scene?
  (state scene-state)
  (bg scene-bg)
  (text scene-text)
  (sprites scene-sprites)
  (music scene-music)
  (carret scene-carret))

(define (scene-update-carret scene carret*)
  (match scene
    (($ <scene> state bg text sprites music carret)
     (make-scene state bg text sprites music carret*))))

(define (scene-update-text scene text*)
  (match scene
    (($ <scene> state bg text sprites music carret)
     (make-scene state bg text* sprites music carret))))

(define (scene-update-bg scene bg*)
  (match scene
    (($ <scene> state bg text sprites music carret)
     (make-scene state bg* text sprites music carret))))

(define (scene-update-sprites scene sprites*)
  (match scene
    (($ <scene> state bg text sprites music carret)
     (make-scene state bg text sprites* music carret))))

(define (scene-update-music scene music*)
  (match scene
    (($ <scene> state bg text sprites music carret)
     (make-scene state bg text sprites music* carret))))
