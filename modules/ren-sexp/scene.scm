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

	    scene-update-text
	    scene-update-bg
	    scene-update-sprites
	    scene-update-music))

(define-record-type <scene>
  (make-scene state bg text sprites music)
  scene?
  (state scene-state)
  (bg scene-bg)
  (text scene-text)
  (sprites scene-sprites)
  (music scene-music))

(define (scene-update-text scene text*)
  (match scene
    (($ <scene> state bg text sprites music)
     (make-scene state bg text* sprites music))))

(define (scene-update-bg scene bg*)
  (match scene
    (($ <scene> state bg text sprites music)
     (make-scene state bg* text sprites music))))

(define (scene-update-sprites scene sprites*)
  (match scene
    (($ <scene> state bg text sprites music)
     (make-scene state bg text sprites* music))))

(define (scene-update-music scene music*)
  (match scene
    (($ <scene> state bg text sprites music)
     (make-scene state bg text sprites music*))))
