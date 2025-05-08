(define-module (ren-sexp scene-utils)
  #:use-module (ren-sexp sprites)
  #:use-module (goblins actor-lib let-on)
  #:use-module (fibers channels)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp bg)
  #:use-module (ren-sexp music)
  #:use-module (ren-sexp utils)
  #:use-module (goblins)
  #:export (append-empty-scene!))

(define (append-empty-scene! state scene next empty-scene)
  (let* ((curr-bg (scene-bg scene))
	 (next-bg (scene-bg next))
	 
	 (curr-sprites (scene-sprites scene))
	 (next-sprites (scene-sprites next))
	 
	 (curr-music (scene-music scene))
	 (next-music (scene-music next))
	 
	 (next-scene1
	  (include-bg empty-scene curr-bg next-bg))

	 (next-scene2
	  (include-sprites next-scene1 curr-sprites next-sprites))
	 
	 (next-scene3
	  (include-music next-scene2 curr-music next-music)))
    
    (cons next-scene3 state)))
