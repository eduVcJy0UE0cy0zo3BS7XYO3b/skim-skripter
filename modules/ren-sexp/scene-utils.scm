(define-module (ren-sexp scene-utils)
  #:use-module (ren-sexp sprites)
  #:use-module (fibers channels)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp bg)
  #:use-module (ren-sexp music)
  #:use-module (ren-sexp utils)
  #:use-module (goblins)
  #:export (append-empty-scene!))

(define (append-empty-scene! ^state empty-scene)
  (let* ((curr-scene ($ ^state 'current-scene))
	 (next-scene ($ ^state 'current-story-scene))

	 (curr-bg (scene-bg curr-scene))
	 (next-bg (scene-bg next-scene))
	 
	 (curr-sprites (scene-sprites curr-scene))
	 (next-sprites (scene-sprites next-scene))
	 
	 (curr-music (scene-music curr-scene))
	 (next-music (scene-music next-scene)))
    
    (define next-scene1
      (include-bg empty-scene curr-bg next-bg))
    (define next-scene2
      (include-sprites next-scene1 curr-sprites next-sprites))
    (define next-scene3
      (include-music next-scene2 curr-music next-music))
    
    (cons next-scene3 ^state)))
