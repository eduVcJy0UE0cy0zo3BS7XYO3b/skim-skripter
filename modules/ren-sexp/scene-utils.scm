(define-module (ren-sexp scene-utils)
  #:use-module (ren-sexp sprites)
  #:use-module (fibers channels)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp bg)
  #:use-module (ren-sexp music)
  #:use-module (ren-sexp utils)
  #:export (append-empty-scene!))

(define (append-empty-scene! state-box scene next empty-scene)
  (let* ((curr-bg (scene-bg scene))
	 (next-bg (scene-bg next))

	 (curr-sprites (scene-sprites scene))
	 (next-sprites (scene-sprites next))

	 (curr-music (scene-music scene))
	 (next-music (scene-music next))

	 (with-bg
	  (include-bg empty-scene curr-bg next-bg))

	 (with-sprites
	  (include-sprites with-bg curr-sprites next-sprites))

	 (with-music
	  (include-music with-sprites curr-music next-music))
         )

    (atomic-append-scene state-box with-music)))
