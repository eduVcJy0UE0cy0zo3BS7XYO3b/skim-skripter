(define-module (ren-sexp scene-utils)
  #:use-module (ren-sexp sprites)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp bg)
  #:use-module (ren-sexp music)
  #:use-module (ren-sexp utils)
  #:export (current-scene-completed?
	    local-and-remote-scene
	    current-scene-completed?
	    current-state-completed?
	    append-empty-scene!))

(define (local-and-remote-scene state data)
  (define local/current-scene (last state))
  (define local/current-scene-id (- (length state) 1))
  (define remote/current-scene (list-ref data local/current-scene-id))
  (cons local/current-scene remote/current-scene))

(define (current-scene-completed? local remote)
  (equal? remote local))

(define (current-state-completed? state data)
  (let* ((local&current (local-and-remote-scene state data))
	 (local (car local&current))
	 (remote (cdr local&current)))
    (current-scene-completed? local remote)))

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
