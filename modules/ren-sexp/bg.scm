(define-module (ren-sexp bg)
  #:use-module (scheme char)
  #:use-module (dom element)
  #:use-module (dom canvas)
  #:use-module (ice-9 match)
  #:use-module (hoot records)
  #:use-module (ren-sexp utils)
  #:use-module (ren-sexp scene)
  #:export (<bg>
	    %make-bg
	    make-bg
	    make-bg-with-path
	    bg-img
	    bg-alpha
	    bg-path
	    same-bg?
	    draw-bg
	    next-bg

	    include-bg))

(define (same-bg? bg1 bg2)
  (equal? bg1 bg2))

(define-record-type <bg>
  (%make-bg img alpha path)
  bg?
  (img bg-img)
  (alpha bg-alpha)
  (path bg-path))

(define (make-bg img)
  (%make-bg img 1000 #f))

(define (make-bg-with-path img path)
  (%make-bg img 1000 path))

(define (draw-bg bg context game-width game-height)
  (match bg
    (($ <bg> img alpha)
     (context-save! context)
     (set-alpha! context (/ alpha 1000.0))
     (draw-image context
		 img
		 0.0
		 0.0
		 game-width
		 game-height
		 0.0
		 0.0
		 game-width
		 game-height)
     (context-restore! context))
    (_ #f)))

(define (next-bg src dst)
  (let* ((dst-bg (scene-bg dst))
	 (src-bg (scene-bg src)))
    (if dst-bg
	(let* ((alpha (bg-alpha dst-bg))
	       (delta 10)
	       (next-alpha (+ alpha delta))
	       (bg* (bg-update-alpha src-bg next-alpha)))
	  (scene-update-bg dst bg*))
        (let ((bg* (bg-update-alpha src-bg 0)))
	  (scene-update-bg dst bg*)))))

(define (bg-update-alpha bg alpha*)
  (match bg
    (($ <bg> img alpha path)
     (%make-bg img alpha* path))
    (_ #f)))

(define (include-bg scene curr next)
  ;; (pk 54321)
  (if (equal? curr next)
      (scene-update-bg scene curr)
      scene))
