(define-module (ren-sexp bg)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme char)
  #:use-module (dom element)
  #:use-module (dom canvas)
  #:use-module (ice-9 match)
  #:use-module (hoot debug)
  #:use-module (ren-sexp utils)
  #:use-module (ren-sexp scene)
  #:export (<bg>
	    %make-bg
	    make-bg
	    bg-img
	    bg-alpha

	    draw-bg
	    next-bg

	    include-bg))

(define-record-type <bg>
  (%make-bg img alpha)
  bg?
  (img bg-img)
  (alpha bg-alpha))

(define (make-bg img)
  (%make-bg img 1000))

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
  (define dst-bg (scene-bg dst))
  (define src-bg (scene-bg src))
  (define alpha (bg-alpha dst-bg))
  (define delta 10)
  (define next-alpha (+ alpha delta))
  (define bg* (bg-update-alpha src-bg next-alpha))
  (scene-update-bg dst bg*))

(define (bg-update-alpha bg alpha*)
  (match bg
    (($ <bg> img alpha)
     (%make-bg img alpha*))))

(define (include-bg scene curr next)
  (if (equal? curr next)
      (scene-update-bg scene curr)
      scene))
