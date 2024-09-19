(define-module (ren-sexp carret)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme char)
  #:use-module (dom element)
  #:use-module (dom canvas)
  #:use-module (dom document)
  #:use-module (ice-9 match)
  #:use-module (hoot debug)
  #:use-module (ren-sexp utils)
  #:use-module (ren-sexp scene)
  #:export (<carret>
	    %make-carret
	    make-carret
	    carret-char
	    carret-alpha

	    draw-carret
	    next-carret

	    include-carret))

(define-record-type <carret>
  (%make-carret char alpha)
  carret?
  (char carret-char)
  (alpha carret-alpha))

(define (make-carret char)
  (%make-carret char 1000))

(define (even? number)
  (equal? (remainder number 2) 0))

(define (draw-carret carret context padding-top text-width completed?)
  (match carret
    (($ <carret> char alpha)
     ;; (context-save! context)
     ;; (set-alpha! context (/ alpha 1000.0))
     (fill-text context
		(if completed?
		    (if (even? (current-second)) "▢" "")
		    "▷")
		(+ 470.0 20 text-width)
		(- padding-top 50.0))
     ;; (context-restore! context)
     )
    (_ #f)))

(define (next-carret src dst)
  (define dst-carret (scene-carret dst))
  (define src-carret (scene-carret src))
  (define alpha (carret-alpha dst-carret))
  (define delta 10)
  (define next-alpha (+ alpha delta))
  (define carret* (carret-update-alpha src-carret next-alpha))
  (scene-update-carret dst carret*))

(define (carret-update-alpha carret alpha*)
  (match carret
    (($ <carret> char alpha)
     (%make-carret char alpha*))))

(define (include-carret scene curr next)
  (if (equal? curr next)
      (scene-update-carret scene curr)
      scene))
