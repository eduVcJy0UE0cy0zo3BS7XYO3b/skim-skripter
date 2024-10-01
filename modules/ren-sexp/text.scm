(define-module (ren-sexp text)
  #:use-module (scheme char)
  #:use-module (dom element)
  #:use-module (dom canvas)
  #:use-module (dom document)
  #:use-module (hoot strings)
  #:use-module (ice-9 match)
  #:use-module (ren-sexp utils)
  #:use-module (ren-sexp scene)
  #:export (next-string
	    draw-text
	    same-text?
	    next-old-text
	    draw-old-text
	    same-old-text?))

(define (same-text? text1 text2)
  (equal? text1 text2))

(define (same-old-text? text1 text2)
  (equal? text1 text2))

(define (get-lines context text)
  (if (string=? text "")
      '("")
      (let* ((words (string-split char-whitespace? text))
	     (current-line (car words)))
	(define (get-lines-iter lines words current-line)
	  (if (null? words)
	      (cons current-line lines)
	      (let* ((word (car words))
		     (rest-words (cdr words))
		     (current-line* (string-append current-line " " word))
		     (width (element-width (measure-text context current-line*))))
		
		(if (< width 1000)
		    (get-lines-iter lines rest-words current-line*)
		    (get-lines-iter (cons current-line lines) rest-words word)))))
	
	(reverse (get-lines-iter '() (cdr words) current-line)))))

(define (next-string next current)
  (define current-text (scene-text current))
  (define next-text (scene-text next))
  (define pos (string-length current-text))
  (define next-pos (+ 1 pos))
  (define text* (substring next-text 0 next-pos))
  (scene-update-text current text*))

(define (next-old-text next current)
  (define current-text (scene-old-text current))
  (define next-text (scene-old-text next))
  (scene-add-text current next-text))

(define (draw-old-text old-text context initial-padding)
  (match old-text
    ((text old-lines ...)
     (draw-old-text
      old-lines
      context
      (if (equal? text "")
	  initial-padding
	  (+ 20 (car (draw-text text context initial-padding))))))
    (() initial-padding)))

(define (draw-text text context initial-padding)
  (define lines (get-lines context text))
  (define (display-lines lines padding-top old-lines)
    (match lines
      ((current-line rest-lines ...)
       (fill-text context current-line 470.0 padding-top)
       (stroke-text context current-line 470.0 padding-top)
       (display-lines rest-lines
		      (+ 50.0 padding-top)
		      (cons current-line old-lines)))
      (()
       (if (null? old-lines)
	   (cons padding-top 0)
	   (let* ((last-line (car old-lines))
		  (text-width (element-width (measure-text context last-line))))
	     (cons padding-top text-width))))))
  
  (let ((ans (display-lines lines initial-padding '())))
    (if (unspecified? ans)
	(cons 50 0)
	ans)))
