(define-module (ren-sexp sprites)
  #:use-module (scheme char)
  #:use-module (dom element)
  #:use-module (dom canvas)
  #:use-module (ice-9 match)
  #:use-module (hoot records)
  #:use-module (ren-sexp utils)
  #:use-module (hoot debug)
  #:use-module (ren-sexp scene)
  #:export (<sprite>
	    make-sprite
	    sprite-img
	    sprite-alpha
	    sprite-pos
	    sprite-character
	    sprite-mood
	    sprite-wear

	    draw-sprites
	    next-sprites
	    get-same-sprites
	    include-sprites))

(define-record-type <sprite>
  (make-sprite img alpha pos character mood wear)
  sprite?
  (img sprite-img)
  (alpha sprite-alpha)
  (pos sprite-pos)
  (character sprite-character)
  (mood sprite-mood)
  (wear sprite-wear))

(define (characters-in-scene sprites)
  (map (lambda (sprite) (sprite-character sprite)) sprites))

(define (sprite-in-scene? src-sprite dst-sprites)
  (define src-character (sprite-character src-sprite))
  (define all-chars (characters-in-scene dst-sprites))
  (if (member src-character all-chars)
      (car (member src-character all-chars))
      (member src-character all-chars)))

(define (update-sprite-alpha sprite alpha*)
  (match sprite
    (($ <sprite> img alpha pos character mood wear)
     (make-sprite img alpha* pos character mood wear))))

(define (get-sprite-by-name name sprites)
  (define (%get-sprite-by-name sprites)
    (if (null? sprites)
	'()
	(if (eq? name (sprite-character (car sprites)))
	    (car sprites)
	    (%get-sprite-by-name (cdr sprites)))))
  (%get-sprite-by-name sprites))

(define (next-sprite src-sprites dst-sprites)
  (map (lambda (src-sprite)
	 (if (sprite-in-scene? src-sprite dst-sprites)
	     (let* ((dst-sprite (get-sprite-by-name (sprite-character src-sprite)
						    dst-sprites))
		    (dst-sprite-alpha (sprite-alpha dst-sprite)))
	       (if (= dst-sprite-alpha 1000)
		   dst-sprite
		   (update-sprite-alpha dst-sprite (+ dst-sprite-alpha 50))))
	     (update-sprite-alpha src-sprite 0)))
       src-sprites))

(define (next-sprites src dst)
  (define dst-sprites (scene-sprites dst))
  (define src-sprites (scene-sprites src))
  (define sprites* (next-sprite src-sprites dst-sprites))
  (scene-update-sprites dst sprites*))

(define (draw-sprite sprite context)
  (match sprite
    (($ <sprite> img alpha pos character mood)
     (context-save! context)
     (set-alpha! context (/ alpha 1000.0))
     (define pos* (position->point pos))
     (define dx (car pos*))
     (define dy (cdr pos*))       
     (draw-image-simple context
			img
			dx
		        dy)
     (context-restore! context))
    (_ #f)))

(define (draw-sprites sprites context)
  (for-each
   (lambda (sprite) (draw-sprite sprite context))
   sprites))

(define (position->point position)
  (match position
    ('left (cons 200 100))
    ('right (cons 1200 100))))

(define (get-same-sprites curr-sprites next-sprites)
  (define (%get-same-sprites curr-sprites result)
    (if (null? curr-sprites)
	result
	(let* ((curr-sprite (car curr-sprites))
	       (next-sprite-name (sprite-in-scene? curr-sprite next-sprites)))
	  (pk next-sprite-name)
	  (if next-sprite-name
	      (%get-same-sprites (cdr curr-sprites) (cons (get-sprite-by-name next-sprite-name next-sprites) result))
	      (%get-same-sprites (cdr curr-sprites) result)))))
  (%get-same-sprites curr-sprites '()))

(define (include-sprites next-scene curr-sprites next-sprites)
  (let ((same-sprites (get-same-sprites curr-sprites next-sprites)))
    (scene-update-sprites next-scene same-sprites)))
