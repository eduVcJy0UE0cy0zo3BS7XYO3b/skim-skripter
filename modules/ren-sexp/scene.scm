(define-module (ren-sexp scene)
  #:use-module (ice-9 match)
  #:use-module (hoot records)
  #:use-module (ren-sexp utils)

  #:export (<scene>
	    make-scene
	    scene?
	    scene-state
	    scene-bg
	    scene-old-text
	    scene-text
	    scene-sprites
	    scene-music
	    scene-carret
	    scene-ttl
	    
	    scene-add-text
	    scene-update-text
	    scene-update-bg
	    scene-add-bg
	    scene-update-sprites
	    scene-update-music
	    scene-add-music
	    scene-update-carret
	    scene-update-ttl
	    make-scene-with-state

	    next-ttl))

(define-record-type <scene>
  (%make-scene state bg old-text text sprites music carret ttl)
  scene?
  (state scene-state)
  (bg scene-bg)
  (old-text scene-old-text)
  (text scene-text)
  (sprites scene-sprites)
  (music scene-music)
  (carret scene-carret)
  (ttl scene-ttl))

(define random
  (let ((a 69069) (c 1) (m (expt 2 32)) (seed 19380110))
    (lambda new-seed
      (if (pair? new-seed)
          (set! seed (car new-seed))
          (set! seed (modulo (+ (* seed a) c) m)))
      (/ seed m))))

(define* (make-scene
	  #:key
	  ;; (id (random))
	  (state 'play)
	  (bg #f)
	  (old-text (list))
	  (text "")
	  (sprites (list))
	  (music #f)
	  (carret "")
	  (ttl 'inf))
  (%make-scene state bg old-text text sprites music carret ttl))

;; Создать сцену с заданным состоянием (для восстановления из сохранений)
(define (make-scene-with-state bg music sprites old-text)
  (%make-scene 'play bg old-text "" sprites music "" 'inf))

(define (scene-update-carret scene carret*)
  (match scene
    (($ <scene> state bg old-text text sprites music carret ttl)
     (make-scene
		 #:state state
		 #:bg bg
		 #:old-text old-text
		 #:text text
		 #:sprites sprites
		 #:music music
		 #:carret carret*
		 #:ttl ttl))))

(define (scene-add-text scene old-text*)
  (match scene
    (($ <scene> state bg old-text text sprites music carret ttl)
     (make-scene ;; #:id id
		 #:state state
		 #:bg bg
		 #:old-text old-text*
		 #:text text
		 #:sprites sprites
		 #:music music
		 #:carret carret))))

(define (scene-update-text scene text*)
  (match scene
    (($ <scene> state bg old-text text sprites music carret ttl)
     (make-scene 
		 #:state state
		 #:bg bg
		 #:old-text old-text
		 #:text text*
		 #:sprites sprites
		 #:music music
		 #:carret carret))))

(define (scene-add-bg scene bg*)
  (match scene
    (($ <scene> state bg old-text text sprites music carret ttl)
     (make-scene 
		 #:state state
		 #:bg bg*
		 #:old-text old-text
		 #:text text
		 #:sprites sprites
		 #:music music
		 #:carret carret
		 #:ttl 2))))

(define (scene-update-bg scene bg*)
  (match scene
    (($ <scene> state bg old-text text sprites music carret ttl)
     (make-scene 
		 #:state state
		 #:bg bg*
		 #:old-text old-text
		 #:text text
		 #:sprites sprites
		 #:music music
		 #:carret carret
		 #:ttl ttl))))

(define (scene-update-sprites scene sprites*)
  (match scene
    (($ <scene> state bg old-text text sprites music carret ttl)
     (make-scene
		 #:state state
		 #:bg bg
		 #:old-text old-text
		 #:text text
		 #:sprites sprites*
		 #:music music
		 #:carret carret
		 #:ttl ttl))))

(define (scene-add-music scene music*)
  (match scene
    (($ <scene> state bg old-text text sprites music carret ttl)
     (make-scene
		 #:state state
		 #:bg bg
		 #:old-text old-text
		 #:text text
		 #:sprites sprites
		 #:music music*
		 #:carret carret
		 #:ttl 2))))

(define (scene-update-music scene music*)
  (match scene
    (($ <scene> state bg old-text text sprites music carret ttl)
     (make-scene
		 #:state state
		 #:bg bg
		 #:old-text old-text
		 #:text text
		 #:sprites sprites
		 #:music music*
		 #:carret carret
		 #:ttl ttl))))

(define (next-ttl target current)
  (define current-ttl (scene-ttl current))
  (define target-ttl (scene-ttl target))
  (scene-update-ttl
   current
   (if (and (not (equal? target-ttl 'inf))
	    (equal? current-ttl 'inf))
       0
       (+ current-ttl 1))))

(define (scene-update-ttl scene ttl*)
  (match scene
    (($ <scene> state bg old-text text sprites music carret ttl)
     (make-scene
		 #:state state
		 #:bg bg
		 #:old-text old-text
		 #:text text
		 #:sprites sprites
		 #:music music
		 #:carret carret
		 #:ttl ttl*))))
