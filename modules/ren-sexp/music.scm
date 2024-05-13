(define-module (ren-sexp music)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme char)
  #:use-module (dom element)
  #:use-module (dom media)
  #:use-module (ice-9 match)
  #:use-module (ren-sexp utils)
  #:use-module (ren-sexp scene)
  #:use-module (hoot debug)
  #:export (<music>
	    make-music
	    music?
	    music-audio
	    music-volume

	    include-music))

(define-record-type <music>
  (make-music audio volume)
  music?
  (audio music-audio)
  (volume music-volume))

(define (same-audio? music1 music2)
  (let ((audio1 (music-audio music1))
        (audio2 (music-audio music2)))
    (string=? audio1 audio2)))

(define (same-volume? music1 music2)
  (let ((volume1 (music-volume music1))
        (volume2 (music-volume music2)))
    (= volume1 volume2)))

(define (update-music-volume music volume*)
  (match music
    (($ <music> audio volume)
     (make-music audio volume*))))
  
(define (inc-volume src-music dst-music)
  (if (not (music? dst-music))
      (make-music (music-audio src-music) 0)
      (when (same-audio? src-music dst-music)
	  (update-music-volume dst-music (+ 1 (music-volume dst-music))))))

(define (next-music src dst)
  (define dst-music (scene-music dst))
  (define src-music (scene-music src))
  (define music* (inc-volume src-music dst-music))
  (scene-update-music dst music*))

(define (include-music next-scene curr-music next-music)
  (cond
   ((and (not next-music)
	 (not curr-music))
    (pk 1)
    next-scene)

   ((and (music? next-music)
	 (music? curr-music)
	 (equal? next-music curr-music))
    (pk 2)
    next-scene)

   ((and (music? next-music)
	 (music? curr-music)
	 (same-audio? next-music curr-music)
	 (not (same-volume? next-music curr-music)))
    (pk 3)
    (define next-volume (+ 1 (music-volume curr-music)))
    (set-media-volume! (music-audio curr-music) next-volume)
    (define music* (update-music-volume curr-music next-volume))
    (scene-update-music next-scene music*))
   
   ((and (not next-music) (music? curr-music))
    (pk 4)
    (media-pause (music-audio curr-music))
    next-scene)
   
   ((and (music? next-music) (not curr-music))
    (pk 5)
    (media-play (music-audio next-music))
    (set-media-loop! (music-audio next-music) 1)
    (scene-update-music next-scene next-music))))
