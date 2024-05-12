;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; Example game showing off several common game programming things.
;;;
;;; Code:

(import (scheme base)
        (scheme inexact)
        (hoot debug)
        (hoot ffi)
	(scheme char)
        (hoot hashtables)
        (ice-9 match)
        (dom canvas)
        (dom document)
        (dom element)
        (dom event)
        (dom image)
        (dom media)
        (dom window)
        (math)
        (math rect)
        (math vector))

(define (string-split char-delimiter? string)
  (define (maybe-add a b parts)
    (if (= a b) parts (cons (substring string a b) parts)))
  (let ((n (string-length string)))
    (let loop ((a 0) (b 0) (parts '()))
      (if (< b n)
          (if (not (char-delimiter? (string-ref string b)))
              (loop a (+ b 1) parts)
              (loop (+ b 1) (+ b 1) (maybe-add a b parts)))
          (reverse (maybe-add a b parts))))))

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

(define (last l)
  (cond ((null? (cdr l)) (car l))
        (else (last (cdr l)))))

(define (find-replace a b list)
  (cond
   ((null? list) '())
   ((list? (car list)) (cons (find-replace a b (car list)) (find-replace a b (cdr list))))
   ((eq? (car list) a) (cons b (find-replace a b (cdr list))))
   (else
    (cons (car list) (find-replace a b (cdr list))))))

;; Data types
(define-record-type <scene>
  (make-scene state bg text sprites music)
  scene?
  (state scene-state)
  (bg scene-bg)
  (text scene-text)
  (sprites scene-sprites)
  (music scene-music))

(define-record-type <bg>
  (%make-bg img alpha)
  bg?
  (img bg-img)
  (alpha bg-alpha))

(define-record-type <sprite>
  (make-sprite img alpha pos character mood wear)
  sprite?
  (img sprite-img)
  (alpha sprite-alpha)
  (pos sprite-pos)
  (character sprite-character)
  (mood sprite-mood)
  (wear sprite-wear))

(define-record-type <music>
  (make-music audio volume)
  music?
  (audio music-audio)
  (volume music-volume))

(define (make-bg img)
  (%make-bg img 1000))

(define image:black
  (make-image "resources/bg/end.png"))

(define image:white
  (make-image "resources/bg/white.png"))

(define image:green
  (make-image "resources/bg/green.png"))

(define black-screen
  (%make-bg image:black 0))

(define (empty-scene)
  (make-scene 'play black-screen "" (list) #f))

(define (%empty-scene)
  (make-scene 'play black-screen "Hello and Welcome to this new supert visal novel engine!  Press <SPACE> to launch game." (list) #f))

;; Assets
(define image:kitchen
  (make-image "resources/bg/bg_kitchen.jpeg"))

(define image:nastya-tired
  (make-image "resources/sprites/nastya_tired_1600.png"))

(define image:masha-happy
  (make-image "resources/sprites/masha_happy_1600.png"))

(define audio:curious_critters
  (make-audio "resources/bgm/curious_critters_extended.mp3"))

(define audio:birds
  (make-audio "resources/bgm/salt_marsh_birds.mp3"))

(define music:curious_critters
  (make-music audio:curious_critters 100))

(define music:birds
  (make-music audio:birds 100))

;; Game data
(define game-width    1920.0)
(define game-height   1080.0)

(define nastya:tired
  (make-sprite image:nastya-tired 1000 'left 'nastya 'tired 'casual))

(define masha:happy
  (make-sprite image:masha-happy 1000 'right 'masha 'happy 'casual))

(define scene-1
  (make-scene 'play
	      (make-bg image:kitchen)
	      "Ullam qui est expedita."
	      (list masha:happy)
	      music:curious_critters))

(define scene-2
  (make-scene 'play
	      (make-bg image:kitchen)
	      "Это текст на русском."
	      (list masha:happy nastya:tired)
	      music:curious_critters))

(define scene-3
  (make-scene 'play
	      (make-bg image:kitchen)
	      "Тут пусто и без звуков."
	      (list)
	      #f))

(define scene-4
  (make-scene 'play
	      (make-bg image:kitchen)
	      "4 Сцена."
	      (list masha:happy)
	      music:birds))

(define scene-5
  (make-scene 'play
	      (make-bg image:kitchen)
	      "5 Сцена."
	      (list masha:happy nastya:tired)
	      music:birds))

(define END
  (make-scene 'play
	      (make-bg image:black)
	      ""
	      (list)
	      #f))

(define EMPTY-SCENE (empty-scene))
(define DATA (list EMPTY-SCENE
		   scene-1
		   scene-2
		   scene-3
		   scene-4
		   scene-5
		   END
		   ))
(define *state* (make-parameter (list EMPTY-SCENE)))

(define (current-scene-completed? state)
  (define local/current-scene (last state))
  (define local/current-scene-id (- (length state) 1))
  (define remote/current-scene (list-ref DATA local/current-scene-id))
  (equal? remote/current-scene local/current-scene))

(define (scene-update-text scene text*)
  (match scene
    (($ <scene> state bg text sprites music)
     (make-scene state bg text* sprites music))))

(define (scene-update-bg scene bg*)
  (match scene
    (($ <scene> state bg text sprites music)
     (make-scene state bg* text sprites music))))

(define (scene-update-sprites scene sprites*)
  (match scene
    (($ <scene> state bg text sprites music)
     (make-scene state bg text sprites* music))))

(define (scene-update-music scene music*)
  (match scene
    (($ <scene> state bg text sprites music)
     (make-scene state bg text sprites music*))))

(define (bg-update-alpha bg alpha*)
  (match bg
    (($ <bg> img alpha)
     (%make-bg img alpha*))))

(define (next-bg src dst)
  (define dst-bg (scene-bg dst))
  (define src-bg (scene-bg src))
  (define alpha (bg-alpha dst-bg))
  (define delta 10)
  (define next-alpha (+ alpha delta))
  (define bg* (bg-update-alpha src-bg next-alpha))
  (scene-update-bg dst bg*))

(define (next-string src dst)
  (define dst-text (scene-text dst))
  (define src-text (scene-text src))
  (define pos (string-length dst-text))
  (define next-pos (+ 1 pos))
  (define text* (substring src-text 0 next-pos))
  (scene-update-text dst text*))

(define (characters-in-scene sprites)
  (map (lambda (sprite) (sprite-character sprite)) sprites))

(define (sprite-in-scene? src-sprite dst-sprites)
  (define src-character (sprite-character src-sprite))
  (define all-chars (characters-in-scene dst-sprites))
  (member src-character all-chars))

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

(define (next-scene-increment src dst)
  (cond
   ((not (equal? (scene-bg src) (scene-bg dst))) (next-bg src dst))
   ;; ((not (equal? (scene-music src) (scene-music dst))) (next-music src dst))
   ((not (equal? (scene-sprites src) (scene-sprites dst))) (next-sprites src dst))
   ((not (equal? (scene-text src) (scene-text dst))) (next-string src dst))
   (else src)))

(define (get-next-increment state)
  (define local/current-scene (last state))
  (define local/current-scene-id (- (length state) 1))
  (define remote/current-scene (list-ref DATA local/current-scene-id))
  (define local/current-scene* (next-scene-increment remote/current-scene local/current-scene))
  (define state* (find-replace local/current-scene local/current-scene* state))
  state*)

(define (complete-current-scene! state)
  (define local/current-scene (last state))
  (define local/current-scene-id (- (length state) 1))
  (define remote/current-scene (list-ref DATA local/current-scene-id))
  (define state* (find-replace local/current-scene remote/current-scene state))
  (*state* state*))
  
(define (compute-next-state state)
  (if (current-scene-completed? state)
      state
      (get-next-increment state)))

(define dt (/ 1000.0 60.0)) ; aim for updating at 60Hz
(define (update)
  (define state (*state*))
  (define scene (last state))
  (match (scene-state scene)
    ('play (*state* (compute-next-state state)))
    (_ #t))
  (timeout update-callback dt))
(define update-callback (procedure->external update))

;; Rendering
(define number->string*
  (let ((cache (make-eq-hashtable))) ; assuming fixnums only
    (lambda (x)
      (or (hashtable-ref cache x)
          (let ((str (number->string x)))
            (hashtable-set! cache x str)
            str)))))

(define (draw-bg bg context)
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

(define (draw-text text context)
  (set-fill-color! context "#ffffff")
  (set-border-color! context "black")
  (set-font! context "bold 40px Prime")
  (set-text-align! context "left")
  (set-shadow-blur! context 4)
  (set-shadow-color! context "rgba(0,0,0,0.3)")
  
  (define lines (get-lines context text))
  (define (display-lines lines line-height)
    (unless (null? lines)
      (fill-text context (car lines) 470.0 line-height)
      (stroke-text context (car lines) 470.0 line-height)
      (display-lines (cdr lines) (+ line-height 50.0))))
  
  (display-lines lines 50.0))

(define (position->point position)
  (match position
    ('left (cons 200.0 100.0))
    ('right (cons 1200.0 100.0))))

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

(define (draw prev-time)
  (define scene (last (*state*)))
  (let ((text (scene-text scene))
        (bg (scene-bg scene))
	(sprites (scene-sprites scene)))

    (set-fill-color! context "#140c1c")
    (fill-rect context 0.0 0.0 game-width game-height)

    (draw-bg bg context)
    (draw-sprites sprites context)
    (draw-text text context))
  
  (request-animation-frame draw-callback))
(define draw-callback (procedure->external draw))

;; Input
(define key:space "Space")

(define (get-same-sprites curr-sprites next-sprites)
  (define (%get-same-sprites curr-sprites result)
    (if (null? curr-sprites)
	result
	(let ((curr-sprite (car curr-sprites)))
	  (if (sprite-in-scene? curr-sprite next-sprites)
	      (%get-same-sprites (cdr curr-sprites) (cons curr-sprite result))
	      (%get-same-sprites (cdr curr-sprites) result)))))
  (%get-same-sprites curr-sprites '()))

(define (append-empty-scene! state)
  (unless (eq? (length state)
	       (length DATA))
    (let* ((curr-scene (last state))
	   (curr-scene-id+1 (length state))
	   (next-scene (list-ref DATA curr-scene-id+1))
	   
	   (curr-bg (scene-bg curr-scene))
	   (next-bg (scene-bg next-scene))

	   (curr-sprites (scene-sprites curr-scene))
	   (next-sprites (scene-sprites next-scene))

	   (curr-music (scene-music curr-scene))
	   (next-music (scene-music next-scene)))
      
      (define next-scene1
	(if (equal? curr-bg next-bg)
	    (scene-update-bg EMPTY-SCENE curr-bg)
	    EMPTY-SCENE))
      
      (define next-scene2
        (let ((same-sprites (get-same-sprites curr-sprites next-sprites)))
	  (scene-update-sprites next-scene1 same-sprites)))

      (define next-scene3
	(cond
	 ((and (not next-music)
	       (not curr-music))
	  (pk 1)
	  next-scene2)

	 ((and (music? next-music)
	       (music? curr-music)
	       (equal? next-music curr-music))
	  (pk 2)
	  next-scene2)

	 ((and (music? next-music)
	       (music? curr-music)
	       (same-audio? next-music curr-music)
	       (not (same-volume? next-music curr-music)))
	  (pk 3)
	  (define next-volume (+ 1 (music-volume curr-music)))
	  (set-media-volume! (music-audio curr-music) next-volume)
	  (define music* (update-music-volume curr-music next-volume))
	  (scene-update-music next-scene2 music*))
	 
	 ((and (not next-music) (music? curr-music))
	  (pk 4)
	  (media-pause (music-audio curr-music))
	  next-scene2)
	 
	 ((and (music? next-music) (not curr-music))
	  (pk 5)
	  (media-play (music-audio next-music))
	  (set-media-loop! (music-audio next-music) 1)
	  (scene-update-music next-scene2 next-music))))

      (pk next-scene3)
      (*state* (append state (list next-scene3))))))

(define (on-key-up event)
  (let ((key (keyboard-event-code event)))
    (define scene (last (*state*)))
    (pk scene)
    (match (scene-state scene)
      ('play
       (when (equal? key key:space)
	 (define current-state (*state*))
	 (if (current-scene-completed? current-state)
	     (append-empty-scene! current-state)
	     (complete-current-scene! current-state))))
      (_ #t))))

;; Canvas and event loop setup
(define canvas (get-element-by-id "canvas"))
(define context (get-context canvas "2d"))
;; (define Prime (download-font!
;; 	       "Prime"
;; 	       "url(/resources/fonts/courierprime.otf/courier-prime.otf)"))


(set-element-width! canvas (exact game-width))
(set-element-height! canvas (exact game-height))
(add-event-listener! (current-document) "keyup"
                     (procedure->external on-key-up))

;; (define (init-call font)
;;   (add-font! font)
;;   (request-animation-frame draw-callback)
;;   (timeout update-callback dt))

;; (then (load-font Prime)
;;       (procedure->external init-call))

(request-animation-frame draw-callback)
(timeout update-callback dt)
