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
        (dom image)
	(dom media)
	;; (game-assets)
	(ren-sexp music)
	(ren-sexp scene)
	(ren-sexp bg)
	(ren-sexp sprites)
	(ren-sexp core))


;; Data types
(define image:end
  (make-image "resources/bg/end.png"))

(define image:black
  (make-image "resources/bg/black.png"))

(define image:white
  (make-image "resources/bg/white.png"))

(define image:green
  (make-image "resources/bg/green.png"))

(define black-screen
  (%make-bg image:black 1000))

(define white-screen
  (%make-bg image:white 0))

(define (empty-scene)
  (make-scene 'play (%make-bg image:black 0) "" (list) #f))

(define (%empty-scene)
  (make-scene 'play black-screen "Hello and Welcome to this new super visual novel engine!  Press <SPACE> to launch the game." (list) #f))

;; Assets
(define image:kitchen
  (make-image "resources/bg/bg_kitchen.jpeg"))

(define image:dummy-5.2K
  (make-image "resources/sprites/dummy.png"))
  
(define image:nastya-tired
  (make-image "resources/sprites/nastya_tired_1600.webp"))

(define image:masha-happy
  (make-image "resources/sprites/masha_happy_1600.webp"))

(define audio:curious_critters
  (make-audio "resources/bgm/curious_critters_extended.mp3"))

(define audio:birds
  (make-audio "resources/bgm/salt_marsh_birds.mp3"))

(define music:curious_critters
  (make-music audio:curious_critters 100))

(define music:birds
  (make-music audio:birds 100))

(define dummy:tired
  (make-sprite image:dummy-5.2K 1000 'left 'nastya 'tired 'casual))

(define dummy:happy
  (make-sprite image:dummy-5.2K 1000 'right 'masha 'happy 'casual))

(define nastya:tired
  (make-sprite image:nastya-tired 1000 'left 'nastya 'tired 'casual))

(define masha:happy
  (make-sprite image:masha-happy 1000 'right 'masha 'happy 'casual))

(define bg:kitchen (%make-bg image:kitchen 1000))

(define scene-1
  (make-scene 'play
	      bg:kitchen
	      "Voluptatem vel dolor non magnam quae. Saepe quisquam laborum nulla consequuntur magni id ea. Recusandae accusantium enim libero sed voluptatum aut voluptatibus."
	      (list masha:happy)
	      music:curious_critters))

(define scene-2
  (make-scene 'play
	      bg:kitchen
	      "Это текст на русском."
	      (list masha:happy nastya:tired)
	      music:curious_critters))

(define scene-3
  (make-scene 'play
	      bg:kitchen
	      "Тут пусто и без звуков."
	      (list)
	      #f))

(define scene-4
  (make-scene 'play
	      bg:kitchen
	      "4 Сцена."
	      (list nastya:tired)
	      music:birds))

(define scene-5
  (make-scene 'play
	      bg:kitchen
	      "5 Сцена."
	      (list nastya:tired masha:happy)
	      music:birds))

(define END
  (make-scene 'play
	      (make-bg image:end)
	      ""
	      (list)
	      #f))

(define EMPTY-SCENE (empty-scene))
(define BEGIN (%empty-scene))
(define DATA (list BEGIN
		   scene-1
		   scene-2
		   scene-3
		   scene-4
		   scene-5
		   END))

(init DATA EMPTY-SCENE)
