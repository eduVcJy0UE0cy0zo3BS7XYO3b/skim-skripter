;;; Copyright (C) 2024 Dmitry Polyakov <liltechdude@gmail.com>
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
;;; Madness begins there.
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
  (make-scene 'play (%make-bg image:black 0) "" (list) #f ""))

(define (%empty-scene)
  (make-scene 'play black-screen "Hello and Welcome to this new super visual novel engine!  Press <SPACE> to launch the game." (list) #f ""))

;; Assets
(define image:kitchen
  (make-image "resources/bg/bg_kitchen.jpeg"))

(define image:dummy-5.2K
  (make-image "resources/sprites/dummy.png"))
  
(define image:nastya-tired
  (make-image "resources/sprites/nastya_tired_1600.webp"))

(define image:nastya-normal
  (make-image "resources/sprites/nastya_normal_1600.png"))

(define image:nastya-normal-ice
  (make-image "resources/sprites/nastya_normal_1600_ice.png"))

(define image:masha-happy
  (make-image "resources/sprites/masha_happy_1600.webp"))

(define image:masha-normal
  (make-image "resources/sprites/masha_normal_1600.png"))

(define image:masha-troubled
  (make-image "resources/sprites/masha_troubled_1600.png"))

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

(define nastya:normal
  (make-sprite image:nastya-normal 1000 'left 'nastya 'normal 'casual))

(define nastya:normal-ice
  (make-sprite image:nastya-normal-ice 1000 'left 'nastya 'normal-ice 'casual))

(define masha:happy
  (make-sprite image:masha-happy 1000 'right 'masha 'happy 'casual))

(define masha:troubled
  (make-sprite image:masha-troubled 1000 'right 'masha 'troubled 'casual))

(define masha:normal
  (make-sprite image:masha-normal 1000 'right 'masha 'normal 'casual))

(define bg:kitchen (%make-bg image:kitchen 1000))

(define (chat-in-kitchen text chars)
  (make-scene 'play
	      bg:kitchen
	      text
	      chars
	      music:curious_critters
	      ""))

(define END
  (make-scene 'play
	      (make-bg image:end)
	      ""
	      (list)
	      #f
	      ""))

(define EMPTY-SCENE (empty-scene))
(define BEGIN (%empty-scene))
(define DATA (list BEGIN
		   (chat-in-kitchen "And here the water is ready." (list masha:happy nastya:tired))
		   (chat-in-kitchen "My patience has ended!" (list masha:happy nastya:normal))
		   (chat-in-kitchen "Wait a little bit longer, please." (list masha:troubled nastya:normal))
		   (chat-in-kitchen "I don't want to wait, pour it faster." (list masha:troubled nastya:tired))
		   (chat-in-kitchen "No, a tea ceremony must be performed according to the rules." (list masha:normal nastya:tired))
		   (chat-in-kitchen "These strange procedures of yours.. these bowls with the size of a kitten's paw.. and it watery sour tea... What is the point of all these bourgeois trinkets?! Our forefathers drank a tea from kettles in samovars!" (list masha:normal nastya:normal))
		   (chat-in-kitchen "Nobody ever drank a tea like that, as far as I know. A tea was brought to the Moscow Tsardom from China in the middle of the 17th century and was initially available only to an upper classes... And a date of creating the first samovar in the Russian Empire is marked as 1740. It was made, by the way, not far away, just three hours drive from there." (list masha:happy nastya:normal))
		   (chat-in-kitchen "That's all bourgeois science. I believe only in my master – The Cold. The Cold is harsh but fair. At night, he whispers to me The Entire Truth of this world... If I forget to close a window properly." (list masha:happy nastya:tired))
		   (chat-in-kitchen "I sincerely believe he will help you pass our exam." (list masha:normal nastya:tired))
		   (chat-in-kitchen "He is needed for prophecies. You're for exams. The world will plunge into darkness and servants of the Lord will smash everything living apart that stands in their way..." (list masha:normal nastya:normal-ice))
		   (chat-in-kitchen "...If we disturb this fragile balance." (list masha:normal nastya:tired))
		   (chat-in-kitchen "...You seem very tired." (list masha:troubled nastya:tired))
		   (chat-in-kitchen "Fine. There will be a samovar. In a week, lets gather at an interesting place near the Yeltsin Center. For now... The water is exactly the right temperature. Today we will try my raspberry tea." (list masha:happy nastya:tired))
		   END))

(init DATA EMPTY-SCENE)
