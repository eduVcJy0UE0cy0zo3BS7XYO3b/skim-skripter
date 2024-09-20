(import (demo assets)
	(scheme base)
	(ren-sexp scene)
	(ren-sexp bg)
	(ren-sexp music)
	(ren-sexp sprites)
	(ren-sexp core))

(define (welcome-scene)
  (make-scene
   #:bg (black-screen)
   #:text "Hello and Welcome to this new super visual novel engine!  Press <SPACE> to launch the game."))

(define music:curious_critters
  (make-music (audio:curious_critters) 100))

(define nastya:tired
  (make-sprite (image:nastya-tired) 1000 'left 'nastya 'tired 'casual))

(define nastya:normal
  (make-sprite (image:nastya-normal) 1000 'left 'nastya 'normal 'casual))

(define nastya:normal-ice
  (make-sprite (image:nastya-normal-ice) 1000 'left 'nastya 'normal-ice 'casual))

(define masha:happy
  (make-sprite (image:masha-happy) 1000 'right 'masha 'happy 'casual))

(define masha:troubled
  (make-sprite (image:masha-troubled) 1000 'right 'masha 'troubled 'casual))

(define masha:normal
  (make-sprite (image:masha-normal) 1000 'right 'masha 'normal 'casual))

(define bg:kitchen (%make-bg (image:kitchen) 1000))

(define (chat-in-kitchen text chars)
  (make-scene #:bg bg:kitchen
	      #:music music:curious_critters
	      #:text text
	      #:sprites chars))

(define END
  (make-scene #:bg (make-bg (image:end))))

(define script
  (list
   (welcome-scene)
   (chat-in-kitchen "" (list))
   (chat-in-kitchen "And here the water is ready."
		    (list masha:happy
			  nastya:tired))
   (chat-in-kitchen "My patience has ended!"
		    (list
		     masha:happy
		     nastya:normal))
   (chat-in-kitchen "Wait a little bit longer, please."
		    (list
		     masha:troubled
		     nastya:normal))
   (chat-in-kitchen "I don't want to wait, pour it faster."
		    (list
		     masha:troubled
		     nastya:tired))
   (chat-in-kitchen "No, a tea ceremony must be performed according to the rules."
		    (list
		     masha:normal
		     nastya:tired))
   (chat-in-kitchen "These strange procedures of yours.. these bowls with the size of a kitten's paw.. and it watery sour tea... What is the point of all these bourgeois trinkets?! Our forefathers drank a tea from kettles in samovars!"
		    (list
		     masha:normal
		     nastya:normal))
   (chat-in-kitchen "Nobody ever drank a tea like that, as far as I know. A tea was brought to the Moscow Tsardom from China in the middle of the 17th century and was initially available only to an upper classes... And a date of creating the first samovar in the Russian Empire is marked as 1740. It was made, by the way, not far away, just three hours drive from there."
		    (list
		     masha:happy
		     nastya:normal))
   (chat-in-kitchen "That's all bourgeois science. I believe only in my master – The Cold. The Cold is harsh but fair. At night, he whispers to me The Entire Truth of this world... If I forget to close a window properly."
		    (list
		     masha:happy
		     nastya:tired))
   (chat-in-kitchen "I sincerely believe he will help you pass our exam."
		    (list
		     masha:normal
		     nastya:tired))
   (chat-in-kitchen "He is needed for prophecies. You're for exams. The world will plunge into darkness and servants of the Lord will smash everything living apart that stands in their way..."
		    (list
		     masha:normal
		     nastya:normal-ice))
   (chat-in-kitchen "...If we disturb this fragile balance."
		    (list
		     masha:normal
		     nastya:tired))
   (chat-in-kitchen "...You seem very tired."
		    (list
		     masha:troubled
		     nastya:tired))
   (chat-in-kitchen "Fine. There will be a samovar. In a week, lets gather at an interesting place near the Yeltsin Center. For now... The water is exactly the right temperature. Today we will try my raspberry tea."
		    (list
		     masha:happy
		     nastya:tired))
   END))


(init script)
