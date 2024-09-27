(import (demo assets)
	(guile)
	(ren-sexp scene)
	(ren-sexp rssl)
	(ren-sexp bg)
	(ren-sexp music)
	(ren-sexp sprites)
	(ren-sexp core))

(define WELCOME
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

(define END
  (make-scene #:bg (make-bg (image:end))))

(define script
  (->> (list WELCOME)
       (CLEAN)
       (BG	bg:kitchen)
       (BGM	music:curious_critters)
       (PAUSE	100)
       (JOIN	(list masha:happy nastya:tired))
       (TXT	"And here the water is ready.")

       (UPDATE	nastya:tired nastya:normal)
       (TXT	"My patience has ended!")

       (UPDATE	masha:happy masha:troubled)
       (TXT	"Wait a little bit longer, please.")
       
       (UPDATE	nastya:normal nastya:tired)
       (TXT     "I don't want to wait, pour it faster.")

       (UPDATE	masha:troubled masha:normal)
       (TXT     "No, a tea ceremony must be performed according to the rules.")

       (UPDATE	nastya:tired nastya:normal)
       (TXT     "These strange procedures of yours.. these bowls with the size of a kitten's paw.. and it watery sour tea... What is the point of all these bourgeois trinkets?! Our forefathers drank a tea from kettles in samovars!")

       (UPDATE	masha:normal masha:happy)
       (TXT     "Nobody ever drank a tea like that, as far as I know. A tea was brought to the Moscow Tsardom from China in the middle of the 17th century and was initially available only to an upper classes... And a date of creating the first samovar in the Russian Empire is marked as 1740. It was made, by the way, not far away, just three hours drive from there.")

       (UPDATE	nastya:normal nastya:tired)
       (TXT     "That's all bourgeois science. I believe only in my master – The Cold. The Cold is harsh but fair. At night, he whispers to me The Entire Truth of this world... If I forget to close a window properly.")

       (UPDATE	masha:happy masha:normal)
       (TXT     "I sincerely believe he will help you pass our exam.")

       (UPDATE	nastya:tired nastya:normal-ice)
       (TXT     "He is needed for prophecies. You're for exams. The world will plunge into darkness and servants of the Lord will smash everything living apart that stands in their way...")

       (UPDATE	nastya:normal-ice nastya:tired)
       (TXT     "...If we disturb this fragile balance.")

       (UPDATE	masha:normal masha:troubled)
       (TXT     "...You seem very tired.")

       (UPDATE	masha:troubled masha:happy)
       (TXT     "Fine. There will be a samovar. In a week, lets gather at an interesting place near the Yeltsin Center. For now... The water is exactly the right temperature. Today we will try my raspberry tea.")
       
       (JUST	END)))

(init (reverse script))
