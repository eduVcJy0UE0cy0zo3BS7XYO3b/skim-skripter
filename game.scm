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
       (JUST	END)))

(init (reverse script))
