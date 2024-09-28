(define-module (ren-sexp rssl)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp utils)
  #:export (BG BGM PAUSE JOIN CLEAN TXT +TXT JUST UPDATE ->>))

(define-syntax ->>
    (syntax-rules ()
        ; Each syntax rule is a pair: a pattern to match on, and an expression to
        ; rewrite to. This first list is the pattern: both args (`->>`, `value`)
        ; are just variables, which we could name however we want. Since the first
        ; part of the pattern matches the macro name, we bind it to `->>`, but `_`
        ; is another popular choice.
        ; So, this pattern matches, e.g., (->> 3)...
        ((->> value)
            ; ...and rewrites to the expression 3, our expected result
            value)

        ; Here we match a more complex pattern, where the second var is a list--
        ; that is, a function call. Note the ellipses, which gather multiple items
        ; into `args` and `rest`.
        ; This would match, e.g., (->> 3 (+ 1 2) (/ 3))...
        ((->> value (fn args ...) rest ...)
            ; ...and rewrite it to (->> (+ 1 2 3) (/ 3)), or (->> 6 (/ 3)).
            ; Note that we just accomplished the "thread-last" part!
            ; Obviously we're not at a final value yet: (->> 6 (/ 3)) will invoke
            ; the macro again. So, we "cycle through," evaluating incrementally,
            ; until we reach a final value.
            (->> (fn args ... value) rest ...))

        ; Finally, here we match a named function in the second position:
        ; e.g.. (->> 3 inc (/ 3))
        ((->> value fn rest ...)
            ; ...and pipe it through just like we did above: (->> 4 (/ 3))
            (->> (fn value) rest ...))))

(define (BG image scenes)
  (cons (scene-add-bg (car scenes) image) scenes))

(define (BGM music scenes)
  (cons (scene-add-music (car scenes) music) scenes))

(define (PAUSE time scenes)
  (cons (scene-update-ttl (car scenes) time) scenes))

(define (JOIN sprites scenes)
  (cons (scene-update-sprites (car scenes) sprites) scenes))

(define (UPDATE sprite sprite* scenes)
  (let* ((last-scene (car scenes))
	 (sprites (scene-sprites last-scene))
	 (sprites* (find-replace sprite sprite* sprites)))
    (cons
     (scene-update-ttl
      (scene-add-text
       (scene-update-text
	(scene-update-sprites last-scene sprites*)
	"")
       (list))
       10)
     scenes)))

(define (CLEAN scenes)
  (cons (make-scene #:ttl 2) scenes))

(define (TXT text scenes)
  (cons
   (scene-add-text
    (scene-update-text (car scenes) text)
    (list))
   scenes))

(define (+TXT text scenes)
  (cons
   (scene-add-text
    (scene-update-text (car scenes) text)
    (cons (scene-text (cadr scenes))
	  (scene-old-text (cadr scenes))))
   scenes))


(define (JUST scene scenes)
  (cons scene scenes))
