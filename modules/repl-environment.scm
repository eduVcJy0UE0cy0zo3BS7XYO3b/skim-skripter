(library (repl-environment)
  (export repl-environment
	  story-in
	  story-out
	  state-in
	  state-out)
  (import (except (guile) - / = < <= = >= >)
	  (fibers channels)
	  (fibers)
	  (ice-9 match)
	  (ren-sexp scene)
          (prefix (only (guile) - / < <= = > >=) %)
          (only (hoot modules) current-module))

  ;; Some of the arithmetic and comparison operators are macros, which
  ;; don't work with Hoot's eval yet.  So, we use their identifier
  ;; syntax to residualize them to procedures here.
  (define - %-)
  (define / %/)
  (define < %<)
  (define <= %<=)
  (define = %=)
  (define >= %>=)
  (define > %>)

  (define story-in (make-channel))
  (define story-out (make-channel))
  (define state-in (make-channel))
  (define state-out (make-channel))

  (define box-in (make-channel))
  (define box-out (make-channel))

  (define (make-box in out default)
    (let lp ((old default))
      (match (get-message in)
	(#f (put-message out old))
	(data (lp data)))
      (lp old)))

  (define (get-current-scene)
    (put-message state-in #f)
    (get-message state-out))

  (define (get-next-scene)
    (put-message story-in 'next)
    (get-message story-out))
  
  (define (dumb)
    (define state-in (make-channel))
    (define state-out (make-channel))
    (spawn-fiber (lambda ()
		   (make-box state-in state-out (list (make-scene))))))
  
  (define (repl-environment)
    (current-module)))
