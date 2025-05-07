(use-modules
 (goblins)
 (hoot records)
 (ice-9 match))

(define-record-type <foo>
  (make-foo bar)
  foo?
  (bar foo-bar))


(match (list (make-foo 1) (make-foo 2))
  ((($ <foo> bar)
    ($ <foo> bar*))
   (pk (cons bar bar*))))
