(define-module (ren-sexp settings)
  #:use-module (dom storage)
  #:export (init-settings!))

(define (init-settings!)
  (let ((volume (get-item "volume"))
	(is-mute (get-item "is-mute")))
    (or volume (set-item! "volume" "1.0"))
    (or is-mute (set-item! "is-mute" "#f"))))
