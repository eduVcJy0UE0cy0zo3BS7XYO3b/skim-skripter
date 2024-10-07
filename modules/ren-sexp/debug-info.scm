(define-module (ren-sexp debug-info)
  #:use-module (dom canvas)
  #:use-module (hoot numbers)
  #:export (draw-fps))

(define (draw-fps number context W H)
  (clear-rect context 0.0 0.0 W H)
  (fill-text
   context
   (string-append "FPS: " (number->string number))
   1700.0
   50.0))
