(define-module (ren-sexp debug-info)
  #:use-module (dom canvas)
  #:use-module (hoot numbers)
  #:export (draw-fps draw-performance-info))

(define (draw-fps number context W H)
  (clear-rect context 0.0 0.0 W H)
  (fill-text
   context
   (string-append "FPS: " (number->string number))
   1700.0
   50.0))

(define (draw-performance-info fps update-time draw-time context W H)
  (clear-rect context 0.0 0.0 W H)
  ;; FPS
  (fill-text context
             (string-append "FPS: " (number->string fps))
             1700.0
             50.0)
  ;; Update time
  (fill-text context
             (string-append "Update: " (number->string (exact (round update-time))) "ms")
             1700.0
             100.0)
  ;; Draw time  
  (fill-text context
             (string-append "Draw: " (number->string (exact (round draw-time))) "ms")
             1700.0
             150.0)
  ;; Total time
  (fill-text context
             (string-append "Total: " (number->string (exact (round (+ update-time draw-time)))) "ms")
             1700.0
             200.0))
