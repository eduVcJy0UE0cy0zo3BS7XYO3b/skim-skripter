(define-module (ren-sexp debug-info)
  #:use-module (dom canvas)
  #:use-module (hoot numbers)
  #:use-module (ren-sexp settings)
  #:use-module (dom fullscreen)
  #:export (draw-fps draw-performance-info draw-text-speed))

(define (draw-fps number context W H)
  (clear-rect context 1650.0 0.0 270.0 100.0)  ; очищаем только область FPS
  (set-fill-color! context "#00ff00")  ; зелёный цвет
  (set-font! context "bold 20px PTSans")
  (fill-text
   context
   (string-append "FPS: " (number->string number))
   1700.0
   50.0))

(define (draw-performance-info fps update-time draw-time context W H)
  (clear-rect context 0.0 0.0 W H)
  ;; Устанавливаем стили для debug текста
  (set-fill-color! context "#00ff00")  ; зелёный цвет
  (set-font! context "bold 20px PTSans")
  
  ;; Скорость текста (слева вверху)
  (let* ((speed (get-text-speed))
         (rounded-speed (/ (round (* speed 10)) 10))  ; округляем до 1 знака после запятой
         (speed-str (number->string rounded-speed))
         (frames-per-char (/ 1.0 speed)))
    (fill-text context
               (string-append "Text Speed: " speed-str "x (" (number->string (inexact->exact (round frames-per-char))) " frames/char)")
               50.0
               50.0)
    (fill-text context
               "Controls: 1-4 presets, +/- adjust, F fullscreen"
               50.0
               100.0)
    
    ;; Статус полноэкранного режима
    (fill-text context
               (string-append "Fullscreen: " (if (is-fullscreen?) "ON" "OFF"))
               50.0
               150.0)
    
    ;; Информация о громкости
    (let* ((volume (get-volume))
           (rounded-volume (* (round (* volume 100)) 1))  ; округляем до процентов
           (muted? (get-mute)))
      (fill-text context
                 (string-append "Volume: " (if muted? "MUTED" (string-append (number->string (inexact->exact rounded-volume)) "%")))
                 50.0
                 200.0)
      (fill-text context
                 "Controls: [/] volume, M mute"
                 50.0
                 250.0)))
  
  ;; Performance info (справа вверху)
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

(define (draw-text-speed context W H)
  ;; Очищаем только область для скорости текста
  (clear-rect context 0.0 0.0 800.0 150.0)
  ;; Устанавливаем стили для debug текста
  (set-fill-color! context "#00ff00")  ; зелёный цвет для хорошей видимости
  (set-font! context "bold 20px PTSans")
  (let* ((speed (get-text-speed))
         (rounded-speed (/ (round (* speed 10)) 10))  ; округляем до 1 знака после запятой
         (speed-str (number->string rounded-speed))
         (frames-per-char (/ 1.0 speed)))
    (fill-text context
               (string-append "Text Speed: " speed-str "x (" (number->string (inexact->exact (round frames-per-char))) " frames/char)")
               50.0
               50.0)
    (fill-text context
               "Controls: 1-4 presets, +/- adjust"
               50.0
               100.0)
    
    ;; Информация о громкости
    (let* ((volume (get-volume))
           (rounded-volume (* (round (* volume 100)) 1))  ; округляем до процентов
           (muted? (get-mute)))
      (fill-text context
                 (string-append "Volume: " (if muted? "MUTED" (string-append (number->string (inexact->exact rounded-volume)) "%")))
                 50.0
                 150.0)
      (fill-text context
                 "Controls: [/] volume, M mute"
                 50.0
                 200.0))))
