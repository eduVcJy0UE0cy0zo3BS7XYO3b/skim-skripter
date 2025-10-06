(define-module (ren-sexp settings)
  #:use-module (dom storage)
  #:export (init-settings!
            get-text-speed
            set-text-speed!
            should-add-text-char?
            get-fullscreen-preference
            set-fullscreen-preference!
            get-auto-fullscreen-on-first-interaction
            set-auto-fullscreen-on-first-interaction!))

(define (init-settings!)
  (let ((volume (get-item "volume"))
	(is-mute (get-item "is-mute"))
        (text-speed (get-item "text-speed"))
        (fullscreen-preference (get-item "fullscreen-preference")))
    (or volume (set-item! "volume" "1.0"))
    (or is-mute (set-item! "is-mute" "#f"))
    (or text-speed (set-item! "text-speed" "1.0"))
    (or fullscreen-preference (set-item! "fullscreen-preference" "#t"))  ; По умолчанию включено
    
    ;; Если предпочтение полноэкранного режима включено, активируем флаг
    (when (get-fullscreen-preference)
      (set-auto-fullscreen-on-first-interaction! #t))))

;; Глобальный флаг для автоматического полноэкранного режима
(define *auto-fullscreen-on-first-interaction* #f)

(define (get-text-speed)
  (let ((speed-str (get-item "text-speed")))
    (if speed-str
        (string->number speed-str)
        1.0)))

(define (set-text-speed! speed)
  (let* ((clamped-speed (max 0.1 (min 2.0 speed)))
         (rounded-speed (/ (round (* clamped-speed 10)) 10)))  ; округляем до 1 знака
    (set-item! "text-speed" (number->string rounded-speed))))

;; Глобальный счетчик кадров для текста
(define *text-frame-counter* 0)

;; Определяет, нужно ли добавить символ в этом кадре
(define (should-add-text-char?)
  (set! *text-frame-counter* (+ *text-frame-counter* 1))
  (let* ((speed (get-text-speed))
         (frames-per-char (/ 1.0 speed))
         (should-add (>= *text-frame-counter* frames-per-char)))
    (when should-add
      (set! *text-frame-counter* 0))  ; сбрасываем счетчик
    should-add))  ; возвращаем true, если нужно добавить символ

;; Настройки предпочтения полноэкранного режима
(define (get-fullscreen-preference)
  (let ((pref-str (get-item "fullscreen-preference")))
    (if pref-str
        (string=? pref-str "#t")
        #t)))  ; По умолчанию включено

(define (set-fullscreen-preference! enabled)
  (set-item! "fullscreen-preference" (if enabled "#t" "#f")))

;; Функции для автоматического полноэкранного режима при первом взаимодействии
(define (get-auto-fullscreen-on-first-interaction)
  *auto-fullscreen-on-first-interaction*)

(define (set-auto-fullscreen-on-first-interaction! enabled)
  (set! *auto-fullscreen-on-first-interaction* enabled))
