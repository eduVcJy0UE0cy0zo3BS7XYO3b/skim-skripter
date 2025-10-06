(define-module (ren-sexp settings)
  #:use-module (dom storage)
  #:use-module (dom media)
  #:export (init-settings!
            get-text-speed
            set-text-speed!
            should-add-text-char?
            get-volume
            set-volume!
            get-mute
            set-mute!
            toggle-mute!
            set-current-audio!
            get-fullscreen-preference
            set-fullscreen-preference!
            get-auto-fullscreen-on-first-interaction
            set-auto-fullscreen-on-first-interaction!
            get-debug-info
            set-debug-info!
            toggle-debug-info!
            stop-current-music!
            get-current-audio))

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

;; Глобальная переменная для текущего аудио
(define *current-audio* #f)

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

;; Функции для работы с громкостью
(define (get-volume)
  (let ((volume-str (get-item "volume")))
    (if volume-str
        (string->number volume-str)
        1.0)))

(define (set-volume! volume)
  (let* ((clamped-volume (max 0.0 (min 1.0 volume)))
         ;; Округляем до 2 знаков после запятой чтобы избежать проблем с точностью
         (rounded-volume (/ (round (* clamped-volume 100)) 100)))
    (set-item! "volume" (number->string rounded-volume))
    ;; Обновить громкость активной музыки если не заглушено
    (unless (get-mute)
      (update-active-music-volume rounded-volume))))

(define (get-mute)
  (let ((mute-str (get-item "is-mute")))
    (if mute-str
        (string=? mute-str "#t")
        #f)))

(define (set-mute! muted)
  (set-item! "is-mute" (if muted "#t" "#f")))

(define (toggle-mute!)
  (let ((new-mute-state (not (get-mute))))
    (set-mute! new-mute-state)
    ;; Обновить громкость активной музыки
    (if new-mute-state
        (update-active-music-volume 0.0)  ; заглушить
        (update-active-music-volume (get-volume)))))

;; Функции для работы с текущим аудио
(define (set-current-audio! audio)
  (set! *current-audio* audio))

(define (update-active-music-volume volume)
  (when *current-audio*
    (set-media-volume! *current-audio* volume)))

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

;; Настройки отображения дебаг информации
(define (get-debug-info)
  (let ((debug-str (get-item "debug-info")))
    (if debug-str
        (string=? debug-str "#t")
        #f)))  ; По умолчанию выключено

(define (set-debug-info! enabled)
  (set-item! "debug-info" (if enabled "#t" "#f")))

(define (toggle-debug-info!)
  (set-debug-info! (not (get-debug-info))))

;; Получить текущее аудио
(define (get-current-audio)
  *current-audio*)

;; Остановить текущую музыку
(define (stop-current-music!)
  (when *current-audio*
    (media-pause *current-audio*)
    (set! *current-audio* #f)))
