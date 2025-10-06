(define-module (ren-sexp save-system)
  #:use-module (ice-9 atomic)
  #:use-module (dom storage)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp game-state)
  #:use-module (version)
  #:export (save-game
            load-game
            get-save-info
            has-saves?
            is-save-compatible?
            get-save-slots
            set-game-script!
            get-game-script))

;; Получить информацию о сохранении
(define (get-save-info slot-number)
  (let ((save-str (get-item (string-append "save_slot_" (number->string slot-number)))))
    (if save-str
        (call-with-input-string save-str read)
        #f)))

;; Проверить есть ли хотя бы одно сохранение
(define (has-saves?)
  (or (get-save-info 1)
      (get-save-info 2) 
      (get-save-info 3)))

;; Проверка совместимости версий
(define (is-save-compatible? save-data)
  (let ((save-version (assoc-ref save-data 'game_version)))
    (string=? save-version GAME_VERSION)))

;; Получить состояние всех слотов сохранения
(define (get-save-slots)
  (map (lambda (slot)
         (let ((save-data (get-save-info slot)))
           (if save-data
               `((slot . ,slot)
                 (exists . #t)
                 (compatible . ,(is-save-compatible? save-data))
                 (timestamp . ,(assoc-ref save-data 'timestamp))
                 (version . ,(assoc-ref save-data 'game_version)))
               `((slot . ,slot)
                 (exists . #f)))))
       '(1 2 3)))

;; Глобальная переменная для хранения скрипта игры
(define *game-script* '())

;; Установить скрипт игры
(define (set-game-script! script)
  (set! *game-script* script))

;; Получить скрипт игры
(define (get-game-script)
  *game-script*)

;; Получить текущее время как число
(define (current-timestamp)
  (inexact->exact (round (current-time))))

;; Сохранить игру
(define (save-game slot-number state-box)
  (let* ((state (atomic-box-ref state-box))
         (counter (assoc-ref state 'counter))
         (save-data `((game_version . ,GAME_VERSION)
                      (counter . ,counter)
                      (timestamp . ,(current-timestamp)))))
    (set-item! (string-append "save_slot_" (number->string slot-number))
               (call-with-output-string
                 (lambda (port) (write save-data port))))
    #t))

;; Загрузить игру
(define (load-game slot-number state-box)
  (let ((save-data (get-save-info slot-number)))
    (when save-data
      (let ((counter (assoc-ref save-data 'counter)))
        ;; Переключаемся в игровой режим
        (set-game-mode! 'game)
        ;; Устанавливаем состояние игры
        (atomic-box-set! state-box 
          `((current-scene . ,(make-scene))
            (current-story-scene . ,(list-ref *game-script* counter))
            (counter . ,counter)))
        #t))))

