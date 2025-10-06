(define-module (ren-sexp keyboard)
  #:use-module (ice-9 match)
  #:use-module (ice-9 atomic)
  #:use-module (dom event)
  #:use-module (dom document)
  #:use-module (hoot ffi)
  #:use-module (fibers channels)
  #:use-module (ren-sexp scene-utils)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp music)
  #:use-module (ren-sexp utils)
  #:use-module (ren-sexp settings)
  #:use-module (ren-sexp menu)
  #:use-module (ren-sexp main-menu)
  #:use-module (ren-sexp game-state)
  #:use-module (dom fullscreen)
  #:export (add-key-up-listener!
            add-click-listener!
            handle-interaction!
            start-game!
            set-game-script!))

;; Глобальная переменная для хранения скрипта игры
(define *game-script* '())

;; Установить скрипт игры
(define (set-game-script! script)
  (set! *game-script* script))

;; Начать игру
(define (start-game! state-box)
  (set-game-mode! 'game)
  ;; Устанавливаем состояние игры с первой сценой из скрипта
  (when (not (null? *game-script*))
    (atomic-box-set! state-box 
      `((current-scene . ,(make-scene))
        (current-story-scene . ,(car *game-script*))
        (counter . 0)))))

;; Универсальная функция для обработки взаимодействия (клик/клавиша)
(define (handle-interaction! state-box)
  (let* ((state (atomic-box-ref state-box))
         (current (assoc-ref state 'current-scene))
	 (remote  (assoc-ref state 'current-story-scene)))
    ;; Проверяем флаг автоматического полноэкранного режима
    (when (get-auto-fullscreen-on-first-interaction)
      (pk "Auto-activating fullscreen on first interaction")
      (toggle-fullscreen-stage)
      (set-auto-fullscreen-on-first-interaction! #f))  ; Отключаем после первого использования
    
    (if (equal? current remote)
	(append-empty-scene! state-box current remote (make-scene))
	(atomic-update-current-scene state-box remote))))

;; Обёртка для обратной совместимости
(define (complete-or-begin-new-scene! state-box)
  (handle-interaction! state-box))

(define (add-key-up-listener! state-box)
  (add-event-listener!
   (current-document)
   "keydown"
   (procedure->external
    (init-keyboard state-box))))

;; Обработка клавиш в главном меню
(define (handle-main-menu-keys key state-box)
  (match key
    ('ArrowUp (navigate-main-menu 'up))
    ('ArrowDown (navigate-main-menu 'down))
    ('Enter (handle-main-menu-selection state-box))
    ('KeyF (toggle-fullscreen-stage))
    ('F11 (toggle-fullscreen-stage))
    ('Backquote (toggle-debug-info!))
    (_ #t)))

;; Обработка выбора в главном меню
(define (handle-main-menu-selection state-box)
  (let ((action (select-main-menu-item)))
    (match action
      ('start-game (start-game! state-box))
      ('settings (set-game-mode! 'menu))  ; Переход в настройки
      ('credits (set-game-mode! 'menu))   ; Переход в credits
      ('exit (pk "Game exit requested"))
      (_ #t))))

;; Обработка клавиш в игровом меню
(define (handle-menu-keys key)
  (match key
    ('ArrowUp (navigate-menu 'up))
    ('ArrowDown (navigate-menu 'down))
    ('Enter (handle-menu-selection))
    ('Escape (set-game-mode! 'game)) ; Вернуться в игру
    ('KeyF (toggle-fullscreen-stage))
    ('F11 (toggle-fullscreen-stage))
    (_ #t)))

;; Обработка выбора в меню
(define (handle-menu-selection)
  (let ((action (select-menu-item)))
    (match action
      ('continue-game (set-game-mode! 'game))
      ('exit-game (set-game-mode! 'main-menu)) ; Вернуться в главное меню
      ('adjust-text-speed #t) ; Будет обрабатываться отдельно
      ('adjust-volume #t)     ; Будет обрабатываться отдельно
      ('toggle-fullscreen (toggle-fullscreen-stage))
      ('toggle-debug-info (toggle-debug-info!))
      (_ #t))))

;; Обработка клавиш в игре
(define (handle-game-keys key state-box)
  (match key
    ('Space	(complete-or-begin-new-scene! state-box))
    ('Escape (set-game-mode! 'menu)) ; Открыть меню
    ('Equal	(set-text-speed! (+ (get-text-speed) 0.1))) ; + увеличить скорость
    ('Minus	(set-text-speed! (- (get-text-speed) 0.1))) ; - уменьшить скорость
    ('BracketRight (set-volume! (+ (get-volume) 0.1))) ; ] увеличить громкость
    ('BracketLeft  (set-volume! (- (get-volume) 0.1))) ; [ уменьшить громкость
    ('KeyM	(toggle-mute!)) ; M - переключить звук
    ('Digit1	(set-text-speed! 0.5))  ; 1 - очень медленно
    ('Digit2	(set-text-speed! 1.0))  ; 2 - нормально
    ('Digit3	(set-text-speed! 1.5))  ; 3 - быстро
    ('Digit4	(set-text-speed! 2.0))  ; 4 - очень быстро
    ('KeyF	(pk "F key pressed, calling toggle-fullscreen-stage") (toggle-fullscreen-stage)) ; F - переключить полноэкранный режим
    ('F11	(pk "F11 key pressed, calling toggle-fullscreen-stage") (toggle-fullscreen-stage)) ; F11 - переключить полноэкранный режим
    ('ShiftF11 (begin ; Shift+F11 - переключить автоматический полноэкранный режим
                 (set-fullscreen-preference! (not (get-fullscreen-preference)))
                 (pk "Fullscreen preference toggled to:" (get-fullscreen-preference))))
    ('Backquote (toggle-debug-info!)) ; ` (тильда) - переключить дебаг информацию
    (_ #t)))

(define (init-keyboard state-box)
  (lambda (event)
    (let* ((key (string->symbol (keyboard-event-code event))))
      
      (cond
        ((is-in-main-menu?)
         ;; Обработка клавиш в главном меню
         (handle-main-menu-keys key state-box))
        ((is-in-menu?)
         ;; Обработка клавиш в игровом меню
         (handle-menu-keys key))
        (else
         ;; Обработка клавиш в игре
         (handle-game-keys key state-box))))))

;; Добавить обработчик кликов на stage
(define (add-click-listener! state-box)
  (let ((stage (get-element-by-id "stage")))
    (add-event-listener!
     stage
     "click"
     (procedure->external
      (lambda (event)
        (pk "Click detected, calling handle-interaction!")
        (handle-interaction! state-box))))))
