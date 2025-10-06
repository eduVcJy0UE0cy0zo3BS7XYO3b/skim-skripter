(define-module (ren-sexp menu-draw)
  #:use-module (ice-9 match)
  #:use-module (dom canvas)
  #:use-module (ren-sexp menu)
  #:use-module (ren-sexp main-menu)
  #:use-module (ren-sexp save-menu)
  #:use-module (ren-sexp settings)
  #:export (draw-menu draw-main-menu draw-save-menu))

;; Настройки отображения меню
(define MENU_BG_COLOR "rgba(0, 0, 0, 0.8)")
(define MENU_TEXT_COLOR "#ffffff")
(define MENU_SELECTED_COLOR "#ffff00")
(define MENU_TITLE_COLOR "#00ff00")

;; Отрисовка меню
(define (draw-menu context W H)
  (let* ((menu-state (get-menu-state))
         (current-menu (menu-state-current-menu menu-state))
         (selected-item (menu-state-selected-item menu-state)))
    
    ;; Полупрозрачный фон
    (set-fill-color! context MENU_BG_COLOR)
    (fill-rect context 0 0 W H)
    
    ;; Заголовок меню
    (set-fill-color! context MENU_TITLE_COLOR)
    (set-font! context "bold 48px PTSans")
    (set-text-align! context "center")
    
    (let ((title (match current-menu
                   ('main "MAIN MENU")
                   ('settings "SETTINGS")
                   ('credits "CREDITS")
                   (_ "MENU"))))
      (fill-text context title (/ W 2) 200))
    
    ;; Элементы меню
    (set-font! context "bold 32px PTSans")
    (draw-menu-items context W H current-menu selected-item)))

;; Отрисовка элементов меню
(define (draw-menu-items context W H current-menu selected-item)
  (let* ((items (get-current-menu-items))
         (start-y 300)
         (item-height 60))
    
    (let loop ((items-list items) (index 0))
      (when (not (null? items-list))
        (let* ((item (car items-list))
               (text (car item))
               (action (cdr item))
               (y-pos (+ start-y (* index item-height)))
               (is-selected (= index selected-item)))
          
          ;; Цвет текста
          (set-fill-color! context 
                           (if is-selected MENU_SELECTED_COLOR MENU_TEXT_COLOR))
          
          ;; Добавить дополнительную информацию для настроек
          (let ((display-text (match action
                                ('text-speed 
                                 (string-append text ": " (number->string (get-text-speed)) "x"))
                                ('volume 
                                 (let ((vol (get-volume))
                                       (muted (get-mute)))
                                   (string-append text ": " 
                                                  (if muted "MUTED" 
                                                      (string-append (number->string (inexact->exact (* vol 100))) "%")))))
                                ('fullscreen
                                 (string-append text ": " (if (get-fullscreen-preference) "ON" "OFF")))
                                ('debug-info
                                 (string-append text ": " (if (get-debug-info) "ON" "OFF")))
                                (_ text))))
            
            ;; Отрисовка текста
            (fill-text context display-text (/ W 2) y-pos)
            
            ;; Индикатор выбора
            (when is-selected
              (fill-text context ">" (- (/ W 2) 200) y-pos)
              (fill-text context "<" (+ (/ W 2) 200) y-pos)))
          
          (loop (cdr items-list) (+ index 1)))))))

;; Отрисовка главного меню
(define (draw-main-menu context W H)
  (let* ((menu-state (get-main-menu-state))
         (selected-item (main-menu-state-selected-item menu-state)))
    
    ;; Черный фон
    (set-fill-color! context "rgba(0, 0, 0, 1.0)")
    (fill-rect context 0 0 W H)
    
    ;; Заголовок игры
    (set-fill-color! context "#00ff00")
    (set-font! context "bold 72px PTSans")
    (set-text-align! context "center")
    (fill-text context "SKIM SKRIPTER" (/ W 2) 200)
    
    ;; Подзаголовок
    (set-fill-color! context "#ffffff")
    (set-font! context "bold 24px PTSans")
    (fill-text context "Visual Novel Engine" (/ W 2) 250)
    
    ;; Элементы меню
    (set-font! context "bold 36px PTSans")
    (draw-main-menu-items context W H selected-item)))

;; Отрисовка элементов главного меню
(define (draw-main-menu-items context W H selected-item)
  (let* ((items (get-main-menu-items))
         (start-y 400)
         (item-height 80))
    
    (let loop ((items-list items) (index 0))
      (when (not (null? items-list))
        (let* ((item (car items-list))
               (text (car item))
               (action (cdr item))
               (y-pos (+ start-y (* index item-height)))
               (is-selected (= index selected-item)))
          
          ;; Цвет текста
          (set-fill-color! context 
                           (if is-selected MENU_SELECTED_COLOR MENU_TEXT_COLOR))
          
          ;; Отрисовка текста
          (fill-text context text (/ W 2) y-pos)
          
          ;; Индикатор выбора
          (when is-selected
            (fill-text context ">" (- (/ W 2) 200) y-pos)
            (fill-text context "<" (+ (/ W 2) 200) y-pos))
          
          (loop (cdr items-list) (+ index 1)))))))

;; Отрисовка меню сохранений/загрузки
(define (draw-save-menu context W H menu-type)
  (let* ((menu-state (get-save-menu-state))
         (selected-item (save-menu-state-selected-item menu-state)))
    
    ;; Черный фон
    (set-fill-color! context "rgba(0, 0, 0, 1.0)")
    (fill-rect context 0 0 W H)
    
    ;; Заголовок
    (set-fill-color! context "#00ff00")
    (set-font! context "bold 48px PTSans")
    (set-text-align! context "center")
    (let ((title (match menu-type
                   ('load "LOAD GAME")
                   ('save "SAVE GAME")
                   (_ "SAVE/LOAD"))))
      (fill-text context title (/ W 2) 200))
    
    ;; Элементы меню
    (set-font! context "bold 32px PTSans")
    (draw-save-menu-items context W H menu-type selected-item)))

;; Отрисовка элементов меню сохранений
(define (draw-save-menu-items context W H menu-type selected-item)
  (let* ((items (get-save-menu-items menu-type))
         (start-y 350)
         (item-height 60))
    
    (let loop ((items-list items) (index 0))
      (when (not (null? items-list))
        (let* ((item (car items-list))
               (text (car item))
               (is-back (eq? (cadr item) 'back))
               (exists (if is-back #t (caddr item)))
               (compatible (if is-back #t (cadddr item)))
               (y-pos (+ start-y (* index item-height)))
               (is-selected (= index selected-item)))
          
          ;; Цвет текста в зависимости от состояния
          (set-fill-color! context 
                           (cond
                             (is-selected MENU_SELECTED_COLOR)
                             ((and exists (not compatible)) "#ff4444") ; красный для несовместимых
                             ((not exists) "#888888")                   ; серый для пустых
                             (else MENU_TEXT_COLOR)))                   ; белый для нормальных
          
          ;; Отрисовка текста
          (fill-text context text (/ W 2) y-pos)
          
          ;; Индикатор выбора
          (when is-selected
            (set-fill-color! context MENU_SELECTED_COLOR)
            (fill-text context ">" (- (/ W 2) 300) y-pos)
            (fill-text context "<" (+ (/ W 2) 300) y-pos))
          
          (loop (cdr items-list) (+ index 1)))))))