(define-module (ren-sexp menu-draw)
  #:use-module (ice-9 match)
  #:use-module (dom canvas)
  #:use-module (ren-sexp menu)
  #:use-module (ren-sexp settings)
  #:export (draw-menu))

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
  (let* ((items (match current-menu
                  ('main '(("Continue" . continue)
                           ("Settings" . settings)
                           ("Credits" . credits)
                           ("Exit" . exit)))
                  ('settings '(("Text Speed" . text-speed)
                               ("Volume" . volume)
                               ("Fullscreen" . fullscreen)
                               ("Debug Info" . debug-info)
                               ("Back" . back)))
                  ('credits '(("Game Engine: Skim Skripter" . info)
                              ("Built with Guile Hoot" . info)
                              ("WebAssembly Technology" . info)
                              ("Back" . back)))
                  (_ '())))
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