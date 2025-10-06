(define-module (ren-sexp menu)
  #:use-module (ice-9 match)
  #:use-module (ice-9 atomic)
  #:use-module (hoot records)
  #:use-module (ren-sexp utils)
  #:export (<menu-state>
            make-menu-state
            menu-state?
            menu-state-current-menu
            menu-state-selected-item
            get-menu-state
            set-menu-state!
            navigate-menu
            select-menu-item))

;; Типы меню
(define-record-type <menu-state>
  (make-menu-state current-menu selected-item)
  menu-state?
  (current-menu menu-state-current-menu)
  (selected-item menu-state-selected-item))

;; Глобальное состояние меню
(define *menu-state* (make-menu-state 'main 0))

;; Определения меню
(define *main-menu-items* 
  '(("Continue" . continue)
    ("Settings" . settings)
    ("Credits" . credits)
    ("Exit" . exit)))

(define *settings-menu-items*
  '(("Text Speed" . text-speed)
    ("Volume" . volume)
    ("Fullscreen" . fullscreen)
    ("Debug Info" . debug-info)
    ("Back" . back)))

(define *credits-menu-items*
  '(("Game Engine: Skim Skripter" . info)
    ("Built with Guile Hoot" . info)
    ("WebAssembly Technology" . info)
    ("Back" . back)))

;; Получить текущие элементы меню
(define (get-current-menu-items)
  (match (menu-state-current-menu *menu-state*)
    ('main *main-menu-items*)
    ('settings *settings-menu-items*)
    ('credits *credits-menu-items*)
    (_ *main-menu-items*)))

;; Получить состояние меню
(define (get-menu-state)
  *menu-state*)

;; Установить состояние меню
(define (set-menu-state! menu-state)
  (set! *menu-state* menu-state))

;; Навигация по меню
(define (navigate-menu direction)
  (let* ((items (get-current-menu-items))
         (current-index (menu-state-selected-item *menu-state*))
         (max-index (- (length items) 1))
         (new-index (match direction
                      ('up (if (> current-index 0)
                               (- current-index 1)
                               max-index))
                      ('down (if (< current-index max-index)
                                 (+ current-index 1)
                                 0))
                      (_ current-index))))
    (set! *menu-state* 
          (make-menu-state (menu-state-current-menu *menu-state*) new-index))))

;; Выбор элемента меню
(define (select-menu-item)
  (let* ((items (get-current-menu-items))
         (selected-index (menu-state-selected-item *menu-state*))
         (selected-item (list-ref items selected-index))
         (action (cdr selected-item)))
    (match action
      ('continue 'continue-game)
      ('settings (set! *menu-state* (make-menu-state 'settings 0)) 'menu-changed)
      ('credits (set! *menu-state* (make-menu-state 'credits 0)) 'menu-changed)
      ('exit 'exit-game)
      ('back (set! *menu-state* (make-menu-state 'main 0)) 'menu-changed)
      ('text-speed 'adjust-text-speed)
      ('volume 'adjust-volume)
      ('fullscreen 'toggle-fullscreen)
      ('debug-info 'toggle-debug-info)
      (_ 'no-action))))