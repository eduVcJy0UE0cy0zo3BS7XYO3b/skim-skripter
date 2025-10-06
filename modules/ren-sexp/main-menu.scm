(define-module (ren-sexp main-menu)
  #:use-module (ice-9 match)
  #:use-module (ice-9 atomic)
  #:use-module (hoot records)
  #:use-module (ren-sexp utils)
  #:use-module (ren-sexp save-system)
  #:export (<main-menu-state>
            make-main-menu-state
            main-menu-state?
            main-menu-state-selected-item
            get-main-menu-state
            set-main-menu-state!
            navigate-main-menu
            select-main-menu-item
            get-main-menu-items))

;; Состояние главного меню
(define-record-type <main-menu-state>
  (make-main-menu-state selected-item)
  main-menu-state?
  (selected-item main-menu-state-selected-item))

;; Глобальное состояние главного меню
(define *main-menu-state* (make-main-menu-state 0))

;; Получить элементы главного меню (динамически в зависимости от наличия сохранений)
(define (get-main-menu-items)
  (if (has-saves?)
      '(("New Game" . new-game)
        ("Load Game" . load-game)
        ("Settings" . settings)
        ("Credits" . credits)
        ("Exit" . exit))
      '(("Start Game" . start-game)
        ("Settings" . settings)
        ("Credits" . credits)
        ("Exit" . exit))))

;; Получить состояние главного меню
(define (get-main-menu-state)
  *main-menu-state*)

;; Установить состояние главного меню
(define (set-main-menu-state! menu-state)
  (set! *main-menu-state* menu-state))

;; Навигация по главному меню
(define (navigate-main-menu direction)
  (let* ((items (get-main-menu-items))
         (current-index (main-menu-state-selected-item *main-menu-state*))
         (max-index (- (length items) 1))
         (new-index (match direction
                      ('up (if (> current-index 0)
                               (- current-index 1)
                               max-index))
                      ('down (if (< current-index max-index)
                                 (+ current-index 1)
                                 0))
                      (_ current-index))))
    (set! *main-menu-state* (make-main-menu-state new-index))))

;; Выбор элемента главного меню
(define (select-main-menu-item)
  (let* ((items (get-main-menu-items))
         (selected-index (main-menu-state-selected-item *main-menu-state*))
         (selected-item (list-ref items selected-index))
         (action (cdr selected-item)))
    action))