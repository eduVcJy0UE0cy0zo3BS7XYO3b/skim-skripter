(define-module (ren-sexp save-menu)
  #:use-module (ice-9 match)
  #:use-module (hoot records)
  #:use-module (ren-sexp save-system)
  #:export (<save-menu-state>
            make-save-menu-state
            save-menu-state?
            save-menu-state-selected-item
            save-menu-state-menu-type
            get-save-menu-state
            set-save-menu-state!
            navigate-save-menu
            select-save-menu-item
            get-save-menu-items
            init-save-menu
            format-timestamp))

;; Состояние меню сохранений
(define-record-type <save-menu-state>
  (make-save-menu-state menu-type selected-item)
  save-menu-state?
  (menu-type save-menu-state-menu-type)     ; 'load или 'save
  (selected-item save-menu-state-selected-item))

;; Глобальное состояние меню сохранений
(define *save-menu-state* (make-save-menu-state 'load 0))

;; Функция для форматирования времени (простое отображение unix timestamp)
(define (format-timestamp timestamp)
  (if timestamp
      (let* ((total-seconds timestamp)
             (days (quotient total-seconds 86400))
             (remaining-seconds (remainder total-seconds 86400))
             (hours (quotient remaining-seconds 3600))
             (remaining-seconds (remainder remaining-seconds 3600))
             (minutes (quotient remaining-seconds 60)))
        (string-append 
          "Day " (number->string days) " "
          (if (< hours 10) "0" "") (number->string hours) ":"
          (if (< minutes 10) "0" "") (number->string minutes)))
      ""))

;; Инициализация меню сохранений с указанием типа
(define (init-save-menu menu-type)
  (set! *save-menu-state* (make-save-menu-state menu-type 0)))

;; Получить элементы меню сохранений
(define (get-save-menu-items menu-type)
  (let* ((slots (get-save-slots))
         ;; Найти самый свежий timestamp среди существующих сейвов
         (timestamps (map (lambda (slot-info)
                            (if (assoc-ref slot-info 'exists)
                                (assoc-ref slot-info 'timestamp)
                                0))
                          slots))
         (latest-timestamp (apply max (cons 0 timestamps))))
    (append
      (map (lambda (slot-info)
             (let ((slot (assoc-ref slot-info 'slot))
                   (exists (assoc-ref slot-info 'exists))
                   (compatible (assoc-ref slot-info 'compatible))
                   (timestamp (assoc-ref slot-info 'timestamp)))
               (if exists
                   (let* ((time-str (format-timestamp timestamp))
                          (is-latest (= timestamp latest-timestamp))
                          (slot-text (string-append "Slot " (number->string slot) 
                                                   (if compatible "" " [INCOMPATIBLE]")
                                                   " - " time-str))
                          ;; Выделяем самый свежий сейв жирным шрифтом
                          (display-text (if is-latest
                                            (string-append "**" slot-text "**")
                                            slot-text)))
                     (list display-text slot exists compatible))
                   (list (string-append "Slot " (number->string slot) ": Empty")
                         slot
                         exists
                         #t))))
           slots)
      '(("Back" back #t #t)))))

;; Получить состояние меню сохранений
(define (get-save-menu-state)
  *save-menu-state*)

;; Установить состояние меню сохранений
(define (set-save-menu-state! menu-state)
  (set! *save-menu-state* menu-state))

;; Навигация по меню сохранений
(define (navigate-save-menu direction)
  (let* ((menu-type (save-menu-state-menu-type *save-menu-state*))
         (items (get-save-menu-items menu-type))
         (current-index (save-menu-state-selected-item *save-menu-state*))
         (max-index (- (length items) 1))
         (new-index (match direction
                      ('up (if (> current-index 0)
                               (- current-index 1)
                               max-index))
                      ('down (if (< current-index max-index)
                                 (+ current-index 1)
                                 0))
                      (_ current-index))))
    (set! *save-menu-state* 
          (make-save-menu-state menu-type new-index))))

;; Выбор элемента в меню сохранений
(define (select-save-menu-item)
  (let* ((menu-type (save-menu-state-menu-type *save-menu-state*))
         (items (get-save-menu-items menu-type))
         (selected-index (save-menu-state-selected-item *save-menu-state*))
         (selected-item (list-ref items selected-index)))
    (if (eq? (cadr selected-item) 'back)
        'back
        `(,menu-type ,(cadr selected-item) ,(caddr selected-item) ,(cadddr selected-item)))))