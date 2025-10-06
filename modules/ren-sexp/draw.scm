(define-module (ren-sexp draw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 atomic)
  #:use-module (dom document)
  #:use-module (dom canvas)
  #:use-module (dom element)
  #:use-module (dom window)
  #:use-module (ren-sexp text)
  #:use-module (ren-sexp bg)
  #:use-module (ren-sexp sprites)
  #:use-module (ren-sexp debug-info)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp scene-utils)
  #:use-module (ren-sexp carret)
  #:use-module (hoot numbers)
  #:use-module (fibers channels)
  #:use-module (hoot ffi)
  #:use-module (ren-sexp utils)
  #:use-module (ren-sexp settings)
  #:export (init-draw init-draw-function))

(define (make-2d-context elem)
  (let* ((canvas (get-element-by-id elem))
	 (context (get-context canvas "2d"))
	 (width    1920.0)
	 (height   1080.0))
    (set-element-width! canvas (exact width))
    (set-element-height! canvas (exact height))
    context))

;; Оптимизированная функция настройки текстового контекста
(define (setup-text-context! context)
  ;; Возвращаем эффекты для текста
  (set-fill-color! context "#ffffff")
  (set-border-color! context "black")
  (set-font! context "bold 45px PTSans")      ; ← вернули bold
  (set-text-align! context "left")
  ;; Возвращаем shadow эффекты:
  (set-shadow-blur! context 2)
  (set-shadow-color! context "rgba(0,0,0,0.3)")
  (set-shadow-offset-x! context 5)
  (set-shadow-offset-y! context 5)
  )

(define (init-draw state-box)
  (let ((bg-context (make-2d-context "all-canvas"))
	(gray-context (make-2d-context "gray-canvas"))
	(text-context (make-2d-context "text-canvas"))
	(debug-context (make-2d-context "debug-canvas"))
	(GW   1920.0)
	(GH   1080.0))
    ;; Настройка gray-context для полупрозрачного оверлея
    (set-fill-color! gray-context "black")
    (set-alpha! gray-context 0.3)
    (fill-rect gray-context 0 0 GW GH)
    
    ;; Оптимизированная настройка текстовых контекстов
    (setup-text-context! text-context)
    (setup-text-context! debug-context)
    (define *last-pos* (make-parameter 0))
    (define *fps-counter* (make-parameter 0))
    (define *last-time* (make-parameter 0))
    ;; Оптимизация: создаём объект каретки один раз
    (define *carret-obj* (make-carret ""))
    ;; Параметры анимации каретки
    (define *carret-animation-time* (make-parameter 0))
    (define *carret-fade-direction* (make-parameter 1)) ; 1 = fade in, -1 = fade out
    ;; Кэширование scene вычислений
    (define *cached-scene* (make-parameter #f))
    (define *cached-text* (make-parameter ""))
    (define *cached-old-text* (make-parameter '()))
    (define *cached-bg* (make-parameter #f))
    (define *cached-sprites* (make-parameter '()))
    (define *cached-reversed-old-text* (make-parameter '()))
    (define (draw prev-time)
      (let* ((state (atomic-box-ref state-box))
             (scene (assoc-ref state 'current-scene))
	     (next  (assoc-ref state 'current-story-scene)))
        ;; (pk state)
	(define completed? (equal? scene next))
	
	;; Кэширование: вычисляем scene данные только при изменении сцены
	(unless (equal? scene (*cached-scene*))
	  (*cached-scene* scene)
	  (*cached-text* (scene-text scene))
	  (*cached-old-text* (scene-old-text scene))
	  (*cached-bg* (scene-bg scene))
	  (*cached-sprites* (scene-sprites scene))
	  (*cached-reversed-old-text* (reverse (*cached-old-text*))))
	
	;; Используем кэшированные значения
	(define text (*cached-text*))
	(define old-text (*cached-old-text*))
	(define bg (*cached-bg*))
	(define sprites (*cached-sprites*))
	(*fps-counter* (+ (*fps-counter*) 1))
	(let ((curr-second (current-second))
	      (last-time (*last-time*)))
	  (when (>= (- curr-second last-time) 1)
	    (*last-time* curr-second)
	    (draw-fps (*fps-counter*) debug-context GW GH)
	    (*fps-counter* 0)))

	;; (unless completed?
	(draw-bg bg bg-context GW GH)
	(draw-sprites sprites bg-context)
	(clear-rect text-context 0.0 0.0 GW GH)
	(define p1
	  (draw-old-text (*cached-reversed-old-text*) text-context 50.0))
	(*last-pos* p1)
                                        ;)

	(let* ((p2&w2 (draw-text text text-context (*last-pos*)))
	       (p2 (car p2&w2))
	       (w2 (cdr p2&w2)))

	  (unless (equal? text "")
	    ;; Обновление анимации каретки
	    (let ((anim-time (*carret-animation-time*))
	          (direction (*carret-fade-direction*)))
	      ;; Увеличиваем время анимации (0.02 = ~50 FPS анимации)
	      (define new-time (+ anim-time (* direction 0.02)))
	      ;; Ограничиваем между 0 и 1
	      (cond
	        ((>= new-time 1.0) 
	         (*carret-animation-time* 1.0)
	         (*carret-fade-direction* -1)) ; Начинаем fade out
	        ((<= new-time 0.0)
	         (*carret-animation-time* 0.0)
	         (*carret-fade-direction* 1))  ; Начинаем fade in
	        (else
	         (*carret-animation-time* new-time)))
	      
	      ;; Отрисовка с альфа-каналом
	      (context-save! text-context)
	      (set-alpha! text-context (*carret-animation-time*))
	      (draw-carret *carret-obj*
	                   text-context p2 w2 completed?)
	      (context-restore! text-context)))
	  1
	  )
	1))
    (define draw-callback (procedure->external draw))
    draw-callback))

(define (init-draw-function state-box)
  (let ((bg-context (make-2d-context "all-canvas"))
	(gray-context (make-2d-context "gray-canvas"))
	(text-context (make-2d-context "text-canvas"))
	(debug-context (make-2d-context "debug-canvas"))
	(GW   1920.0)
	(GH   1080.0))
    ;; Настройка gray-context для полупрозрачного оверлея
    (set-fill-color! gray-context "black")
    (set-alpha! gray-context 0.3)
    (fill-rect gray-context 0 0 GW GH)
    
    ;; Оптимизированная настройка текстовых контекстов
    (setup-text-context! text-context)
    (setup-text-context! debug-context)
    (define *last-pos* (make-parameter 0))
    (define *fps-counter* (make-parameter 0))
    (define *last-time* (make-parameter 0))
    ;; Оптимизация: создаём объект каретки один раз
    (define *carret-obj* (make-carret ""))
    ;; Параметры анимации каретки
    (define *carret-animation-time* (make-parameter 0))
    (define *carret-fade-direction* (make-parameter 1)) ; 1 = fade in, -1 = fade out
    ;; Кэширование scene вычислений
    (define *cached-scene* (make-parameter #f))
    (define *cached-text* (make-parameter ""))
    (define *cached-old-text* (make-parameter '()))
    (define *cached-bg* (make-parameter #f))
    (define *cached-sprites* (make-parameter '()))
    (define *cached-reversed-old-text* (make-parameter '()))
    (define (draw prev-time update-time draw-time)
      (let* ((state (atomic-box-ref state-box))
             (scene (assoc-ref state 'current-scene))
	     (next  (assoc-ref state 'current-story-scene)))
        ;; (pk state)
	(define completed? (equal? scene next))
	
	;; Кэширование: вычисляем scene данные только при изменении сцены
	(unless (equal? scene (*cached-scene*))
	  (*cached-scene* scene)
	  (*cached-text* (scene-text scene))
	  (*cached-old-text* (scene-old-text scene))
	  (*cached-bg* (scene-bg scene))
	  (*cached-sprites* (scene-sprites scene))
	  (*cached-reversed-old-text* (reverse (*cached-old-text*))))
	
	;; Используем кэшированные значения
	(define text (*cached-text*))
	(define old-text (*cached-old-text*))
	(define bg (*cached-bg*))
	(define sprites (*cached-sprites*))
	(*fps-counter* (+ (*fps-counter*) 1))
	(let ((curr-second (current-second))
	      (last-time (*last-time*)))
	  (when (>= (- curr-second last-time) 1)
	    (*last-time* curr-second)
	    (draw-performance-info (*fps-counter*) update-time draw-time debug-context GW GH)
	    (*fps-counter* 0)))

	;; (unless completed?
	(draw-bg bg bg-context GW GH)
	(draw-sprites sprites bg-context)
	(clear-rect text-context 0.0 0.0 GW GH)
	(define p1
	  (draw-old-text (*cached-reversed-old-text*) text-context 50.0))
	(*last-pos* p1)
                                        ;)

	(let* ((p2&w2 (draw-text text text-context (*last-pos*)))
	       (p2 (car p2&w2))
	       (w2 (cdr p2&w2)))

	  (unless (equal? text "")
	    ;; Обновление анимации каретки
	    (let ((anim-time (*carret-animation-time*))
	          (direction (*carret-fade-direction*)))
	      ;; Увеличиваем время анимации (0.02 = ~50 FPS анимации)
	      (define new-time (+ anim-time (* direction 0.02)))
	      ;; Ограничиваем между 0 и 1
	      (cond
	        ((>= new-time 1.0) 
	         (*carret-animation-time* 1.0)
	         (*carret-fade-direction* -1)) ; Начинаем fade out
	        ((<= new-time 0.0)
	         (*carret-animation-time* 0.0)
	         (*carret-fade-direction* 1))  ; Начинаем fade in
	        (else
	         (*carret-animation-time* new-time)))
	      
	      ;; Отрисовка с альфа-каналом
	      (context-save! text-context)
	      (set-alpha! text-context (*carret-animation-time*))
	      (draw-carret *carret-obj*
	                   text-context p2 w2 completed?)
	      (context-restore! text-context)))
	  1
	  )
	1))
    draw))
