(define-module (ren-sexp update)
  #:use-module (ice-9 match)
  #:use-module (ice-9 atomic)
  #:use-module (dom window)
  #:use-module (dom document)
  #:use-module (ren-sexp text)
  #:use-module (ren-sexp bg)
  #:use-module (ren-sexp sprites)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp carret)
  #:use-module (ren-sexp music)
  #:use-module (dom media)
  #:use-module (ren-sexp keyboard)
  #:use-module (ren-sexp scene-utils)
  #:use-module (hoot ffi)
  #:use-module (fibers channels)
  #:use-module (ren-sexp draw)
  #:use-module (ren-sexp utils)
  #:export (init-update))

(define (inf-ttl? scene)
  (equal? (scene-ttl scene) 'inf))

(define (next-scene-increment next current)
  (match (list next current)
    ((($ <scene> state* bg* old-text*
	 text* sprites* music* carret* ttl*)
      ($ <scene> state  bg  old-text
	 text  sprites  music  carret  ttl))
     (cond
      ;; ((not (equal? (and music (music-path music))
      ;;               (and music* (music-path music*))))
      ;;  (cond
      ;;   ((and (music? music)
      ;;         (not music*))
      ;;    (media-pause (music-audio music))
      ;;    (scene-update-music current music*))

      ;;   ((and (not music)
      ;;         (music? music*))
      ;;    (media-play (set-current-volume (music-audio music*)))
      ;;    (set-media-loop! (music-audio music*) 1)
      ;;    (scene-update-music current music*))

      ;;   (else current)))

      ((not (same-bg? bg* bg))
       (next-bg next current))
      ((not (same-sprites? sprites* sprites))
       (next-sprites next current))
      ((not (same-old-text? old-text* old-text))
       (next-old-text next current))
      ((not (same-text? text* text))
       (next-string next current))
      ((and (number? ttl) (equal? ttl* (+ 1 ttl)))
       next)
      ((not (equal? ttl ttl*))
       (next-ttl next current))
      (else next)))))

(define (compute-next-state state-box scene next)
  (if (equal? scene next)
      (if (inf-ttl? next)
          scene
          (append-empty-scene! state-box scene next (make-scene)))
      (next-scene-increment next scene)))

(define (init-update state-box)
  ;; (pk 2)
  (define draw-function (init-draw-function state-box))
  ;; Переменные для профилирования
  (define *profile-counter* (make-parameter 0))
  (define *profile-last-time* (make-parameter 0))
  (define *update-time-sum* (make-parameter 0))
  (define *draw-time-sum* (make-parameter 0))
  (define *avg-update-time* (make-parameter 0))
  (define *avg-draw-time* (make-parameter 0))

  (define *last-frame-time* (make-parameter 0))

  (define (update-and-draw current-time)
    ;; (pk 3)
    ;; (pk (get-state))
    (define start-total current-time)

    ;; Этап обновления - используем current-time для точного измерения
    (define start-update current-time)
    (let* ((state (atomic-box-ref state-box))
           (scene (assoc-ref state 'current-scene))
           (next  (assoc-ref state 'current-story-scene)))
      (define next-scene (compute-next-state state-box scene next))
      ;; (pk state)
      (atomic-update-current-scene state-box next-scene)
      ;; (pk state)
      ;; (quit)
      )
    ;; Примерное время окончания update (добавляем небольшую оценку)
    (define end-update (+ start-update 0.1))

    ;; Этап отрисовки (сначала вызываем draw, а потом добавляем профилирование)
    (define start-draw end-update)
    (draw-function current-time (*avg-update-time*) (*avg-draw-time*))
    ;; Общее время кадра
    (define frame-time (- current-time (*last-frame-time*)))
    (*last-frame-time* current-time)

    ;; Сбор статистики - используем frame-time для более точного измерения
    (*profile-counter* (+ (*profile-counter*) 1))
    (*update-time-sum* (+ (*update-time-sum*) 0.1)) ; Примерная оценка
    (*draw-time-sum* (+ (*draw-time-sum*) (max 0 (- frame-time 0.1))))

    ;; Обновление средних значений раз в секунду
    (let ((curr-second (current-second))
          (last-time (*profile-last-time*)))
      (when (>= (- curr-second last-time) 1)
        (*profile-last-time* curr-second)
        (let ((frame-count (*profile-counter*)))
          (when (> frame-count 0)
            (*avg-update-time* (/ (*update-time-sum*) frame-count))
            (*avg-draw-time* (/ (*draw-time-sum*) frame-count))))
        ;; Сброс счётчиков
        (*profile-counter* 0)
        (*update-time-sum* 0)
        (*draw-time-sum* 0)))

    ;; (pk 5)
    (request-animation-frame update-and-draw-callback))
  (define update-and-draw-callback (procedure->external update-and-draw))
  update-and-draw-callback)
