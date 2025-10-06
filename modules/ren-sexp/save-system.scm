(define-module (ren-sexp save-system)
  #:use-module (ice-9 atomic)
  #:use-module (dom storage)
  #:use-module (dom image)
  #:use-module (dom media)
  #:use-module (ren-sexp scene)
  #:use-module (ren-sexp bg)
  #:use-module (ren-sexp music)
  #:use-module (ren-sexp sprites)
  #:use-module (ren-sexp game-state)
  #:use-module (ren-sexp settings)
  #:use-module (version)
  #:export (save-game
            load-game
            get-save-info
            has-saves?
            is-save-compatible?
            get-save-slots
            set-game-script!
            get-game-script
            extract-scene-recipe
            rebuild-scene-from-recipe))

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

;; Извлечь "рецепт" состояния из текущей сцены
(define (extract-scene-recipe scene)
  (let ((bg (scene-bg scene))
        (music (scene-music scene))
        (sprites (scene-sprites scene))
        (old-text (scene-old-text scene)))
    ;; Отладочный вывод
    (pk "=== EXTRACT SCENE RECIPE ===")
    (pk "BG:" bg)
    (when bg (pk "BG-PATH:" (bg-path bg)))
    (pk "MUSIC:" music)
    (when music (pk "MUSIC-PATH:" (music-path music)))
    (pk "SPRITES COUNT:" (length sprites))
    (when (not (null? sprites))
      (pk "FIRST SPRITE:" (car sprites))
      (pk "FIRST SPRITE PATH:" (sprite-path (car sprites))))
    
    `((background . ,(if (and bg (bg-path bg)) (bg-path bg) #f))
      (music . ,(if (and music (music-path music)) (music-path music) #f))
      (sprites . ,(filter (lambda (sprite-data) sprite-data)
                          (map (lambda (sprite)
                                 (let ((path (sprite-path sprite)))
                                   (if path
                                       `((path . ,path)
                                         (pos . ,(sprite-pos sprite))
                                         (character . ,(sprite-character sprite))
                                         (mood . ,(sprite-mood sprite))
                                         (wear . ,(sprite-wear sprite)))
                                       #f)))
                               sprites)))
      (accumulated_text . ,old-text))))

;; Пересоздать сцену из "рецепта"
(define (rebuild-scene-from-recipe recipe)
  (let* ((bg-path (assoc-ref recipe 'background))
         (music-path (assoc-ref recipe 'music))
         (sprites-data (assoc-ref recipe 'sprites))
         (old-text (assoc-ref recipe 'accumulated_text))
         
         ;; Пересоздаем объекты из путей
         (new-bg (if bg-path 
                     (make-bg-with-path (make-image bg-path) bg-path)
                     #f))
         (new-music (if music-path
                        (make-music (make-audio music-path) 100 music-path)
                        #f))
         (new-sprites (map (lambda (sprite-data)
                             (let ((path (assoc-ref sprite-data 'path))
                                   (pos (assoc-ref sprite-data 'pos))
                                   (character (assoc-ref sprite-data 'character))
                                   (mood (assoc-ref sprite-data 'mood))
                                   (wear (assoc-ref sprite-data 'wear)))
                               (make-sprite (make-image path) 1000 pos character mood wear path)))
                           sprites-data)))
    
    ;; Создаем новую сцену с восстановленным состоянием
    (make-scene-with-state new-bg new-music new-sprites old-text)))

;; Сохранить игру
(define (save-game slot-number state-box)
  (let* ((state (atomic-box-ref state-box))
         (current-scene (assoc-ref state 'current-scene))
         (current-story-scene (assoc-ref state 'current-story-scene))
         (counter (assoc-ref state 'counter)))
    
    (pk "=== SAVE GAME DEBUG ===")
    (pk "COUNTER:" counter)
    (pk "CURRENT-SCENE:" current-scene)
    (pk "CURRENT-STORY-SCENE:" current-story-scene)
    (pk "SCENES EQUAL?" (equal? current-scene current-story-scene))
    
    (let* ((scene-recipe (extract-scene-recipe current-scene))
           (save-data `((game_version . ,GAME_VERSION)
                        (counter . ,counter)
                        (scene_state . ,scene-recipe)
                        (timestamp . ,(current-timestamp)))))
      (set-item! (string-append "save_slot_" (number->string slot-number))
                 (call-with-output-string
                   (lambda (port) (write save-data port))))
      #t)))

;; Загрузить игру
(define (load-game slot-number state-box)
  (let ((save-data (get-save-info slot-number)))
    (when save-data
      (let ((counter (assoc-ref save-data 'counter))
            (scene-recipe (assoc-ref save-data 'scene_state)))
        ;; Переключаемся в игровой режим
        (set-game-mode! 'game)
        ;; Восстанавливаем сцену из рецепта
        (let ((restored-scene (if scene-recipe
                                  (rebuild-scene-from-recipe scene-recipe)
                                  (make-scene))))
          ;; Устанавливаем состояние игры с восстановленной сценой
          (let ((story-scene (if (and (< counter (length *game-script*))
                                      (>= counter 0))
                                 (list-ref *game-script* counter)
                                 (make-scene))))
            (atomic-box-set! state-box 
              `((current-scene . ,restored-scene)
                (current-story-scene . ,story-scene)
                (counter . ,counter)))
            (pk "LOADED SCENE:" restored-scene)
            (pk "STORY SCENE:" story-scene))
          ;; Останавливаем текущую музыку и запускаем новую если есть
          (stop-current-music!) ; Останавливаем любую играющую музыку
          (let ((music (scene-music restored-scene)))
            (when music
              (pk "STARTING MUSIC:" music)
              (pk "MUSIC AUDIO:" (music-audio music))
              (pk "MUSIC PATH:" (music-path music))
              (let ((audio-element (set-current-volume (music-audio music))))
                (media-play audio-element)
                (set-media-loop! audio-element 1)
                (pk "MUSIC STARTED")))))
        #t))))

