(define-module (dom fullscreen)
  #:use-module (hoot ffi)
  #:use-module (dom document)
  #:export (request-fullscreen-element
            exit-fullscreen
            is-fullscreen?
            get-fullscreen-element
            toggle-fullscreen-stage))

;; Запросить полноэкранный режим для элемента
(define-foreign request-fullscreen-element
  "document" "requestFullscreen"
  (ref extern) -> none)

;; Выйти из полноэкранного режима
(define-foreign exit-fullscreen
  "document" "exitFullscreen"
  -> none)

;; Получить элемент в полноэкранном режиме
(define-foreign get-fullscreen-element
  "document" "fullscreenElement"
  -> (ref null extern))

;; Проверить, находимся ли в полноэкранном режиме (прямая проверка через JS)
(define-foreign %is-fullscreen-js
  "document" "isFullscreen"
  -> i32)

;; Проверить, находимся ли в полноэкранном режиме
(define (is-fullscreen?)
  (let ((result (%is-fullscreen-js)))
    (= result 1)))

;; Безопасный выход из полноэкранного режима
(define (safe-exit-fullscreen)
  (when (is-fullscreen?)
    (exit-fullscreen)))

;; Переключить полноэкранный режим для игрового контейнера
(define (toggle-fullscreen-stage)
  (pk "toggle-fullscreen-stage called")
  (if (is-fullscreen?)
      (begin
        (pk "Exiting fullscreen")
        (safe-exit-fullscreen))
      (let ((stage (get-element-by-id "stage")))
        (pk "Requesting fullscreen for stage:" stage)
        (request-fullscreen-element stage))))