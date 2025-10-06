(define-module (ren-sexp game-state)
  #:use-module (ice-9 atomic)
  #:export (get-game-mode
            set-game-mode!
            is-in-menu?
            is-in-game?))

;; Режимы игры
(define *game-mode* 'game) ; 'game, 'menu

;; Получить текущий режим игры
(define (get-game-mode)
  *game-mode*)

;; Установить режим игры
(define (set-game-mode! mode)
  (set! *game-mode* mode))

;; Проверки режима
(define (is-in-menu?)
  (eq? *game-mode* 'menu))

(define (is-in-game?)
  (eq? *game-mode* 'game))