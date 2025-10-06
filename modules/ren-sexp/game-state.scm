(define-module (ren-sexp game-state)
  #:use-module (ice-9 atomic)
  #:export (get-game-mode
            set-game-mode!
            is-in-menu?
            is-in-game?
            is-in-main-menu?))

;; Режимы игры
(define *game-mode* 'main-menu) ; 'main-menu, 'game, 'menu

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

(define (is-in-main-menu?)
  (eq? *game-mode* 'main-menu))