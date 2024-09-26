(define-module (dom storage)
  #:use-module (hoot ffi)
  #:export (get-item set-item!))


(define-foreign set-item!
  "storage" "setItem"
  (ref string) (ref string) -> none)

(define-foreign get-item
  "storage" "getItem"
  (ref string) -> (ref null string))
