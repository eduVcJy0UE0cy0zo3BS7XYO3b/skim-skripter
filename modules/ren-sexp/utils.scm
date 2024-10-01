(define-module (ren-sexp utils)
  #:use-module (hoot ffi)
  #:export (string-split
            find-replace))

(define (string-split char-delimiter? string)
  (define (maybe-add a b parts)
    (if (= a b) parts (cons (substring string a b) parts)))
  (let ((n (string-length string)))
    (let loop ((a 0) (b 0) (parts '()))
      (if (< b n)
          (if (not (char-delimiter? (string-ref string b)))
              (loop a (+ b 1) parts)
              (loop (+ b 1) (+ b 1) (maybe-add a b parts)))
          (reverse (maybe-add a b parts))))))

(define (last l)
  (cond ((null? (cdr l)) (car l))
        (else (last (cdr l)))))

(define (find-replace a b list)
  (cond
   ((null? list) '())
   ((list? (car list)) (cons (find-replace a b (car list)) (find-replace a b (cdr list))))
   ((eq? (car list) a) (cons b (find-replace a b (cdr list))))
   (else
    (cons (car list) (find-replace a b (cdr list))))))
