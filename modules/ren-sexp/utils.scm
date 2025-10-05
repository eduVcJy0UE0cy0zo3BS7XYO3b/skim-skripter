(define-module (ren-sexp utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 atomic)
  #:use-module (hoot ffi)
  #:export (string-split
            find-replace
            set-data!
            atomic-update-current-scene
            atomic-append-scene))


(define DATA 1)

(define (set-data! data)
  (set! DATA data))


(define (atomic-update-current-scene state-box updated-scene)
  (let* ((state (atomic-box-ref state-box))
         (scene (assoc-ref state 'current-scene))
	 (next  (assoc-ref state 'current-story-scene))
         (counter  (assoc-ref state 'counter)))
    (define state* `((current-story-scene . ,next)
                     (current-scene . ,updated-scene)
                     (counter . ,counter)))
    (atomic-box-set! state-box state*)
    state*))



(define (atomic-append-scene state-box new-scene)
  (let* ((state (atomic-box-ref state-box))
         (counter  (assoc-ref state 'counter))
         (counter* (+ 1 counter)))
    (define state* `((current-story-scene . ,(list-ref DATA counter*))
                     (current-scene . ,new-scene)
                     (counter . ,counter*)))
    (atomic-box-set! state-box state*)
    new-scene))

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
