;;; guile scheme utilities for using json data

(define-module (json utils)
  #:export (json:parse json:unfold json:keys uf json:verbose-unfold))
(use-modules (json reader))

(define (json:parse in)
  (let ((res '()) (new '()))
    (while (not (eof-object? (peek-char in)))
	   (set! new (json:read-object in))
	   (if (not (or (null? new) (unspecified? new)))
	       (set! res (cons new res))))
    res))

;; helper to investigate deep structure of the json data
(define (json:keys h)
  "returns a list of the keys in hash table h"
  (let ((r '()))
    (hash-for-each-handle (lambda (x) (set! r (cons (car x) r))) h)
    (cons 'hash-table-with-keys: r)))

(define json:verbose-unfold #f)

(define (json:unfold . doll)
  "unfolds a nested matrioshka of data structures given a list of accessors"
  (case (length doll)
    ((0) (format json:verbose-unfold "NO DATA\n" 'no_data))
    ((1) (format json:verbose-unfold "INNERMOST: ~a\n" doll) (car doll))
    (else
     (format json:verbose-unfold "INSPECTING: ~a\n" doll)
     (let ((accessor (list-ref doll 1))
	   (structure (list-ref doll 0))
	   (matrioshka (list-tail doll 2)))
       (cond
	((hash-table? structure)
	 (apply json:unfold (hash-ref structure accessor) matrioshka))
	((list? structure)
	 (apply json:unfold (list-ref structure accessor) matrioshka))
	(#t doll))))))
