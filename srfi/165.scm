;; Copyright (C) Marc Nieper-Wißkirchen (2019).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define make-environment-variable
  (let ((count -1))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define variable-comparator
  (make-comparator integer? = < values))

(define default-computation
  (make-environment-variable))

(define-record-type <environment>
  (%make-environment global local)
  environment?
  (global environment-global)
  (local environment-local))

(define (make-environment)
  (%make-environment (hash-table variable-comparator)
		     (mapping variable-comparator)))

(define (environment-ref env var)
  (mapping-ref (environment-local env)
	       var
	       (lambda ()
		 (hash-table-ref/default (environment-global env) var #f))
	       unbox))

(define (environment-update env var val)
  (%make-environment (environment-global env)
		     (mapping-set (environment-local env) var (box val))))

(define (environment-update! env var val)
  (mapping-ref (environment-local env)
	       var
	       (lambda ()
		 (hash-table-set! (environment-global env) var val))
	       (lambda (cell)
		 (set-box! cell val))))

(define (environment-copy env)
  (let ((global (hash-table-copy (environment-global env) #t)))
    (mapping-for-each (lambda (var cell)
			(hash-table-set! global var (unbox cell)))
		     (environment-local env))
    (%make-environment global (mapping variable-comparator))))

(define (execute computation env)
  (let ((coerce (if (procedure? computation)
		    values
		    (or (environment-ref env default-computation)
			(error "not a computation" computation)))))
    ((coerce computation) env)))

(define (make-computation proc)
  (lambda (env)
    (proc (lambda (c) (execute c env)))))

(define (run computation)
  (execute computation (make-environment)))

(define (pure . args)
  (make-computation
   (lambda (compute)
     (apply values args))))

(define (each a . a*)
  (each-in-list (cons a a*)))

(define (each-in-list a*)
 (make-computation
   (lambda (compute)
     (let loop ((a (car a*)) (a* (cdr a*)))
       (if (null? a*)
	   (compute a)
	   (begin
	     (compute a)
	     (loop (car a*) (cdr a*))))))))

(define (bind a . f*)
  (make-computation
   (lambda (compute)
     (let loop ((a a) (f* f*))
       (if (null? f*)
	   (compute a)
	   (loop (call-with-values
		     (lambda () (compute a))
		   (car f*))
		 (cdr f*)))))))

(define (ask)
  (lambda (env)
    env))

(define (local updater computation)
  (lambda (env)
    (computation (updater env))))

(define-syntax fn
  (syntax-rules ()
    ((_ ((id var) ...) expr ... computation)
     (%fn ((id var) ...) () expr ... computation))))

(define-syntax %fn
  (syntax-rules ()
    ((_ () ((id var tmp) ...) expr ... computation)
     (let ((tmp var) ...)
       (bind (ask) (lambda (env)
		     (let ((id (environment-ref env tmp)) ...)
		       expr ...
		       computation)))))
    ((_ ((id var) . rest) (p ...) expr ... computation)
     (%fn rest (p ... (id var tmp)) expr ... computation))))

(define-syntax with
  (syntax-rules ()
    ((_ ((var val) ...) a* ... a)
     (%with ((var val) ...) () () a* ... a))))

(define-syntax %with
  (syntax-rules ()
    ((_ () ((var u val v) ...) ((a b) ...))
     (let ((u var) ... (v val) ... (b a) ...)
       (local (lambda (env)
		(let* ((env (environment-update env u v)) ...)
		  env))
	 (each b ...))))
    ((_ ((var val) . rest) (p ...) () a* ...)
     (%with rest (p ... (var u val v)) () a* ...))
    ((_ () p* (q ...) a . a*)
     (%with () p* (q ... (a b)) . a*))))

(define-syntax with!
  (syntax-rules ()
    ((_ (var val) ...)
     (%with! (var val) ... ()))))

(define-syntax %with!
  (syntax-rules ()
    ((_ ((var u val v) ...))
     (let ((u var) ... (v val) ...)
       (bind (ask) (lambda (env)
		     (environment-update! env u v) ...
		     (pure (if #f #f))))))
    ((_ (var val) r ... (p ...))
     (%with! r ... (p ... (var u val v))))))

(define (forked a . a*)
  (make-computation
   (lambda (compute)
     (let loop ((a a) (a* a*))
       (if (null? a*)
	   (compute a)
	   (begin
	     (compute (local (lambda (env) (environment-copy env)) a))
	     (loop (car a*) (cdr a*))))))))
