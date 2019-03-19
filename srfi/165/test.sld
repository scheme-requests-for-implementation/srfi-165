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

(define-library (srfi 165 test)
  (export run-tests)
  (import (scheme base)
	  (srfi 64)
	  (srfi 165))
  (begin
    (define (run-tests)
      (test-begin "SRFI 165")

      (test-assert (not (eqv? (make-environment-variable)
			      (make-environment-variable))))

      (test-assert (make-environment))

      (test-eqv #f
	(let ((x (make-environment-variable)))
	  (environment-ref (make-environment) x)))

      (test-eqv 42
	(let ((x (make-environment-variable)))
	  (environment-ref (environment-update (make-environment) x 42)
			   x)))

      (test-eqv #f
	(let ((x (make-environment-variable))
	      (y (make-environment-variable)))
	  (environment-ref (environment-update (make-environment) x 42)
			   y)))

      (test-eqv #f
	(let ((x (make-environment-variable))
	      (env (make-environment)))
	  (environment-update env x 42)
	  (environment-ref env x)))

      (test-eqv 42
	(let ((x (make-environment-variable))
	      (env (make-environment)))
	  (environment-update! env x 42)
	  (environment-ref env x)))

      (test-eqv 42
	(let ((x (make-environment-variable))
	      (env (make-environment)))
	  (environment-update! env x 42)
	  (environment-update env x 10)
	  (environment-ref env x)))

      (test-eqv #f
	(let ((x (make-environment-variable))
	      (env (make-environment)))
	  (environment-update! (environment-update env x 10) x 42)
	  (environment-ref env x)))

      (test-eqv 42
	(let ((x (make-environment-variable))
	      (y (make-environment-variable))
	      (env (make-environment)))
	  (environment-update! (environment-update env y 10) x 42)
	  (environment-ref env x)))

      (test-eqv 42
	(let* ((x (make-environment-variable))
	       (env (environment-update (make-environment) x 42))
	       (copy (environment-copy env)))
	  (environment-update! env x 10)
	  (environment-ref copy x)))

      (test-eqv #f
	(let ((flag #f))
	  (make-computation
	   (lambda (compute)
	     (set! flag #t)))
	  flag))

      (test-eqv 42
	(run (make-computation
	      (lambda (compute)
		42))))

      (test-eqv 42
	(run (make-computation
	      (lambda (compute)
		(compute (pure 42))))))

      (test-equal '(10 42)
	(call-with-values
	    (lambda () (run (make-computation
			     (lambda (compute)
			       (compute (pure 10 42))))))
	  list))

      (test-equal '(42 (b a))
	(let* ((acc '())
	       (result
		(run (each (make-computation
			    (lambda (compute)
			      (set! acc (cons 'a acc))))
			   (make-computation
			    (lambda (compute)
			      (set! acc (cons 'b acc))
			      42))))))
	  (list result acc)))

      (test-equal 83
	(run (bind (pure 42)
		   (lambda (x)
		     (pure (* x 2)))
		   (lambda (x)
		     (pure (- x 1))))))

      (test-equal '(42 #f)
	(let ((x (make-environment-variable)))
	  (run (make-computation
		(lambda (compute)
		  (let ((a (compute
			    (local (lambda (env)
				     (environment-update env x 42))
			      (bind (ask)
				    (lambda (env)
				      (pure (environment-ref env x))))))))
		    (list a (environment-ref (compute (ask)) x))))))))

      (test-eqv 42
	(let ((x (make-environment-variable)))
	  (run (with ((x 42))
		 (fn ((y x))
		   (pure y))))))

      (test-eqv #f
	(let ((x (make-environment-variable)))
	  (run (each (with ((x 42))
		       (fn ((y x))
			 (pure y)))
		     (fn ((y x))
		       (pure y))))))

      (test-eqv 42
	(let ((x (make-environment-variable)))
	  (run (each (with! (x 42))
		     (fn ((y x))
		       (pure y))))))

      (test-eqv #f
	(let ((x (make-environment-variable)))
	  (run (forked (with! (x 42))
		       (fn ((y x))
			 (pure y))))))

      (test-eqv 42
	(run (with ((default-computation pure))
	       42)))


      (test-eqv 42
	(let ((x (make-environment-variable)))
	  (run (with ((x 10))
		 (with ((x 42))
		   (fn ((x x))
		     (pure x)))))))

      (test-end))))
