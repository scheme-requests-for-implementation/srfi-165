;; Copyright (C) Marc Nieper-Wißkirchen (2019).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software computation-without
;; restriction, including computation-without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", COMPUTATION-WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION COMPUTATION-WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-library (srfi 165 test)
  (export run-tests)
  (import (scheme base)
	  (srfi 64)
	  (srfi 165))
  (begin
    (define (run-tests)
      (test-begin "SRFI 165")

      (test-assert (not (eqv? (make-computation-environment-variable)
			      (make-computation-environment-variable))))

      (test-assert (make-computation-environment))

      (test-eqv #f
	(let ((x (make-computation-environment-variable)))
	  (computation-environment-ref (make-computation-environment) x)))

      (test-eqv 42
	(let ((x (make-computation-environment-variable)))
	  (computation-environment-ref
	   (computation-environment-update (make-computation-environment) x 42)
	   x)))

      (test-eqv #f
	(let ((x (make-computation-environment-variable))
	      (y (make-computation-environment-variable)))
	  (computation-environment-ref
	   (computation-environment-update (make-computation-environment) x 42)
	   y)))

      (test-eqv #f
	(let ((x (make-computation-environment-variable))
	      (env (make-computation-environment)))
	  (computation-environment-update env x 42)
	  (computation-environment-ref env x)))

      (test-eqv 42
	(let ((x (make-computation-environment-variable))
	      (env (make-computation-environment)))
	  (computation-environment-update! env x 42)
	  (computation-environment-ref env x)))

      (test-eqv 42
	(let ((x (make-computation-environment-variable))
	      (env (make-computation-environment)))
	  (computation-environment-update! env x 42)
	  (computation-environment-update env x 10)
	  (computation-environment-ref env x)))

      (test-eqv #f
	(let ((x (make-computation-environment-variable))
	      (env (make-computation-environment)))
	  (computation-environment-update!
	   (computation-environment-update env x 10) x 42)
	  (computation-environment-ref env x)))

      (test-eqv 42
	(let ((x (make-computation-environment-variable))
	      (y (make-computation-environment-variable))
	      (env (make-computation-environment)))
	  (computation-environment-update!
	   (computation-environment-update env y 10) x 42)
	  (computation-environment-ref env x)))

      (test-eqv 42
	(let* ((x (make-computation-environment-variable))
	       (env (computation-environment-update
		     (make-computation-environment) x 42))
	       (copy (computation-environment-copy env)))
	  (computation-environment-update! env x 10)
	  (computation-environment-ref copy x)))

      (test-eqv #f
	(let ((flag #f))
	  (make-computation
	   (lambda (compute)
	     (set! flag #t)))
	  flag))

      (test-eqv 42
	(computation-run (make-computation
			  (lambda (compute)
			    42))))

      (test-eqv 42
	(computation-run (make-computation
			  (lambda (compute)
			    (compute (computation-pure 42))))))

      (test-equal '(10 42)
	(call-with-values
	    (lambda ()
	      (computation-run (make-computation
				(lambda (compute)
				  (compute (computation-pure 10 42))))))
	  list))

      (test-equal '(42 (b a))
	(let* ((acc '())
	       (result
		(computation-run (computation-each (make-computation
						    (lambda (compute)
						      (set! acc (cons 'a acc))))
						   (make-computation
						    (lambda (compute)
						      (set! acc (cons 'b acc))
						      42))))))
	  (list result acc)))

      (test-equal '(42 (b a))
	(let* ((acc '())
	       (result
		(computation-run (computation-each-in-list
				  (list (make-computation
					 (lambda (compute)
					   (set! acc (cons 'a acc))))
					(make-computation
					 (lambda (compute)
					   (set! acc (cons 'b acc))
					   42)))))))
	  (list result acc)))

      (test-equal 83
	(computation-run (computation-bind (computation-pure 42)
			   (lambda (x)
			     (computation-pure (* x 2)))
			   (lambda (x)
			     (computation-pure (- x 1))))))

      (test-equal (list 42 84)
	(computation-run (computation-sequence (list (computation-pure 42)
						     (computation-pure 84)))))


      (test-equal '(42 #f)
	(let ((x (make-computation-environment-variable)))
	  (computation-run
	   (make-computation
	    (lambda (compute)
	      (let ((a (compute
			(computation-local
			    (lambda (env)
			      (computation-environment-update env x 42))
			  (computation-bind (computation-ask)
			    (lambda (env)
			      (computation-pure
			       (computation-environment-ref env x))))))))
		(list a (computation-environment-ref
			 (compute (computation-ask)) x))))))))

      (test-eqv 42
	(let ((x (make-computation-environment-variable)))
	  (computation-run (computation-with ((x 42))
			     (computation-fn ((y x))
			       (computation-pure y))))))

      (test-eqv 42
	(let ((x (make-computation-environment-variable)))
	  (computation-run (computation-with ((x 42))
			     (computation-fn (x)
			       (computation-pure x))))))

      (test-eqv #f
	(let ((x (make-computation-environment-variable)))
	  (computation-run (computation-each (computation-with ((x 42))
					       (computation-fn ((y x))
						 (computation-pure y)))
					     (computation-fn ((y x))
					       (computation-pure y))))))

      (test-eqv 42
	(let ((x (make-computation-environment-variable)))
	  (computation-run (computation-each (computation-with! (x 42))
					     (computation-fn ((y x))
					       (computation-pure y))))))

      (test-eqv #f
	(let ((x (make-computation-environment-variable)))
	  (computation-run (computation-forked (computation-with! (x 42))
					       (computation-fn ((y x))
						 (computation-pure y))))))

      (test-equal (list #f 2)
	(let ((x (make-computation-environment-variable)))
	  (computation-run
	   (computation-bind/forked (computation-each
				      (computation-with! (x 42))
				      (computation-pure 2))
				    (lambda (z)
				      (computation-fn ((y x))
					(computation-pure (list y z))))))))

      (test-eqv 42
	(computation-run
	 (computation-with ((default-computation computation-pure))
	   42)))


      (test-eqv 42
	(let ((x (make-computation-environment-variable)))
	  (computation-run (computation-with ((x 10))
			     (computation-with ((x 42))
			       (computation-fn ((x x))
				 (computation-pure x)))))))

      (test-end))))
