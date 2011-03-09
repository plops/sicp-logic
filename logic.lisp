;; lecture 5b event-driven simulation, an example why assignments are useful
(defmacro define ((name &rest args) &body body)
  "I want to define procedures as they do in SICP."
  `(setf (fdefinition ',name)
	 (lambda ,args ,@body)))

(define (inverter in out)
  (define (inverter-in)
    (let ((new (logical-not (get-signal in))))
      (after-delay inverter-delay
		   (lambda ()
		     (set-signal! out new)))))
  (add-action! in inverter-in))

(define (logical-not s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(t (error "invalid signal ~a" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
	   (logical-and (get-signal a1)
			(get-signal a2))))
      (after-delay and-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure))

(define (make-wire)
  (let ((signal 0)
	(action-procs '()))
    (define (set-my-signal! new)
      (cond ((= signal new) 'done)
	    (t (setf signal new)
	       (call-each action-procs))))
    (define (accept-action-proc proc)
      (setf action-procs
	    (cons proc action-procs))
      (proc))
    (define (dispatch m)
      (cond ((eq m 'get-signal) signal)
	    ((eq m 'set-signal!) set-my-signal!)
	    (t (error "Bad message ~a" m))))
    dispatch))

(define (call-each procedures)
  (cond ((null procedures) 'done)
	(t ((car procedures))
	   (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-proc)
  ((wire 'add-action!) action-proc))

(define (after-delay delay action)
  (add-to-agenda!
   (+ delay (current-time the-agenda))
   action
   the-agenda))

(define (propagate)
  (cond ((empty-agenda? the-agenda) 'done)
	(t ((first-item the-agenda))
	   (remove-first-item! the-agenda)
	   (propagate))))

(defparameter the-agenda (make-agenda))
(defparameter inverter-delay 2)
(defparameter and-gate-delay 3)
(defparameter or-gate-delay 5)

; make-agenda 
; (current-time agenda)
; empty-agenda?
; (add-to-agenda! time action agenda)
; first-item
; remove-first-item

; implementation of queue

; church cons car cdr and assignment


;; 6a abelson stream processing
;; lazy evaluation

(defun cons-stream (x y)
  (cons x (delay y)))

(defun head (s)
  (car s))

(defun tail (s)
  (force (cdr s)))

(defun memo-proc (proc)
  "Remember result after first run."
  (let ((already-run-p nil)
	(result nil))
    (lambda ()
      (if (not already-run-p)
	  (progn
	    (setf result (proc)
		  already-run-p t)
	    result)
	  result))))

(defun delay (exp)
  "Promise to calculate something."
  (memo-proc (lambda () (exp))))

(defun force (proc)
  "Call an earlier promise."
  (proc))