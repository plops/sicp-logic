(defparameter *inverter-delay* 2)
(defparameter *and-gate-delay* 3)

; queue
; add to add end and delete add beginning
; (make-queue)                     -> new queue
; (insert-queue! queue item) ; insert in the back       
; (delete-queue! queue) ; deletes first thing
; (front-queue queue)
; (empty-queue? queue)
;; (cons front rear)
;; front = (cons 1 rear)
;; rear = (cons 2 nil)

;; adding new = (cons 3 nil)
;; change cdr rear to point to new
;; and rear to new
;; the first one is add front pointer
;; delete first item move front to 2 
; (set-car! <pair> <value>) ; i don't need that
; (set-cdr! <pair> <value>) 

(defun make-queue ()
  (let ((front (cons nil nil)))
   (cons front front)))
#+nil
(make-queue)

(defmacro front-queue (queue)
  `(car ,queue))
#+nil
(front-queue (make-queue))

(defmacro rear-queue (queue)
  `(cdr ,queue))

(defun empty-queue-p (queue)
  (null (car (front-queue queue))))
#+nil
(empty-queue-p (make-queue))

(defun insert-queue! (queue item)
  (if (empty-queue-p queue) 
      (setf (car (front-queue queue)) item)
      (let ((new (cons item nil)))
	(setf (cdr (rear-queue queue)) new
	      (rear-queue queue) new))))
#+nil
(let ((m (make-queue)))
  (dotimes (i 4) (insert-queue! m i))
  (front-queue m))

(defun delete-queue! (queue)
  (let* ((front (front-queue queue))
	(new-front (cdr front)))
    (if new-front
	(setf (front-queue queue) new-front)
	;; less than one element
	(setf (car front) nil))
    (car front)))
#+nil
(let ((m (make-queue)))
  (dotimes (i 4) (insert-queue! m i))
  (dotimes (i 4) (delete-queue! m))
  (insert-queue! m 2)
  (front-queue m))


(defparamater *the-agenda* (make-agenda))



; data structure for agenda
; (current-time agenda)
; empty-agenda?
; (add-to-agenda! time action agenda)
; first-item
; remove-first-item!
; (cons *agenda* (cons (cons 10 (make-queue)) (cons (cons 30 (make-queue)) nil)))
; add thing to time 10 -> just increase queue
; add thing at time 20 -> insert another segment between 10 and 30
;   change cddr of agenda to point to new cons and its cdr should point to the cdddr
; add thing at time 5 -> you need header cell for that
; deleting if queue empty ...



(defun call-each (procedures)
  (cond ((null procedures) 'done)
	(t (funcall (car procedures))
	   (call-each (cdr procedures)))))

(defun get-signal (wire) ; call dispatch that is returned when wire is created
  (funcall wire 'get-signal)) 

(defun set-signal! (wire new-value)
  (funcall (funcall wire 'set-signal!) new-value))

(defun add-action! (wire action-proc)
  (funcall (funcall wire 'add-action!) action-proc))

(defun after-delay (delay action)
  (add-to-agenda! (+ delay (current-time *the-agenda*))
		  action *the-agenda*))

(defun propagate ()
  (cond ((empty-agenda-p *the-agenda*) 'done)
	(t (funcall (first-item *the-agenda*))
	   (remove-first-item! *the-agenda*)
	   (propagate))))


(defun make-wire ()
  (let ((signal 0)
	(action-procs nil))
    (labels ((set-my-signal! (new)
	     (cond ((= signal new) 'done)
		   (t (setf signal new)
		      (call-each action-procs))))
	     (accept-action-proc (proc)
	       (setf action-procs (cons proc action-procs))
	     (funcall proc))
	     (dispatch (m)
	       (cond ((eq m 'get-signal) signal)
		     ((eq m 'set-signal!) #'set-my-signal!)
		     (t (error "Bad message ~a" m)))))
      #'dispatch)))


(defun logical-not (s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(t (error "invalid signal ~a" s))))

(defun inverter (in out)
  (flet ((inverter-in ()
	   (let ((new (logical-not (get-signal in))))
	     (after-delay *inverter-delay*
			  (lambda ()
			    (set-signal! out new))))))
    (add-action! in #'inverter-in)))

(defun and-gate (a1 a2 output)
  (flet ((and-action-procedure ()
	   (let ((new-value (logic-and (get-signal a1) (get-signal a2))))
	     (after-delay *and-gate-delay*
			  (lambda ()
			    (set-signal! output new-value))))))
    (add-action! a1 #'and-action-procedure)
    (add-action! a2 #'and-action-procedure)))

#+nil
(defun half-adder (a b s c)
  (let ((d (make-wire))
	(e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)))

#+nil
(defun full-adder (a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))