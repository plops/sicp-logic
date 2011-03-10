(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(defparameter *inverter-delay* 2)
(defparameter *and-gate-delay* 3)
(defparameter *or-gate-delay* 5)

(defun make-queue ()
  (cons nil nil))
#+nil
(make-queue)

(defmacro front-ptr (queue)
  `(car ,queue))
#+nil
(front-queue (make-queue))

(defmacro rear-ptr (queue)
  `(cdr ,queue))
;; a queue is empty when the front pointer is empty
(defun empty-queue-p (queue)
  (null (front-ptr queue)))
#+nil
(empty-queue-p (make-queue))

(defun front-queue (queue)
  "Select first item."
  (if (empty-queue-p queue)
      (error "Queue is empty.")
      (car (front-ptr queue))))

;; in an empty queue the front and rear pointer are set to item,
;; in a filled list the cdr of the last element and the rear ptr
(defun insert-queue! (queue item)
  (let ((new-pair (cons item nil)))
    (if (empty-queue-p queue) 
	  (setf (front-ptr queue) new-pair
		(rear-ptr queue) new-pair)
	  (setf (cdr (rear-ptr queue)) new-pair
		(rear-ptr queue) new-pair))
    queue))

#+nil
(let ((m (make-queue)))
  (dotimes (i 4) (insert-queue! m i))
  m)

(defun delete-queue! (queue)
  (when (empty-queue-p queue)
	(error "Queue is empty."))
  (setf (front-ptr queue) (cdr (front-ptr queue)))
  queue)
#+nil
(let ((m (make-queue)))
  (dotimes (i 4) (insert-queue! m i))
  (dotimes (i 3) (delete-queue! m))
  (insert-queue! m 2)
  m)


;; the agenda is made up of time segments. each segment is a pair of a
;; number and a queue. the queue holds the scheduled procedures
(defun make-time-segment (time queue)
  (cons time queue))
(defun segment-time (s) (car s))
(defun segment-queue (s) (cdr s))

;; the agenda is a 1d table of time segments, sorted in the order of
;; increasing time. in addition the current time (the time of the last
;; action that was processed) is stored at the head of the agenda.  a
;; newly constructed agenda has no time segments and a current time of
;; 0.

(defun make-agenda () (list 0))
(defun current-time (agenda) (car agenda))
(defun set-current-time! (agenda time)
  (setf (car agenda) time))
(defun segments (agenda)
  (cdr agenda))
(defun set-segments! (agenda segments)
  (setf (cdr agenda) segments))
(defun first-segment (agenda)
  (car (segments agenda)))
(defun rest-segments (agenda)
  (cdr (segments agenda)))
(defun empty-agenda-p (agenda)
  (null (segments agenda)))

;; for adding action to empty agenda create time-segment and attach
;; if agenda is non-empty, search a segment of appointed time and add action
;; to the associated queue 
;; if we reach a time later than the appointed, insert a new time-segment
;; if end of agenda is reached add a time-segment at its end

(defun add-to-agenda! (time action agenda)
  (labels ((belongs-before-p (segments)
	     (or (null segments)
		 (< time (segment-time (car segments)))))
	   (make-new-time-segment (time action)
	     (let ((q (make-queue)))
	       (insert-queue! q action)
	       (make-time-segment time q)))
	   (add-to-segments! (segments)
	     (if (= time (segment-time (car segments)))
		 (insert-queue! (segment-queue (car segments))
				action)
		 (let ((rest (cdr segments)))
		   (if (belongs-before-p rest)
		       (setf (cdr segments)
			     (cons (make-new-time-segment time action)
				 (cdr segments)))
		       (add-to-segments! rest))))))
    (let ((segments (segments agenda)))
      (if (belongs-before-p segments)
	  (set-segments! agenda
			 (cons (make-new-time-segment time action)
			       segments))
	  (add-to-segments! segments)))))

;; if queue time-segment is emptied, remove from list of segments 
(defun remove-first-agenda-item! (agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (when (empty-queue-p q)
      (set-segments! agenda (rest-segments agenda)))))

;; first item is at the head of the queue of first segment. update
;; current time, whenever an item is extracted
(defun first-agenda-item (agenda)
  (if (empty-agenda-p agenda)
      (error "Agenda is empty.")
      (let ((first-seg (first-segment agenda)))
	(set-current-time! agenda (segment-time first-seg))
	(front-queue (segment-queue first-seg)))))

(defparameter *the-agenda* (make-agenda))

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
  (unless (empty-agenda-p *the-agenda*)
    (funcall (first-agenda-item *the-agenda*))
    (remove-first-agenda-item! *the-agenda*)
    (propagate)))


(defun make-wire ()
  (let ((signal-value 0)
	(action-procedures '()))
    (labels ((set-my-signal! (new-value)
	       (unless (= signal-value new-value)
		 (setf signal-value new-value)
		 (call-each action-procedures)))
	     (accept-action-procedure! (procedure)
	       (setf action-procedures 
		     (cons procedure action-procedures))
	       (funcall procedure))
	     (dispatch (m)
	       (cond ((eq m 'get-signal) signal-value)
		     ((eq m 'set-signal!) #'set-my-signal!)
		     ((eq m 'add-action!) #'accept-action-procedure!)
		     (t (error "Bad message ~a" m)))))
      #'dispatch)))


(defun logical-not (s)
  (declare (type (integer 0 1) s))
  (if (= s 0) 1 0))

(defun inverter (in out)
  (flet ((inverter-in ()
	   (let ((new (logical-not (get-signal in))))
	     (after-delay *inverter-delay*
			  (lambda ()
			    (set-signal! out new))))))
    (add-action! in #'inverter-in)))

(defun logical-and (a b)
  (declare (type (integer 0 1) a b))
  (if (and (= a 1) (= b 1))
      1 0))

(defun and-gate (a1 a2 output)
  (flet ((and-action-procedure ()
	   (let ((new-value (logical-and (get-signal a1)
					 (get-signal a2))))
	     (after-delay *and-gate-delay*
			  (lambda ()
			    (set-signal! output new-value))))))
    (add-action! a1 #'and-action-procedure)
    (add-action! a2 #'and-action-procedure)))

(defun logical-or (a b)
  (declare (type (integer 0 1) a b))
  (if (or (= a 1) (= b 1))
      1 0))

(defun or-gate (a1 a2 output)
  (flet ((or-action-procedure ()
	   (let ((new-value (logical-or (get-signal a1)
					(get-signal a2))))
	     (after-delay *or-gate-delay*
			  (lambda ()
			    (set-signal! output new-value))))))
    (add-action! a1 #'or-action-procedure)
    (add-action! a2 #'or-action-procedure)))

(defun half-adder (a b s c)
  (let ((d (make-wire))
	(e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)))

(defun full-adder (a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))


(defun probe (name wire)
  (add-action! wire
	       (lambda ()
		 (format t "~a~%" (list name
					(current-time *the-agenda*)
					(get-signal wire))))))
(progn
  (setf *the-agenda* (make-agenda))
  (let ((i1 (make-wire))
	(i2 (make-wire))
	(sum (make-wire))
	(carry (make-wire)))
    (probe 'sum sum)
    (probe 'carry carry)
    (half-adder i1 i2 sum carry)
    (set-signal! i1 1)
    (propagate)))
