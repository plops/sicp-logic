(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

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

(defun make-agenda ()
  (cons 'agenda nil))
(defun empty-agenda-p (a)
  (null (cdr a)))
#+nil
(empty-agenda-p (make-agenda))
(defun segment-time (s)
  (car s))
(defun segment-queue (s)
  (cdr s))
(defun first-item (a) "Returns an action."
  (car (front-queue (segment-queue (cdr a)))))
(defun current-time (a)
  (segment-time (cdr a)))
(defun remove-first-item! (a)
  (unless (empty-agenda-p a)
   (let* ((q (segment-queue (cdr a)))
	  (e (delete-queue! q)))
     (when (empty-queue-p q)
       (setf (cdr a) (cddr a)))
     e)))
(defun add-to-agenda! (time action agenda)
  (flet ((insert (front rear)
	   (let* ((new (cons time 
			     (insert-queue! (make-queue)
					    action)))
		  (dispatch (cons new rear)))
	     (setf (cdr front) dispatch))))
   (if (empty-agenda-p agenda)
       (insert agenda nil)
       (dolist (e (cdr agenda))
	 (destructuring-bind (tim q) e
	   (format t "~a~%" (list 'tim tim 'q q 'time time))
	   (cond ((= tim time) 
		  ;; there is a segment for exactly the time we need
		  (insert-queue! q action))
		 ((and (cdr e)
		       (< tim time (cdar e)))
		  ;; we have to insert a new segment between e and its successor
		  (insert e (cddr e)))
		 ((and (null (cdr e))
		       (< tim time))
		  ;; insert a new segment at the end
		  (insert e nil))
		 ((< time tim)
		  ;; insert a new segment infront of e
		  ;; this can only happen at the front of the segment list
		  (assert (eq (cdr agenda) e))
		  (insert agenda e))
		 (t (error "didn't find a place to insert ~a"
			   (list time action)))))))))

#+nil
(let ((a (make-agenda)))
  (add-to-agenda! 10 'act1 a)
  (add-to-agenda! 20 'twenty a)
  a)

(defparamater *the-agenda* (make-agenda))

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
	   (let ((new-value (logandc2 (get-signal a1) (get-signal a2))))
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