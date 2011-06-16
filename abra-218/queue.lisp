;;;; queue.lisp

;;; queue implementation taken from 
;;;
;;; Norvig, P. and Waters, R. C. 1991. Implementing queues in Lisp. 
;;; SIGPLAN Lisp Pointers IV, 4 (Oct. 1991), 2-8. 
;;; http://doi.acm.org/10.1145/1317265.1317266

(defun make-queue () (let ((q (list nil))) (cons q q))) 
(defun queue-elements (q) (cdar q))
(defun empty-queue-p (q) (null (cdar q)))
(defun queue-front (q) (cadar q))
(defun dequeue (q) (car (setf (car q) (cdar q))))
(defun enqueue (q item) (setf (cdr q) (setf (cddr q) (list item))))