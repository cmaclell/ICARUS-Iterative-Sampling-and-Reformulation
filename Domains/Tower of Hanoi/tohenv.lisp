
; TOHENV.LISP - Simulated environment for Tower of Hanoi puzzle. 


(defvar state*)
(defvar initial*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; note: macros should be setf'able

(defmacro disk-form (disk)
  `(car (member ,disk state* :key #'second)))

(defmacro disk-xpos (disk)
  `(second (member 'xpos (disk-form ,disk))))

(defmacro disk-ypos (disk)
  `(second (member 'ypos (disk-form ,disk))))

(defmacro disk-height (disk)
  `(second (member 'height (disk-form ,disk))))

(defmacro disk-width (disk)
  `(second (member 'width (disk-form ,disk))))

(defmacro peg-form (peg)
  `(car (member ,peg state* :key #'second)))

(defmacro peg-xpos (peg)
  `(second (member 'xpos (peg-form ,peg))))

(defmacro peg-ypos (peg)
  `(second (member 'ypos (peg-form ,peg))))

;; this is the height of the base
(defmacro peg-height (peg)
  `(second (member 'height (peg-form ,peg))))

(defmacro peg-pegheight (peg)
  `(second (member 'pegheight (peg-form ,peg))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Executiable *move action
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun *move (disk from to)
  (let* ((from-xpos (peg-xpos from))
	 (to-xpos (peg-xpos to))
	 (disk-width (disk-width disk))
	 (from-top-disk (highest-disk-with-xpos from-xpos state*))
	 (to-top-disk (highest-disk-with-xpos to-xpos state*)))
    (cond ((not (eql disk from-top-disk))
	   (warn "Illegal move: Disk ~a is not the top of from peg ~a" disk from))
	  ((and to-top-disk
		(not (< disk-width (disk-width to-top-disk))))
	   (warn "Illegal move: Disk ~a is too large to move onto disk ~a of peg ~a" 
		 disk to-top-disk to))
	  (to-top-disk			; there is a disk on TO peg
	   (setf (disk-xpos disk) to-xpos
		 (disk-ypos disk) (+ (disk-ypos to-top-disk)
				     (disk-height to-top-disk))))
	  (t				; TO peg is empty
	   (setf (disk-xpos disk) to-xpos
		 (disk-ypos disk) (+ (peg-ypos to)
				     (peg-height to)))))))

(defun disk-list (state)
  (loop for form in state
     when (eql (car form) 'disk)
     collect (second form)))

(defun highest-disk-with-xpos (xpos state)
  (loop with max-ypos = 0
     with max-disk = nil 
     for disk in (disk-list state)
     for ypos = (disk-ypos disk)
     when (and (= xpos (disk-xpos disk))
	       (> ypos max-ypos))
     do
     (setf max-disk disk
	   max-ypos ypos)
     finally
       (return max-disk)))

(defun peg-list (state)
  (loop for form in state
     when (eql (car form) 'peg)
     collect (second form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface to Icarus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-world ()
  nil)

(defun preattend ()
  (copy-list state*))

(defun initialize-world ()
  (setf state*
	(rcopy initial*)))


; Specify descriptions for different configurations of disks. 


(defvar state2*)
(defvar state3*)
(defvar state4*)
(defvar state5*)

(setq state2*
      (list '(peg pegA xpos 10 ypos 0 width 1 height 1 pegheight 5)
            '(peg pegB xpos 20 ypos 0 width 1 height 1 pegheight 5)
            '(peg pegC xpos 30 ypos 0 width 1 height 1 pegheight 5)
            '(disk disk1 xpos 10 ypos 2 width 2 height 1)
            '(disk disk2 xpos 10 ypos 1 width 4 height 1)))

(setq state3*
      (list '(peg pegA xpos 10 ypos 0 width 1 height 1 pegheight 5)
            '(peg pegB xpos 20 ypos 0 width 1 height 1 pegheight 5)
            '(peg pegC xpos 30 ypos 0 width 1 height 1 pegheight 5)
            '(disk disk1 xpos 10 ypos 3 width 2 height 1)
            '(disk disk2 xpos 10 ypos 2 width 4 height 1)
            '(disk disk3 xpos 10 ypos 1 width 6 height 1)))

(setq state4*
      (list '(peg pegA xpos 10 ypos 0 width 1 height 1 pegheight 5)
            '(peg pegB xpos 20 ypos 0 width 1 height 1 pegheight 5)
            '(peg pegC xpos 30 ypos 0 width 1 height 1 pegheight 5)
            '(disk disk1 xpos 10 ypos 4 width 2 height 1)
            '(disk disk2 xpos 10 ypos 3 width 4 height 1)
            '(disk disk3 xpos 10 ypos 2 width 6 height 1)
            '(disk disk4 xpos 10 ypos 1 width 8 height 1)))

(setq state5*
      (list '(peg pegA xpos 10 ypos 0 width 1 height 1 pegheight 5)
            '(peg pegB xpos 20 ypos 0 width 1 height 1 pegheight 5)
            '(peg pegC xpos 30 ypos 0 width 1 height 1 pegheight 5)
            '(disk disk1 xpos 10 ypos 2 width 2 height 1)
            '(disk disk2 xpos 30 ypos 2 width 4 height 1)
            '(disk disk3 xpos 30 ypos 1 width 6 height 1)
            '(disk disk4 xpos 10 ypos 1 width 8 height 1)))

; Define functions for initializing the environment. 

(defun task2 ()
  (setq initial* state2*)
  (clear-goals)
  (create-problems ((on-peg disk1 pegc)
		    (on-peg disk2 pegc))))

(defun task3 ()
  (setq initial* state3*)
  (clear-goals)
  (create-problems ((on-peg disk1 pegc)
		    (on-peg disk2 pegc)
		    (on-peg disk3 pegc))))

(defun task4 ()
  (setq initial* state4*)
  (clear-goals)
  (create-problems ((on-peg disk1 pegc)
		    (on-peg disk2 pegc)
		    (on-peg disk3 pegc)
		    (on-peg disk4 pegc))))


(defun task5 ()
  (setq initial* state5*)
  (clear-goals)
  (create-problems ((on-peg disk1 pegc)
		    (on-peg disk2 pegc)
		    (on-peg disk3 pegc)
		    (on-peg disk4 pegc))))


; After you have loaded this file and your concept definitions, you
; can demonstrate their behavior on four different disk configurations
; by typing the commands: 
; 
; (task2)
; (grun)
; (task3)
; (grun)
; (task4)
; (grun)
; (task5)
; (grun)
;
; You will need to type (clear) to clear conceptual memory before 
; you load files for another domain. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PPRINT STATE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf pprint-state* t)

(defparameter pprint-state-data* nil)	; nil = Not Initialized Yet

(defun pre-compute-pprint-state-data (state)
  (let* ((disk-list (disk-list state))
	 (peg-list (peg-list state))
	 (max-disk-num (loop for disk in disk-list
			  maximize (disk-number disk))))
    (list max-disk-num
	  (loop for disk in disk-list
	       for disk-number = (disk-number disk)
	     collect (list disk
			   disk-number
			   (disk-string disk-number max-disk-num)))
	  (loop for peg in peg-list
	       collect (list peg (peg-xpos peg) (peg-string peg max-disk-num))))))

(defun pprint-state (&optional (state state*))
  (unless pprint-state-data*
    (setf pprint-state-data*
	  (pre-compute-pprint-state-data state*)))
  (terpri)
  (princ "STATE:")
  (let* ((max-disk-num (first pprint-state-data*))
	 (disk-data-list (second pprint-state-data*))
	 (peg-data-list (third pprint-state-data*))
	 (state-array (make-array (list (1+ max-disk-num)
					(length peg-data-list))
				  :initial-element 
				  (blank-string-of-length (+ 1 (* 2 max-disk-num)))))
	 (disk-lists-by-peg (loop for (peg xpos) in peg-data-list
			       collect (list xpos))))
    (loop for (disk disk-num) in disk-data-list
       for disk-xpos = (disk-xpos disk)
       do
       (push disk-num
	     (cdr (assoc disk-xpos disk-lists-by-peg)))
       )
    (loop for disk-list in disk-lists-by-peg
       do
       (setf (cdr disk-list)
	     (sort (cdr disk-list) #'>)))
					;(print disk-lists-by-peg)
    (loop for (peg-xpos . disks-on-peg) in disk-lists-by-peg
       do
       (loop with peg-col = (position peg-xpos peg-data-list :key #'second)
	  for disk-num in disks-on-peg
	  for row downfrom (1- max-disk-num)
	  do
	  (setf (aref state-array row peg-col)
		(loop for (disk num disk-string) in disk-data-list
		   when (= num disk-num)
		   return disk-string))
	  finally
	  (setf (aref state-array max-disk-num peg-col)
		(loop for (peg xpos peg-string) in peg-data-list
		   when (= xpos peg-xpos)
		   return peg-string))))
    (pprint-2d-array state-array)))

(defun pprint-2d-array (2d-array)
  (loop for row from 0 below (array-dimension 2d-array 0)
       do
       (terpri)
       (loop for col from 0 below (array-dimension 2d-array 1)
	    do
	    (format t "~a " (aref 2d-array row col)))))

(defun disk-number (disk)
  ;; assume disk symbol is DISKn where n is a single digit
  (read-from-string (subseq (format nil "~a" disk) 4)))

(defun disk-string (n max-disk)
  (let ((inner-string (blank-string-of-length (1- n)))
	(outer-string (blank-string-of-length (- max-disk n))))
    (format nil
	    "~a[~a~a~a]~a"
	    outer-string
	    inner-string
	    n
	    inner-string
	    outer-string)))

(defun peg-string (peg max-disk)
  (let* ((peg-name (format nil "~a" peg))
	 (peg-letter (subseq peg-name 3))
	 (dash-string (dashed-string-of-length max-disk)))
    (format nil
	    "~a~a~a"
	    dash-string
	    peg-letter
	    dash-string)))

(defun blank-string-of-length (length)
  (format nil (format nil "~~~da" length)
	  ""))

(defun dashed-string-of-length (length)
  (format nil (format nil "~~~d,1,0,'-a" length)
	  ""))


;;; FANCIER FORMAT?
;;;     
;;;   [1]                
;;;  [ 2 ]
;;; [  3  ]
;;; ---A---   ---B---   ---C---

  