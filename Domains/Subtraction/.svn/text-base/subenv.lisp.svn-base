
; Derived from
;    SUBENV.LISP - Simulated environment for multi-column subtraction

; Define the executable actions and supporting functions. 

(defvar state*)
(defvar initial*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; note: macros should be setf'able

(defmacro column-form (col)
  `(car (member ,col state* :key #'second)))

(defmacro column-xpos (col)
  `(second (member 'xpos (column-form ,col))))

(defmacro column-top (col)
  `(second (member 'top (column-form ,col))))

(defmacro column-bottom (col)
  `(second (member 'bottom (column-form ,col))))

(defmacro column-below (col)
  `(second (member 'below (column-form ,col))))

(defmacro column-decremented (col)
  `(second (member 'decremented (column-form ,col))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Executable Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FIND-DIFF

(defun *find-diff (col)
  (let* ((top (column-top col))
	 (bottom (column-bottom col)))
    (unless bottom
      (setf bottom 0))
    (setf (column-below col)
	  (- top bottom))))

; SUB-ONE

(defun *sub-one (col)
  (let* ((top (column-top col))
	 (decremented? (column-decremented col)))
    (cond ((< top 1)
	   (warn "Can't do *sub-one -- top is not positive"))
	  (decremented?
	   (warn "Can't do *sub-one -- already decremented"))
	  (t (decf (column-top col))
	     (setf (column-decremented col) t)))))

; ADD-TEN

(defun *add-ten (col)
  (incf (column-top col) 10))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface to Icarus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; UPDATE-WORLD does nothing, since only icarus' actions can alter
; the environment. 

(defun update-world ())

; PREATTEND

(defun preattend () 
  (copy-list state*))

(defun initialize-world ()
  (setf state*
	(copy-tree initial*)))

; Specify initial states for various subtraction problems

(defvar columns1*)
(defvar columns2*)
(defvar columns3*)
(defvar columns4*)
(defvar columns5*)

; 45 - 32 = 13

(setq columns1*
      (list 
       '(column C1 xpos 1 top 5 bottom 2 below nil decremented nil)
       '(column C2 xpos 2 top 4 bottom 3 below nil decremented nil)))

; 45 - 17 = 28

(setq columns2*
      (list 
       '(column C1 xpos 1 top 5 bottom 7 below nil decremented nil)
       '(column C2 xpos 2 top 4 bottom 1 below nil decremented nil)))

; 40 - 17 = 23

(setq columns3*
      (list 
       '(column C1 xpos 1 top 0 bottom 7 below nil decremented nil)
       '(column C2 xpos 2 top 4 bottom 1 below nil decremented nil)))

; 805 - 237 = 568

(setq columns4*
      (list 
       '(column C1 xpos 1 top 5 bottom 7 below nil decremented nil)
       '(column C2 xpos 2 top 0 bottom 3 below nil decremented nil)
       '(column C3 xpos 3 top 8 bottom 2 below nil decremented nil)))

; 2005 - 237 = 1768

(setq columns5*
      (list 
       '(column C1 xpos 1 top 5 bottom 7 below nil decremented nil)
       '(column C2 xpos 2 top 0 bottom 3 below nil decremented nil)
       '(column C3 xpos 3 top 0 bottom 2 below nil decremented nil)
       '(column C4 xpos 4 top 2 bottom nil below nil decremented nil)))

; Define functions for initializing the environment. 

(defun task1 ()
  (setq initial* (copy-tree columns1*)))

(defun task2 ()
  (setq initial* columns2*))

(defun task3 ()
  (setq initial* columns3*))

(defun task4 ()
  (setq initial* columns4*))

(defun task5 ()
  (setq initial* columns5*))

; After you have loaded this file and your concept definitions, you
; can demonstrate their behavior on five different subtraction tasks
; by typing the commands: 
;
; (task1)
; (grun)
; (task2)
; (grun)
; (task3)
; (grun)
; (task4)
; (grun)
; (task5)
; (grun)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PPRINT STATE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf pprint-state* t)

;; assumes column xpos's are consecutive 1 to n
(defun pprint-state (&optional (state state*))
  (terpri)
  (princ "STATE:")
  (let* ((column-count (length state))
	 (state-array (make-array (list 5 column-count)
				  :initial-element 
				  (blank-string-of-length 4)))
	 (column-list (mapcar #'second state*)))
    (loop for column-name in column-list
	 for col-xpos = (column-xpos column-name)
	 for array-col = (- column-count col-xpos)
       do
	 ;; decremented
	 (setf (aref state-array 0 array-col)
	       (if (column-decremented column-name)
		   "  D "
		   "    "))
	 ;; top
	 (setf (aref state-array 1 array-col)
	       (format nil "~3d "(column-top column-name)))
	 ;; bottom
	 (setf (aref state-array 2 array-col)
	       (if (column-bottom column-name)
		   (format nil "~3d "(column-bottom column-name))
		   "    "))
	 ;; subtraction line
	 (setf (aref state-array 3 array-col)
	       "----")
	 ;; below
	 (setf (aref state-array 4 array-col)
	       (if (column-below column-name)
		   (format nil "~3d "(column-below column-name))
		   "  ? "))
       )
    (pprint-2d-array state-array)))

(defun pprint-2d-array (2d-array)
  (loop for row from 0 below (array-dimension 2d-array 0)
       do
       (terpri)
       (loop for col from 0 below (array-dimension 2d-array 1)
	    do
	    ;; note omitting spaces between elements (to make dashed-line continuous)
	    (format t "~a" (aref 2d-array row col)))))

(defun blank-string-of-length (length)
  (format nil (format nil "~~~da" length)
	  ""))

;; not used, but could be used for subtraction line
(defun dashed-string-of-length (length)
  (format nil (format nil "~~~d,1,0,'-a" length)
	  ""))


;;; SAMPLE FORMAT
;;;     
;;;   1   4   2 
;;;       6   3
;;; ------------
;;;   ?   ?   ?
