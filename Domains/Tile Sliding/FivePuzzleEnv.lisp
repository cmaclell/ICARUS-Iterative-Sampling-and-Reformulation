

;;; Five Puzzle

(defvar initial*)

; Create a data structure that holds the state of the environment.
;   . 2 1
;   5 4 3

(setq initial*
      (list '(cell C11 Row 1 Col 1 Right C12 Left NIL Up NIL Down C21)
	    '(cell C12 Row 1 Col 2 Right C13 Left C11 Up NIL Down C22)
	    '(cell C13 Row 1 Col 3 Right NIL Left C12 Up NIL Down C23)
	    '(cell C21 Row 2 Col 1 Right C22 Left NIL Up C11 Down NIL)
	    '(cell C22 Row 2 Col 2 Right C23 Left C21 Up C12 Down NIL)
	    '(cell C23 Row 2 Col 3 Right NIL Left C22 Up C13 Down NIL)
	    '(tile T1 position C13 number 1)
	    '(tile T2 position C12 number 2)
	    '(tile T3 position C23 number 3)
	    '(tile T4 position C22 number 4)
	    '(tile T5 position C21 number 5)
	    '(blank X position C11)
	    ))


; Define a function that Icarus will call to initialize the environment.
(defun initialize-world ()
  (setq state* (rcopy initial*))
  nil)

; Define a function that Icarus will call to update the environment.
(defun update-world () nil)

; Define a function that Icarus will call to update the perceptual buffer.
(defun preattend () (copy-list state*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Accessors / Setters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			
; note: macros should be setf'able

(defmacro cell-form (cell)
  `(car (member ,cell state* :key #'second)))

(defmacro cell-row (cell)
  `(second (member 'row (cell-form ,cell))))

(defmacro cell-col (cell)
  `(second (member 'col (cell-form ,cell))))

(defmacro cell-right (cell)
  `(second (member 'right (cell-form ,cell))))

(defmacro cell-left (cell)
  `(second (member 'left (cell-form ,cell))))

(defmacro cell-up (cell)
  `(second (member 'up (cell-form ,cell))))

(defmacro cell-down (cell)
  `(second (member 'down (cell-form ,cell))))


(defmacro tile-form (tile)
  `(car (member ,tile state* :key #'second)))

(defmacro tile-position (tile)
  `(second (member 'position (tile-form ,tile))))

(defmacro tile-number (tile)
  `(second (member 'number (tile-form ,tile))))

(defmacro blank-form ()
  `(car (member 'X state* :key #'second)))

(defmacro blank-position ()
  `(second (member 'position (blank-form))))


;;; aux functions

(defun neighbor (cell direction)
  (case direction
    (right (cell-right cell))
    (left (cell-right cell))
    (up (cell-up cell))
    (down (cell-down cell))
    (t nil)))

(defun tile-at-cell (cell)
  (loop for form in state*
       when (and (eql (first form) 'tile)
		 (eql (second (member 'position form))
		      cell))
       return (second form)))
       
(defun row-distance (from-cell to-cell)
  (abs (- (cell-row from-cell)
	  (cell-row to-cell))))

(defun col-distance (from-cell to-cell)
  (abs (- (cell-col from-cell)
	  (cell-col to-cell))))

(defun distance (from-cell to-cell)
  (+ (row-distance from-cell to-cell)
     (col-distance from-cell to-cell)))

(defun neighbors? (from-cell to-cell)
  (= 1 (distance from-cell to-cell)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slide Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;FIX THIS:
(defun *move-blank (to-cell)
  (let* ((from-cell (blank-position))
	 (tile (tile-at-cell to-cell)))
    (cond ((not (neighbors? from-cell to-cell))
	   (format t "~%*** The Blank's cell ~a is not a neighbor of the to-cell ~a"
		   from-cell to-cell))
	  (t
	   (setf (tile-position tile) from-cell
		 (blank-position) to-cell)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PPRINT STATE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf pprint-state* t)

(defun pprint-state (&optional (state state*))
  (terpri)
  (princ "STATE:")
  (let ((state-array (make-array (list 2 3)
				 :initial-element " . ")))
    (loop for form in state
       do
       (when (eql (first form) 'tile)
	 (let* ((tile-name (second form))
		(tile-cell (tile-position (second form)))
		(tile-row (1- (cell-row tile-cell)))
		(tile-col (1- (cell-col tile-cell))))
	   ;(print (list tile-name tile-cell tile-row tile-col))
	   (setf (aref state-array tile-row tile-col)
		 (format nil "[~a]" (tile-number tile-name))))))
    (pprint-2d-array state-array)))

(defun pprint-2d-array (2d-array)
  (loop for row from 0 below (array-dimension 2d-array 0)
       do
       (terpri)
       (loop for col from 0 below (array-dimension 2d-array 1)
	    do
	    (format t "~a " (aref 2d-array row col)))))

;;; FANCIER FORMAT?
;;;  [1] [2]  X   
;;;  [3] [4] [5]

  