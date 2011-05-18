; PegEnv.lisp
; Perceptions and actions for a simulated Peg Solitaire environment.

(defvar initial*)
(defvar state*)

; Create a data structure that holds the state of the environment.
(setq initial*
      (list '(cell C11 Peg? T    Row 1 Col 1)
	    '(cell C12 Peg? NIL  Row 1 col 2)
	    '(cell C13 Peg? T    Row 1 Col 3)
	    '(cell C14 Peg? T    Row 1 Col 4)

	    '(cell C21 Peg? T    Row 2 Col 1)
	    '(cell C22 Peg? T    Row 2 Col 2)
	    '(cell C23 Peg? T    Row 2 Col 3)
	    '(cell C24 Peg? T    Row 2 Col 4)

	    '(cell C31 Peg? T    Row 3 Col 1)
	    '(cell C32 Peg? T    Row 3 Col 2)
	    '(cell C33 Peg? T    Row 3 Col 3)
	    '(cell C34 Peg? T    Row 3 Col 4)

	    '(cell C41 Peg? T    Row 4 Col 1)
	    '(cell C42 Peg? T    Row 4 Col 2)
	    '(cell C43 Peg? T    Row 4 Col 3)
	    '(cell C44 Peg? T    Row 4 Col 4)
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

(defmacro cell-peg? (cell)
  `(second (member 'peg? (cell-form ,cell))))

(defmacro cell-row (cell)
  `(second (member 'row (cell-form ,cell))))

(defmacro cell-col (cell)
  `(second (member 'col (cell-form ,cell))))


(defun neighbor-up? (cell neighbor)
  (and (= (cell-col cell)
	  (cell-col neighbor))
       (= (- (cell-row neighbor)
	     (cell-row cell))
	  -1)))

(defun neighbor-down? (cell neighbor)
  (and (= (cell-col cell)
	  (cell-col neighbor))
       (= (- (cell-row neighbor)
	     (cell-row cell))
	  1)))

(defun neighbor-right? (cell neighbor)
  (and (= (cell-row cell)
	  (cell-row neighbor))
       (= (- (cell-col neighbor)
	     (cell-col cell))
	  1)))

(defun neighbor-left? (cell neighbor)
  (and (= (cell-row cell)
	  (cell-row cell))
       (= (- (cell-col neighbor)
	     (cell-col cell))
	  -1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Jump actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Define an action for jumping a peg in a given direction
(defun *jump-up (from over to) 	
  (cond ((not (cell-peg? from))
	 (warn "Can't jump up. FROM cell ~a does not have a peg" from))
	((not (cell-peg? over))
	 (warn "Can't jump up. OVER cell ~a does not have a peg" over))
	((cell-peg? to)
	 (warn "Can't jump up. TO cell ~a is not empty" to))
	((not (neighbor-up? from over))
	 (warn "Can't jump up. OVER cell ~a is not an up-neighbor of FROM cell ~a"
	       over from))
	((not (neighbor-up? over to))
	 (warn "Can't jump up. TO cell ~a is not an up-neighbor of OVER cell ~a"
	       to over))
	(t
	 ;; do the jump
	 (setf (cell-peg? from) nil
	       (cell-peg? over) nil
	       (cell-peg? to)   t))))

(defun *jump-down (from over to)
  (cond ((not (cell-peg? from))
	 (warn "Can't jump down. FROM cell ~a does not have a peg" from))
	((not (cell-peg? over))
	 (warn "Can't jump down. OVER cell ~a does not have a peg" over))
	((cell-peg? to)
	 (warn "Can't jump down. TO cell ~a is not empty" to))
	((not (neighbor-down? from over))
	 (warn "Can't jump down. OVER cell ~a is not an down-neighbor of FROM cell ~a"
	       over from))
	((not (neighbor-down? over to))
	 (warn "Can't jump down. TO cell ~a is not an down-neighbor of OVER cell ~a"
	       to over))
	(t
	 ;; do the jump
	 (setf (cell-peg? from) nil
	       (cell-peg? over) nil
	       (cell-peg? to)   t))))

(defun *jump-right (from over to)
  (cond ((not (cell-peg? from))
	 (warn "Can't jump right. FROM cell ~a does not have a peg" from))
	((not (cell-peg? over))
	 (warn "Can't jump right. OVER cell ~a does not have a peg" over))
	((cell-peg? to)
	 (warn "Can't jump right. TO cell ~a is not empty" to))
	((not (neighbor-right? from over))
	 (warn "Can't jump right. OVER cell ~a is not a right-neighbor of FROM cell ~a"
	       over from))
	((not (neighbor-right? over to))
	 (warn "Can't jump right. TO cell ~a is not a right-neighbor of OVER cell ~a"
	       to over))
	(t
	 ;; do the jump
	 (setf (cell-peg? from) nil
	       (cell-peg? over) nil
	       (cell-peg? to)   t))))

(defun *jump-left (from over to)
  (cond ((not (cell-peg? from))
	 (warn "Can't jump left. FROM cell ~a does not have a peg" from))
	((not (cell-peg? over))
	 (warn "Can't jump left. OVER cell ~a does not have a peg" over))
	((cell-peg? to)
	 (warn "Can't jump left. TO cell ~a is not empty" to))
	((not (neighbor-left? from over))
	 (warn "Can't jump left. OVER cell ~a is not a left-neighbor of FROM cell ~a"
	       over from))
	((not (neighbor-left? over to))
	 (warn "Can't jump left. TO cell ~a is not a left-neighbor of OVER cell ~a"
	       to over))
	(t
	 ;; do the jump
	 (setf (cell-peg? from) nil
	       (cell-peg? over) nil
	       (cell-peg? to)   t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PPRINT STATE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pprint-state (&optional (state state*))
  (terpri)
  (princ "STATE:")
  (let ((state-array (make-array (list 4 4)
				 :initial-element " ")))
    (loop for form in state
       do
       (when (eql (first form) 'cell)
	 (let* ((cell-name (second form))
		(cell-row (1- (cell-row cell-name)))
		(cell-col (1- (cell-col cell-name))))
	   (setf (aref state-array cell-row cell-col)
		 (format nil "~a"
			 (if (cell-peg? cell-name)
			     "O"
			     "."))))))
    (pprint-2d-array state-array)))

(defun pprint-2d-array (2d-array)
  (loop for row from 0 below (array-dimension 2d-array 0)
       do
       (terpri)
       (loop for col from 0 below (array-dimension 2d-array 1)
	    do
	    (format t "~a " (aref 2d-array row col)))))

;;; EXAMPLE FORMAT
;;; STATE:
;;;  O . O O
;;;  O O O O
;;;  O O O O
;;;  O O O O


  