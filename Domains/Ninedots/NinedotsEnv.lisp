;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; BLOCKENV.LISP
; Perceptions and actions for a simulated Blocks World environment.

; Initial-state for nine dots
(defparameter initial1*
      (list '(dot g1 xpos 0 ypos 0)
	    '(dot g2 xpos 0 ypos 1)
	    '(dot g3 xpos 0 ypos 2)
	    '(dot g4 xpos 1 ypos 0)
	    '(dot g5 xpos 1 ypos 1)
	    '(dot g6 xpos 1 ypos 2)
	    '(dot g7 xpos 2 ypos 0)
	    '(dot g8 xpos 2 ypos 1)
	    '(dot g9 xpos 2 ypos 2)
	    '(pen p1 xpos 0 ypos 0 status up)))
	    

;; Create a data structure that holds the state of the environment.
(setf initial* initial1*)

(defun task1 ()
  (setf initial* initial1*)
  (create-problems ((num-lines 4) 
		    (crossed g1)
		    (crossed g2)
		    (crossed g3)
		    (crossed g4)
		    (crossed g5)
		    (crossed g6)
		    (crossed g7)
		    (crossed g8)
		    (crossed g9))))
  
; Define a function that Icarus will call to initialize the environment.
(defun initialize-world ()
  (setq ink-num* 0)
  (setq state* (rcopy initial*))
  nil)

; Define a function that Icarus will call to update the environment.
(defun update-world () nil)

; Define a function that Icarus will call to update the perceptual buffer.
(defun preattend () (copy-list state*))

; Define an action for placing the pen down
(defun *place-pen-down (pen)
  ;;shouldn't we seach for the pen that has the name as the variable name? -CM
  (let* ((object (assoc 'pen state*))
	 (pen-xpos (member 'xpos object))
	 (pen-ypos (member 'ypos object))
	 (status (member 'status object)))
    (setf (cadr status) 'down)
    (push `(ink ,ink-num* xpos ,(cadr pen-xpos) ypos ,(cadr pen-ypos)) state*)
    (incf ink-num*)))

; Define an action for lifting the pen
(defun *lift-pen-up (pen)  (let* ((object (assoc 'pen state*))
	 (status (member 'status object)))
    (setf (cadr status) 'up)))

; Creates the line of ink dots between two points. Used by move-pen-to when the pen is down (on the page).
(defun draw-line (xpos1 ypos1 xpos2 ypos2 &optional (delta 1/10))
  (multiple-value-bind (roundedxpos xremainder) (round xpos1)
    (multiple-value-bind (roundedypos yremainder) (round ypos1)
      (push `(ink ,ink-num* xpos ,roundedxpos ypos ,roundedypos) state*)))
    (incf ink-num*)
  (cond ((< xpos1 xpos2)
	 (draw-line (+ xpos1 delta) (+ ypos2 (* (/ (- ypos2 ypos1) (- xpos2 xpos1)) (- (+ xpos1 delta) xpos2))) xpos2 ypos2))
	((> xpos1 xpos2)
	 (draw-line (- xpos1 delta) (+ ypos2 (* (/ (- ypos2 ypos1) (- xpos2 xpos1)) (- (- xpos1 delta) xpos2))) xpos2 ypos2))))             

; Define an action for moving the pen
(defun *move-pen-to (xpos2 ypos2)
  (let* ((object (assoc 'pen state*))
	 (xpos1 (member 'xpos object))
	 (ypos1 (member 'ypos object))
	 (status (member 'status object)))
    (if (equal (cadr status) 'down)
	(draw-line (cadr xpos1) (cadr ypos1) xpos2 ypos2))
    (setf (cadr xpos1) xpos2)
    (setf (cadr ypos1) ypos2)))

(defun *reset ()
  (initialize-world))
