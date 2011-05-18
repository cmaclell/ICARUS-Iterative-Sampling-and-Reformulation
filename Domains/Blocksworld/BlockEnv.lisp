;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; BLOCKENV.LISP
; Perceptions and actions for a simulated Blocks World environment.

; Initial-state for sussman's anomaly
(defparameter initial2*
      (list '(block A xpos 0 ypos 2 width 2 height 2)
	    '(block B xpos 2 ypos 2 width 2 height 2)
	    '(block C xpos 0 ypos 4 width 2 height 2)
	    '(marker FREE xpos 4 ypos 0 width 0 height 0)
	    '(table T1 xpos 0 ypos 0 width 20 height 2)
	    '(hand H1 status empty)))
	    
;; Default initial-state with 4 blocks
(defparameter initial4*
      (list '(block A xpos 0 ypos 2 width 2 height 2)
	    '(block B xpos 0 ypos 4 width 2 height 2)
	    '(block C xpos 0 ypos 6 width 2 height 2)
	    '(block D xpos 0 ypos 8 width 2 height 2)
	    '(marker FREE xpos 2 ypos 0 width 0 height 0)
	    '(table T1 xpos 0 ypos 0 width 20 height 2)
	    '(hand H1 status empty)))

; Alternate initial-state with only 3 blocks
(defparameter initial3*
      (list '(block A xpos 0 ypos 2 width 2 height 2)
	    '(block B xpos 0 ypos 4 width 2 height 2)
	    '(block C xpos 0 ypos 6 width 2 height 2)
	    '(marker FREE xpos 2 ypos 0 width 0 height 0)
	    '(table T1 xpos 0 ypos 0 width 20 height 2)
	    '(hand H1 status empty)))

;; Create a data structure that holds the state of the environment.
;;    Use the 4 block initial state by default
;;       Change to 3 block by (setf initial* initial3*)
(setf initial* initial4*)

(defun task1 ()
  (setf initial* initial2*)
  (create-problems ((on A B) (on B C))))
  
(defun task2 ()
  (setf initial* initial3*)
  (create-problems ((not (on ?any A)))))

; Define a function that Icarus will call to initialize the environment.
(defun initialize-world ()
  (setq state* (rcopy initial*))
  nil)

; Define a function that Icarus will call to update the environment.
(defun update-world () nil)

; Define a function that Icarus will call to update the perceptual buffer.
(defun preattend () (copy-list state*))

; Define an action for grasping a block.
(defun *grasp (block)
  (let* ((object (assoc 'hand state*))
	 (rest (member 'status object)))
;    (cond ((not (null atrace*))
;	   (terpri)(princ "Grasping ")(princ block)))
    (setf (cadr rest) block)))

; Define an action for ungrasping a block.
(defun *ungrasp (block)
  (let* ((object (assoc 'hand state*))
	 (rest (member 'status object)))
;    (cond ((not (null atrace*))
;	   (terpri)(princ "Ungrasping ")(princ block)))
    (setf (cadr rest) 'empty)))

; Define an action for moving a block vertically.
(defun *vertical-move (block ypos)
  (Let* ((object (car (member block state* :key #'second)))
	 (rest (member 'ypos object)))
;    (cond ((not (null atrace*))
;	   (terpri)(princ "Moving ")(princ block)
;	   (princ " to vertical position ")(princ ypos)))
    (setf (cadr rest) ypos)))

; Define an action for moving a block horizontally.
(defun *horizontal-move (block xpos)
  (let* ((object (car (member block state* :key #'second)))
	 (rest (member 'xpos object)))
;    (cond ((not (null atrace*))
;	   (terpri)(princ "Moving ")(princ block)
;	   (princ " to horizontal position ")(princ xpos)))
    (setf (cadr rest) xpos)))

; Update the free position marker
(defun *update-free (marker xpos)
  (let* ((object (car (member marker state* :key #'second)))
	 (rest (member 'xpos object)))
    (setf (cadr rest) xpos)))


;; support actions (to allow single action calls in defined primitive skills)

(defun *grasp-and-lift (block &optional (y 100))
  (progn
    (cond ((not (null atrace*))
	   (format t "~%Grasping and lifting block ~a to y = ~a" block y)))
    (*grasp block)
    (*vertical-move block y)))

(defun *put-down-at-x-y (block x y
			       ; if width supplied, use to *update-free
			       &optional width) 
  (progn
    (cond ((not (null atrace*))
	   (format t "~%Placing block ~a at position x = ~a, y = ~a" 
		   block x y)))
    (*horizontal-move block x)
    (*vertical-move block y)  	; original had (+ y h) instead of y
    (*ungrasp block)
    (when width
      (*update-free 'FREE (+ x width)))))

