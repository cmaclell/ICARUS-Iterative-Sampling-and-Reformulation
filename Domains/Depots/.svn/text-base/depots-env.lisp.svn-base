;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; depot-env.LISP
; Perceptions and actions for a simulated Depots environment.

(defparameter initial1*
      (list '(depot depot01)
            '(depot depot02)
	    '(truck truck01 at depot01)
	    '(hoist hoist01 status crate01 at depot01)
	    '(hoist hoist02 status nil at depot02)
	    '(pallet pallet01 xpos 0 ypos 0 width 4 height 2 at depot01)
	    '(pallet pallet02 xpos 0 ypos 0 width 4 height 2 at depot02)
	    '(crate crate01 xpos 0 ypos 100 width 4 height 4 at depot01 inside nil)
	    '(crate crate02 xpos 0 ypos 2 width 4 height 4 at depot02 inside nil)
	    '(crate crate03 xpos 0 ypos 6 width 4 height 4 at depot02 inside nil)
	    '(crate crate04 xpos -1 ypos -1 width 4 height 4 at depot01 inside truck01)))

(defun task1 ()
  (setf initial* initial1*)
)

; Define a function that Icarus will call to initialize the environment.
(defun initialize-world ()
  (setq state* (rcopy initial*))
  nil)

; Define a function that Icarus will call to update the environment.
(defun update-world () nil)

; Define a function that Icarus will call to update the perceptual buffer.
(defun preattend () (copy-list state*))

; Define an action for grasping a crate.
(defun *grasp (hoist crate)
  (let* ((object (car (member hoist state* :key #'second)))
	 (rest (member 'status object)))
    (setf (cadr rest) crate)))

; Define an action for ungrasping a crate.
(defun *ungrasp (hoist crate)
  (let* ((object (car (member hoist state* :key #'second)))
	 (rest (member 'status object)))
    (setf (cadr rest) 'empty)))

; Define an action for moving a crate vertically.
(defun *vertical-move (crate ypos)
  (Let* ((object (car (member crate state* :key #'second)))
	 (rest (member 'ypos object)))
    (setf (cadr rest) ypos)))

; Define an action for moving a crate horizontally.
(defun *horizontal-move (crate xpos)
  (let* ((object (car (member crate state* :key #'second)))
	 (rest (member 'xpos object)))
    (setf (cadr rest) xpos)))

(defun *grasp-and-lift (hoist crate &optional (y 100))
  (progn
    (cond ((not (null atrace*))
	   (format t "~%Grasping and lifting crate ~a to y = ~a" crate y)))
    (*grasp hoist crate)
    (*vertical-move crate y)))

(defun *put-down-at-x-y (hoist crate x y)
  (cond ((not (null atrace*))
	 (format t "~%Placing crate ~a at position x = ~a, y = ~a" 
		 crate x y)))
  (*horizontal-move crate x)
  (*vertical-move crate y)
  (*ungrasp hoist crate))

(defun *load-crate (hoist crate truck)
  (let* ((object (car (member crate state* :key #'second)))
	 (xrest (member 'xpos object))
	 (yrest (member 'ypos object))
	 (inrest (member 'inside object)))
    (setf (cadr xrest) -1)
    (setf (cadr yrest) -1)
    (setf (cadr inrest) truck)
    (*ungrasp hoist crate)))

(defun *unload-crate (hoist crate)
  (let* ((object (car (member crate state* :key #'second)))
	 (xrest (member 'xpos object))
	 (yrest (member 'ypos object))
	 (inrest (member 'inside object)))
    (setf (cadr xrest) 0)
    (setf (cadr yrest) 100)
    (setf (cadr inrest) nil)
    (*grasp hoist crate)))

(defun *drive (truck to)
  (let* ((object (car (member truck state* :key #'second)))
	 (rest (member 'at object)))
    (setf (cadr rest) to)
    
    (loop for instance in (concept-instances (cassoc 'in cltm*))
	  when (member truck (cinstance-head instance))
	  do
	  ;;get the correponding belief for crate.
	  (setq object (car (member (second (cinstance-head instance))
				    state* :key #'second)))
	  (setq rest (member 'at object))
	  (setf (cadr rest) to))))