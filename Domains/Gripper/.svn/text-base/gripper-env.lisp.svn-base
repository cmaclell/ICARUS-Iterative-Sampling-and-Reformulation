; gripper-env.LISP
; Perceptions and actions for a simulated Gripper environment.

(defparameter initial1* nil)

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

(defun *pick-up (ball gripper)
  (let* ((object (car (member ball state* :key #'second)))
	 (holder (car (member gripper state* :key #'second)))
	 (rest (member 'in object))
	 (hrest (member 'holding holder)))
    (setf (cadr rest) nil)
    (setf (cadr hrest) ball)))

(defun *drop (ball room gripper)
  (let* ((object (car (member ball state* :key #'second)))
	 (holder (car (member gripper state* :key #'second)))
	 (rest (member 'in object))
	 (hrest (member 'holding holder)))
    (setf (cadr rest) room)
    (setf (cadr hrest) nil))
  )

(defun *move (robot to)
  (let* ((object (car (member robot state* :key #'second)))
	 (rest (member 'at object)))
    (setf (cadr rest) to)))