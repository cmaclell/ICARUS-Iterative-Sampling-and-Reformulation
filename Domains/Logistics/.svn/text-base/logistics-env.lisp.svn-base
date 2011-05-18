;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; logistics-env.LISP
; Perceptions and actions for a simulated Logistics environment.

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

(defun *load-package (package vehicle)
  (let* ((object (car (member package state* :key #'second)))
  	 (atrest (member 'at object))
	 (inrest (member 'inside object)))
    (setf (cadr atrest) nil)
    (setf (cadr inrest) vehicle)))

(defun *unload-package (package location)
  (let* ((object (car (member package state* :key #'second)))
  	 (atrest (member 'at object))
	 (inrest (member 'inside object)))
    (setf (cadr atrest) location)
    (setf (cadr inrest) nil)))

(defun *drive (truck to)
  (let* ((object (car (member truck state* :key #'second)))
	 (rest (member 'at object)))
    (setf (cadr rest) to)))

(defun *fly (plane to)
  (let* ((object (car (member plane state* :key #'second)))
	 (rest (member 'at object)))
    (setf (cadr rest) to)))