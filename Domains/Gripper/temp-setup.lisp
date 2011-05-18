#|
(setq initial1* (list '(room room01)
		      '(room room02)
		      '(ball ball01 in room01)
		      '(robot robot01 at room01)
		      '(gripper gripper01 attached-to robot01 holding nil)))

(setq initial1* (list '(room room01)
		      '(room room02)
		      '(ball ball01 in room01)
		      '(ball ball02 in room01)
		      '(robot robot01 at room01)
		      '(gripper gripper01 attached-to robot01 holding nil)
		      '(gripper gripper02 attached-to robot01 holding nil)))
|#

(defparameter norooms 0)
(defparameter noballs 0)
(defparameter nogrippers 0)
(defparameter nogoals 0)

(defun get-parameters()
  (format t "~%Enter the number of rooms :")
  (setq norooms (read))

  (format t "~%Enter the number of balls :")
  (setq noballs (read))

  (format t "~%Enter the number of grippers :")
  (setq nogrippers (read))
  
  (format t "~%Enter the number of goals :")
  (setq nogoals (read)))

(get-parameters)

(setq initial1* (gripper-prob-generator norooms noballs nogrippers))
(task1)
(let ((goal-list (generate-goals nogoals initial*)))
  (eval (cons 'create-problems (list goal-list))))
;;(run 1)