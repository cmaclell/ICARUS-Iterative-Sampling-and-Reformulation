#|
(setq initial1*
	       (list '(location loc01 type airport city city01)
		     '(location loc02 type other city city01)
		     '(location loc03 type airport city city02)
		     '(location loc04 type other city city02)
		     '(truck t01 at loc02)
		     '(truck t02 at loc03)
		     '(airplane a01 at loc01)
		     '(package p01 at loc02 inside nil)))
|#

(defparameter nocities 0)
;(defparameter nogroundlocs 0)
;(defparameter notrucks 0)
(defparameter nopackages 0)
(defparameter nogoals 0)

(defun get-parameters()
  (format t "~%Enter the number of cities :")
  (setq nocities (read))

  ;;(format t "~%Enter the number of ground locations :")
  (setq nogroundlocs nocities)

  ;;(format t "~%Enter the number of trucks :")
  (setq notrucks nocities)
  
  (format t "~%Enter the number of packages :")
  (setq nopackages (read))
  
  (format t "~%Enter the number of goals :")
  (setq nogoals (read)))

(get-parameters)

(setq initial1* (logistics-prob-generator nocities nogroundlocs notrucks nopackages))
(task1)
(let ((goal-list (generate-goals nogoals initial*)))
  (eval (cons 'create-problems (list goal-list))))
;;(run 1)