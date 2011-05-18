#|
(setq initial1*
	       (list '(depot depot01)
		     '(depot depot02)
		     '(truck truck01 at depot01)
		     '(hoist hoist01 status nil at depot01)
		     '(hoist hoist02 status nil at depot02)
		     '(pallet pallet01 xpos 0 ypos 0 width 4 height 2 at depot01)
		     '(pallet pallet02 xpos 0 ypos 0 width 4 height 2 at depot02)
		     '(crate crate01 xpos 0 ypos 2 width 4 height 4 at depot01 inside nil)))
|#

(defparameter nodepots 0)
(defparameter notrucks 0)
(defparameter nocrates 0)
(defparameter nogoals 0)

(defun get-parameters()
  (format t "~%Enter the number of depots :")
  (setq nodepots (read))

  (format t "~%Enter the number of trucks :")
  (setq notrucks (read))
  
  (format t "~%Enter the number of crates :")
  (setq nocrates (read))
  
  (format t "~%Enter the number of goals :")
  (setq nogoals (read)))

(get-parameters)

(setq initial1* (depots-prob-generator nodepots notrucks nocrates))
(task1)
(let ((goal-list (generate-goals nogoals initial*)))
  (eval (cons 'create-problems (list goal-list))))
;;(run 1)