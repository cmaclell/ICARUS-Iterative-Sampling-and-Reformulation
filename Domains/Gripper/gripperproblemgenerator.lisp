(defun gripper-prob-generator(nrooms nballs ngrippers &optional (nrobots 1))
  (let ((state nil)
	(ballsperroom ngrippers)
	(grippersperrobot (random-distribute ngrippers nrobots)))

    ;;(cond ((= (ceiling (/ nballs ngrippers))
    ;;(- nrooms 1))
	   
    ;;Generate room percepts
    (loop for num from 1 to nrooms
	  do
	  (cond ((< num 10)
		 (push (list 'room (intern (concatenate 'string (string 'room0) 
							(princ-to-string num))))
		       state))
		((>= num 10)
		 (push (list 'room (intern (concatenate 'string (string 'room) 
							(princ-to-string num))))
		       state))))
		 
    ;;Generate robot percepts
    (loop for num from 1 to nrobots
	  do
	  (cond ((< num 10)
		 (push (list 'robot (intern (concatenate 'string (string 'robot0) 
							 (princ-to-string num)))
			     'at 'room01)
		       state))
		((>= num 10)
		 (push (list 'robot (intern (concatenate 'string (string 'robot) 
							 (princ-to-string num)))
			     'at 'room01)
		       state))))
		     
    ;;Generate ball percepts
    (loop for num from 1 to nballs
	  do
	  (cond ((< num 10)
		 (push (list 'ball (intern (concatenate 'string (string 'ball0) 
							(princ-to-string num)))
			     'in 'room01)
		       state))
		((>= num 10)
		 (push (list 'ball (intern (concatenate 'string (string 'ball) 
							(princ-to-string num)))
			     'in 'room01)
		       state))))

    ;;Generate gripper percepts
    (loop with robot-index = 1
	  with grippers-assigned = 0 
	  for num from 1 to ngrippers
	  do
	  (cond ((and (< num 10)
		      (< robot-index 10))
		 (push (list 'gripper (intern (concatenate 'string (string 'gripper0) 
							   (princ-to-string num)))
			     'attached-to (intern (concatenate 'string (string 'robot0)
							       (princ-to-string robot-index)))
			     'holding nil)
		       state))
		((and (>= num 10)
		      (< robot-index 10))
		 (push (list 'gripper (intern (concatenate 'string (string 'gripper) 
							   (princ-to-string num)))
			     'attached-to (intern (concatenate 'string (string 'robot0)
							       (princ-to-string robot-index)))
			     'holding nil)
		       state))
		((and (< num 10)
		      (>= robot-index 10))
		 (push (list 'gripper (intern (concatenate 'string (string 'gripper0) 
							   (princ-to-string num)))
			     'attached-to (intern (concatenate 'string (string 'robot)
							       (princ-to-string robot-index)))
			     'holding nil)
		       state))
		((and (>= num 10)
		      (>= robot-index 10))
		 (push (list 'gripper (intern (concatenate 'string (string 'gripper) 
							   (princ-to-string num)))
			     'attached-to (intern (concatenate 'string (string 'robot)
							       (princ-to-string robot-index)))
			     'holding nil)
		       state)))
		 
	  (incf grippers-assigned)
		 
	  (when (= grippers-assigned (nth (- robot-index 1) grippersperrobot))
	    (incf robot-index)
	    (setq grippers-assigned 0)))
    ;;(t
    ;;(print "ERROR! Invalid problem specification.")))

    state))

(defun generate-goals(ngoals state)
  (let ((goals nil)
	(nrooms nil))

    (setq nrooms (loop for literal in state
		       when (eq (car literal)
				'room)
		       count literal into room-count
		       finally
		       (return room-count)))

    (cond ((= (- nrooms 1) ngoals)
	   (loop for i from 1 to ngoals
		 do
		 (cond ((< i 10)
			(push (list 'moved-objects 'room01
				    (intern (concatenate 'string (string 'room0) 
							 (princ-to-string (+ i 1)))))
			      goals))
		       ((>= i 10)
			(push (list 'moved-objects-to-room
				    (intern (concatenate 'string (string 'room) 
							 (princ-to-string (+ i 1)))))
			      goals))))
	   goals)
	  (t
	   (format t "~%ERROR! Invalid number of goals.")))))

(defun random-distribute (total num)
  (generate-sequence (distribute total num)))

(defun distribute (total num)
  (let* ((seq nil)
	 (extra (mod total num))
	 (base (/ (- total extra) num)))
    (do ((n 0 (+ 1 n)))
	((equal n num) seq)
	(if (>= n extra) (push base seq) (push (1+ base) seq)))))

(defun generate-sequence (list)
  (shuffle (length list) list nil))

(defun shuffle (n from to)
  (setq *random-state* (make-random-state t))
  (cond ((zerop n) to)
	(t (let ((next (nth (random n) from)))
	     (shuffle (1- n) (remove next from :count 1) (cons next to))))))