(defun depots-prob-generator(ndepots ntrucks ncrates &optional (npallets ndepots)  (nhoists (* 2 ndepots)))
  (let ((state nil)
	(trucksperdepot (random-distribute ntrucks ndepots))
	(cratesperpallet (random-distribute ncrates npallets))
	(palletsperdepot (random-distribute npallets ndepots))
	(hoistsperdepot (random-distribute nhoists ndepots)))

    ;;Generate Depots percepts
    (do ((num 1 (1+ num)))
	((> num ndepots))
      (if (< num 10)
	  (push (list 'depot (intern (concatenate 'string (string 'depot0) 
							(princ-to-string num))))
		state)
	  (push (list 'depot (intern (concatenate 'string (string 'depot) 
							(princ-to-string num))))
		state)))

    ;;Generate Trucks percepts
    (loop with depot-index = 1
	  with trucks-assigned = 0
	  for num from 1 to ntrucks
	  do
	  (cond ((and (< num 10)
		      (< depot-index 10))
		 (push (list 'truck (intern (concatenate 'string (string 'truck0) 
							 (princ-to-string num)))
			     'at (intern (concatenate 'string (string 'depot0)
						      (princ-to-string depot-index))))
		       state))
		((and (>= num 10)
		      (< depot-index 10))
		 (push (list 'truck (intern (concatenate 'string (string 'truck) 
							 (princ-to-string num)))
			     'at (intern (concatenate 'string (string 'depot0)
						      (princ-to-string depot-index))))
		       state))
		((and (< num 10)
		      (>= depot-index 10))
		 (push (list 'truck (intern (concatenate 'string (string 'truck0) 
							 (princ-to-string num)))
			     'at (intern (concatenate 'string (string 'depot)
						      (princ-to-string depot-index))))
		       state))
		((and (>= num 10)
		      (>= depot-index 10))
		 (push (list 'truck (intern (concatenate 'string (string 'truck) 
							 (princ-to-string num)))
			     'at (intern (concatenate 'string (string 'depot)
						      (princ-to-string depot-index))))
		       state))
		)
	  
	  (incf trucks-assigned)
		 
	  (when (= trucks-assigned (nth (- depot-index 1) trucksperdepot))
	      (incf depot-index)
	      (setq trucks-assigned 0)))

    ;;Generate Hoists percepts
    (loop with depot-index = 1
	  with hoists-assigned = 0
	  for num from 1 to nhoists
	  do
	  (cond ((and (< num 10)
		      (< depot-index 10))
		 (push (list 'hoist (intern (concatenate 'string (string 'hoist0) 
							 (princ-to-string num)))
			     'status nil
			     'at (intern (concatenate 'string (string 'depot0)
						      (princ-to-string depot-index))))
		       state))
		((and (>= num 10)
		      (< depot-index 10))
		 (push (list 'hoist (intern (concatenate 'string (string 'hoist) 
							 (princ-to-string num)))
			     'status nil
			     'at (intern (concatenate 'string (string 'depot0)
						      (princ-to-string depot-index))))
		       state))
		((and (< num 10)
		      (>= depot-index 10))
		 (push (list 'hoist (intern (concatenate 'string (string 'hoist0) 
							 (princ-to-string num)))
			     'status nil
			     'at (intern (concatenate 'string (string 'depot)
						      (princ-to-string depot-index))))
		       state))
		((and (>= num 10)
		      (>= depot-index 10))
		 (push (list 'hoist (intern (concatenate 'string (string 'hoist) 
							 (princ-to-string num)))
			     'status nil
			     'at (intern (concatenate 'string (string 'depot)
						      (princ-to-string depot-index))))
		       state))
		)
	  
	  (incf hoists-assigned)
		 
	  (when (= hoists-assigned (nth (- depot-index 1) hoistsperdepot))
	      (incf depot-index)
	      (setq hoists-assigned 0)))

    ;;Generate Crates percepts
    (loop with depot-index = 1
	  with pallet-index = 1
	  with crates-assigned = 0
	  with pallets-stacked = 0
	  with xpos = 0
	  with ypos = 2
	  for num from 1 to ncrates
	  do
	  (cond ((and (< num 10)
		      (< depot-index 10))
		 (push (list 'crate (intern (concatenate 'string (string 'crate0) 
							 (princ-to-string num)))
			     'xpos  xpos
			     'ypos  ypos
			     'width  4
			     'height  4
			     'at (intern (concatenate 'string (string 'depot0)
						      (princ-to-string depot-index)))
			     'inside nil)
		       state))
		((and (>= num 10)
		      (< depot-index 10))
		 (push (list 'crate (intern (concatenate 'string (string 'crate) 
							 (princ-to-string num)))
			     'xpos  xpos
			     'ypos  ypos
			     'width  4
			     'height  4
			     'at (intern (concatenate 'string (string 'depot0)
						      (princ-to-string depot-index)))
			     'inside nil)
		       state))
		((and (< num 10)
		      (>= depot-index 10))
		 (push (list 'crate (intern (concatenate 'string (string 'crate0) 
							 (princ-to-string num)))
			     'xpos  xpos
			     'ypos  ypos
			     'width  4
			     'height  4
			     'at (intern (concatenate 'string (string 'depot)
						      (princ-to-string depot-index)))
			     'inside nil)
		       state))
		((and (>= num 10)
		      (>= depot-index 10))
		 (push (list 'crate (intern (concatenate 'string (string 'crate) 
							 (princ-to-string num)))
			     'xpos  xpos
			     'ypos  ypos
			     'width  4
			     'height  4
			     'at (intern (concatenate 'string (string 'depot)
						      (princ-to-string depot-index)))
			     'inside nil)
		       state))
		)
	  
	  (incf crates-assigned)
	  (setq ypos (+ ypos 4))
		 
	  (when (= crates-assigned (nth (- pallet-index 1) cratesperpallet))
	    (incf pallet-index)
	    (incf pallets-stacked)
	    (setq crates-assigned 0)
	    (setq xpos (+ xpos 6))
	    (setq ypos 2))
	  
	  (when (= pallets-stacked (nth (- depot-index 1) palletsperdepot))
	    (incf depot-index)
	    (setq pallets-stacked 0)
	    (setq xpos 0)))
    
    ;;Generate Pallets percepts
    (loop with depot-index = 1
	  with pallets-assigned = 0
	  with xpos = 0
	  for num from 1 to (* 2 npallets)
	  do
	  (cond ((and (< num 10)
		      (< depot-index 10))
		 (push (list 'pallet (intern (concatenate 'string (string 'pallet0) 
							  (princ-to-string num)))
			     'xpos  xpos
			     'ypos  0
			     'width  4
			     'height  2
			     'at (intern (concatenate 'string (string 'depot0)
							      (princ-to-string depot-index))))
		       state))
		((and (>= num 10)
		      (< depot-index 10))
		 (push (list 'pallet (intern (concatenate 'string (string 'pallet) 
							  (princ-to-string num)))
			     'xpos  xpos
			     'ypos  0
			     'width  4
			     'height  2
			     'at (intern (concatenate 'string (string 'depot0)
						      (princ-to-string depot-index))))
		       state))
		((and (< num 10)
		      (>= depot-index 10))
		 (push (list 'pallet (intern (concatenate 'string (string 'pallet0) 
							  (princ-to-string num)))
			     'xpos  xpos
			     'ypos  0
			     'width  4
			     'height  2
			     'at (intern (concatenate 'string (string 'depot)
						      (princ-to-string depot-index))))
		       state))
		((and (>= num 10)
		      (>= depot-index 10))
		 (push (list 'pallet (intern (concatenate 'string (string 'pallet) 
							  (princ-to-string num)))
			     'xpos  xpos
			     'ypos  0
			     'width  4
			     'height  2
			     'at (intern (concatenate 'string (string 'depot)
						      (princ-to-string depot-index))))
		       state))
		)
	  
	  (incf pallets-assigned)
	  (setq xpos (+ xpos 6))
	  
	  (when (= pallets-assigned (* 2 (nth (- depot-index 1) palletsperdepot)))
	    (incf depot-index)
	    (setq pallets-assigned 0)
	    (setq xpos 0)))
    state))

(defun generate-goals(ngoals state)
  (let ((goals nil)
	(crate-list nil))

   (setq crate-list (loop for literal in state
			  when (equal (car literal)
				      'crate)
			  collect literal))

   (cond ((>= (length crate-list)
	      (* ngoals 2))
	  (loop with crate1 = nil
		with crate2 = nil
		with result = nil
		for i from 1 to ngoals
		do
		(setq result (random-choose-and-remove crate-list))
		(setq crate1 (car result))
		(setq crate-list (second result))
	 
		(setq result (random-choose-and-remove crate-list))
		(setq crate2 (car result))
		(setq crate-list (second result))

		(loop while (equal (nth 11 crate1) 
				   (nth 11 crate2))
		      do
		      (push crate2 crate-list)
		      (setq result (random-choose-and-remove crate-list))
		      (setq crate2 (car result))
		      (setq crate-list (second result)))

		(push (list 'on 
			    (cadr crate1)
			    (cadr crate2))
		      goals))
	  goals)
	 (t
	  (format t "~%ERROR! Not Enough crates to generate ~a goals.~%" ngoals)))))

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

(defun random-choose-and-remove (list)
  (let ((literal (nth (random (length list)) list)))
    (setq list (remove literal list))
    (cons literal (list list))))