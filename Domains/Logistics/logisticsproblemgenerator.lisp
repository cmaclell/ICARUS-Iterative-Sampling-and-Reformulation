(defun logistics-prob-generator(ncities ngroundlocs ntrucks npackages &optional (nairports ncities)  (nairplanes (/ nairports 2)))
  (let ((state nil)
	(groundlocspercity (random-distribute ngroundlocs ncities))
	(airportspercity (random-distribute nairports ncities))
	(trucksperlocation (random-distribute ntrucks ngroundlocs))
	(planesperairport (random-distribute nairplanes nairports))
	(packagesperlocation (random-distribute npackages ngroundlocs)))

    ;;Generate ground location percepts
    (loop with city-index = 1
	  with locs-assigned = 0
	  for num from 1 to ngroundlocs
	  do
	  (cond ((and (< num 10)
		      (< city-index 10))
		 (push (list 'location (intern (concatenate 'string (string 'loc0) 
							    (princ-to-string num)))
			     'type 'other
			     'city (intern (concatenate 'string (string 'city0)
							(princ-to-string city-index))))
		       state))
		((and (>= num 10)
		      (< city-index 10))
		 (push (list 'location (intern (concatenate 'string (string 'loc) 
							    (princ-to-string num)))
			     'type 'other
			     'city (intern (concatenate 'string (string 'city0)
							(princ-to-string city-index))))
		       state))
		((and (< num 10)
		      (>= city-index 10))
		 (push (list 'location (intern (concatenate 'string (string 'loc0) 
							    (princ-to-string num)))
			     'type 'other
			     'city (intern (concatenate 'string (string 'city)
							(princ-to-string city-index))))
		       state))
		((and (>= num 10)
		      (>= city-index 10))
		 (push (list 'location (intern (concatenate 'string (string 'loc) 
							    (princ-to-string num)))
			     'type 'other
			     'city (intern (concatenate 'string (string 'city)
							(princ-to-string city-index))))
		       state))
		)
	  
	  (incf locs-assigned)
		 
	  (when (= locs-assigned (nth (- city-index 1) groundlocspercity))
	      (incf city-index)
	      (setq locs-assigned 0)))

    ;;Generate airport percepts
    (loop with city-index = 1
	  with locs-assigned = 0
	  for num from (+ ngroundlocs 1) to (+ ngroundlocs nairports)
	  do
	  (cond ((and (< num 10)
		      (< city-index 10))
		 (push (list 'location (intern (concatenate 'string (string 'loc0) 
							    (princ-to-string num)))
			     'type 'airport
			     'city (intern (concatenate 'string (string 'city0)
							(princ-to-string city-index))))
		       state))
		((and (>= num 10)
		      (< city-index 10))
		 (push (list 'location (intern (concatenate 'string (string 'loc) 
							    (princ-to-string num)))
			     'type 'airport
			     'city (intern (concatenate 'string (string 'city0)
							(princ-to-string city-index))))
		       state))
		((and (< num 10)
		      (>= city-index 10))
		 (push (list 'location (intern (concatenate 'string (string 'loc0) 
							    (princ-to-string num)))
			     'type 'airport
			     'city (intern (concatenate 'string (string 'city)
							(princ-to-string city-index))))
		       state))
		((and (>= num 10)
		      (>= city-index 10))
		 (push (list 'location (intern (concatenate 'string (string 'loc) 
							    (princ-to-string num)))
			     'type 'airport
			     'city (intern (concatenate 'string (string 'city)
							(princ-to-string city-index))))
		       state))
		)
	  
	  (incf locs-assigned)
		 
	  (when (= locs-assigned (nth (- city-index 1) airportspercity))
	      (incf city-index)
	      (setq locs-assigned 0)))
    
    ;;Generate Trucks percepts
    (loop with loc-index = 1
	  with trucks-assigned = 0
	  for num from 1 to ntrucks
	  do
	  (cond ((and (< num 10)
		      (< loc-index 10))
		 (push (list 'truck (intern (concatenate 'string (string 'truck0) 
							 (princ-to-string num)))
			     'at (intern (concatenate 'string (string 'loc0)
						      (princ-to-string loc-index))))
		       state))
		((and (>= num 10)
		      (< loc-index 10))
		 (push (list 'truck (intern (concatenate 'string (string 'truck) 
							 (princ-to-string num)))
			     'at (intern (concatenate 'string (string 'loc0)
						      (princ-to-string loc-index))))
		       state))
		((and (< num 10)
		      (>= loc-index 10))
		 (push (list 'truck (intern (concatenate 'string (string 'truck0) 
							 (princ-to-string num)))
			     'at (intern (concatenate 'string (string 'loc)
						      (princ-to-string loc-index))))
		       state))
		((and (>= num 10)
		      (>= loc-index 10))
		 (push (list 'truck (intern (concatenate 'string (string 'truck) 
							 (princ-to-string num)))
			     'at (intern (concatenate 'string (string 'loc)
						      (princ-to-string loc-index))))
		       state))
		)
	  
	  (incf trucks-assigned)
		 
	  (when (= trucks-assigned (nth (- loc-index 1) trucksperlocation))
	      (incf loc-index)
	      (setq trucks-assigned 0)))

    ;;Generate Airplane percepts
    (loop with airport-index = (+ ngroundlocs 1)
	  with planes-assigned = 0
	  for num from 1 to nairplanes
	  do
	  (cond ((and (< num 10)
		      (< airport-index 10))
		 (push (list 'airplane (intern (concatenate 'string (string 'airplane0) 
							 (princ-to-string num)))
			     'at (intern (concatenate 'string (string 'loc0)
						      (princ-to-string airport-index))))
		       state))
		((and (>= num 10)
		      (< airport-index 10))
		 (push (list 'airplane (intern (concatenate 'string (string 'airplane) 
							 (princ-to-string num)))
			     'at (intern (concatenate 'string (string 'loc0)
						      (princ-to-string airport-index))))
		       state))
		((and (< num 10)
		      (>= airport-index 10))
		 (push (list 'airplane (intern (concatenate 'string (string 'airplane0) 
							 (princ-to-string num)))
			     'at (intern (concatenate 'string (string 'loc)
						      (princ-to-string airport-index))))
		       state))
		((and (>= num 10)
		      (>= airport-index 10))
		 (push (list 'airplane (intern (concatenate 'string (string 'airplane) 
							 (princ-to-string num)))
			     'at (intern (concatenate 'string (string 'loc)
						      (princ-to-string airport-index))))
		       state))
		)
	  
	  (incf planes-assigned)
		 
	  (when (= planes-assigned (nth (- airport-index 
					   (+ ngroundlocs 1)) 
					planesperairport))
	      (incf airport-index)
	      (setq planes-assigned 0)))
    
    ;;Generate Package percepts
    (loop with loc-index = 1
	  with packages-assigned = 0
	  for num from 1 to npackages
	  do
	  (cond ((and (< num 10)
		      (< loc-index 10))
		 (push (list 'package (intern (concatenate 'string (string 'package0) 
							 (princ-to-string num)))
			     'at (intern (concatenate 'string (string 'loc0)
						      (princ-to-string loc-index)))
			     'inside nil)
		       state))
		((and (>= num 10)
		      (< loc-index 10))
		 (push (list 'package (intern (concatenate 'string (string 'package) 
							 (princ-to-string num)))
			     'at (intern (concatenate 'string (string 'loc0)
						      (princ-to-string loc-index)))
			     'inside nil)
		       state))
		((and (< num 10)
		      (>= loc-index 10))
		 (push (list 'package (intern (concatenate 'string (string 'package0) 
							 (princ-to-string num)))
			     'at (intern (concatenate 'string (string 'loc)
						      (princ-to-string loc-index)))
			     'inside nil)
		       state))
		((and (>= num 10)
		      (>= loc-index 10))
		 (push (list 'package (intern (concatenate 'string (string 'package) 
							 (princ-to-string num)))
			     'at (intern (concatenate 'string (string 'loc)
						      (princ-to-string loc-index)))
			     'inside nil)
		       state))
		)
	  
	  (incf packages-assigned)
		 
	  (when (= packages-assigned (nth (- loc-index 1) packagesperlocation))
	      (incf loc-index)
	      (setq packages-assigned 0)))

    state))

(defun generate-goals(ngoals state)
  (let ((goals nil)
	(package-list nil)
	(location-list nil))

    (setq package-list (loop for literal in state
			     when (equal (car literal)
					 'package)
			     collect literal))

    (setq location-list (loop for literal in state
			      when (equal (car literal)
					  'location)
			      collect literal))

    (cond ((>= (length package-list) ngoals)
	   (loop with package = nil
		 with to-location = nil
		 with result = nil
		 for i from 1 to ngoals
		 do
		 (setq result (random-choose-and-remove package-list))
		 (setq package (car result))
		 (setq package-list (second result))
		 
		 (setq result (random-choose-and-remove location-list))
		 (setq to-location (car result))
		 (setq location-list (second result))

		 (loop while (equal (second to-location)
				    (fourth package))
		       do
		       (push to-location location-list)
		       (setq result (random-choose-and-remove location-list))
		       (setq to-location (car result))
		       (setq location-list (second result)))
		 
		 (push (list 'package-at 
			     (cadr package)
			     (cadr to-location))
		       goals))
	   goals)
	  (t
	   (format t "~%ERROR! Not Enough packages to generate ~a goals.~%" ngoals)))))

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