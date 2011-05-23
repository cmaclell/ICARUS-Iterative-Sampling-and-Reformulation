
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PPRINT STATE - Blocks World
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf pprint-state* t)

;;; Sample Target printouts:

;; STATE:
;;  Holding: Empty
;;
;;   . [C] .
;;   . [B] .
;;   . [A] . [D]
;;  ------------

;; STATE:
;;  Holding: [C]
;;
;;   . [B] .
;;   . [A] . [D]
;;  ------------


(defun pprint-state (&optional (state state*))
  (terpri)
  (princ "STATE:")
  ;; block triples of form: (name-string row col)
  (terpri)
  (princ (hand-string state))
  (let* ((block-triples (get-block-triples (get-raw-block-triples state)))
	 (state-array (make-block-array block-triples)))
    (pprint-2d-array state-array)))

;; Need to omit block if being held by hand
(defun get-raw-block-triples (state)
  (loop for form in state
       for hand-status = (get-hand-status state)
       when (and (eql (first form) 'block)
		 (not (eql (second form) hand-status)))
       collect
       (let* ((block-name (block-name-string (second form)))
	      (block-x (second (member 'xpos form)))
	      (block-y (second (member 'ypos form))))
	 (list block-name block-x block-y))))

(defun block-name-string (block)
  (format nil "[~a]" block))

(defun hand-string (state)
  (let* ((hand-form (assoc 'hand state))
	 (hand-status (get-hand-status state)))
    (case hand-status 
      (empty (format nil "Holding: Empty"))
      (t (format nil "Holding: [~a]" hand-status)))))

(defun get-hand-status (state)
  (second (member 'status (assoc 'hand state))))

;; convert coordinates to row col for array
;;    block-y determines row
;;    block-x determines col
;;    assumes all x and y are even (2 times basic coordinates)
;;  returns triples in form: (name-string row col)
(defun get-block-triples (raw-block-triples)
  (let ((max-x (loop for triple in raw-block-triples
		  maximize (second triple)))
	(max-y (loop for triple in raw-block-triples
		  maximize (third triple))))
    (loop for (name-string x y) in raw-block-triples
	 collect
	 (list name-string
	       (floor (- max-y y) 2)	; row
	       (floor x 2)))))		; col

(defun make-block-array (block-triples)
  (let* ((max-row (loop for triple in block-triples
		     maximize (second triple)))
	 (max-col (loop for triple in block-triples
		     maximize (third triple)))
	 (block-array (make-array (list (+ 2 max-row) ; 0 to max-row plus line for table
					(+ 1 max-col)) ; 0 to max-col
				  :initial-element " . ")))
    (loop for col from 0 to max-col
       do
       (setf (aref block-array (1+ max-row) col)
	     "---"))
    (loop for (name-string row col) in block-triples
       do
       (setf (aref block-array row col)
	     name-string))
    block-array))

(defun pprint-2d-array (2d-array)
  (loop for row from 0 below (array-dimension 2d-array 0)
       do
       (terpri)
       (loop for col from 0 below (array-dimension 2d-array 1)
	    do
	    (format t "~a " (aref 2d-array row col)))))
