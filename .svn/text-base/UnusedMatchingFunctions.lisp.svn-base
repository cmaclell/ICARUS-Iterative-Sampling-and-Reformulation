
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Partial Match Unification 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; unify 2 lists of literals (presumably with heads already matching)
;;;   assume the variable sets of the 2 lists are disjoint (gensymized if necessary)
;;;   eg     ((on ?x ?y) (on a ?x) (not (ontable ?y t1)))
;;;     with ((on b ?z) (on ?w c) (not (ontable ?v ?t)))

;;; return list of pairs: ( bindings matched-literal-pairs)
(defun partial-unify-literals (literal-list-1 literal-list-2 
			       &optional
			       (bindings no-bindings)
			       pairs-matched)
  (cond ((null literal-list-1)
	 (list (list bindings pairs-matched)))
	(t
	 (let ((next-bindings (unify (first literal-list-1)
				     (first literal-list-2)
				     bindings)))
	   (append (if next-bindings
		       (partial-unify-literals (rest literal-list-1)
					       (rest literal-list-2)
					       next-bindings
					       (cons (list (first literal-list-1)
							   (first literal-list-2))
						     pairs-matched)))
		   (partial-unify-literals (rest literal-list-1)
					   (rest literal-list-2)
					   bindings
					   pairs-matched))))))

(defun partial-unify-literals-as-triples (lit-list-1 lit-list-2
					  &optional bindings)
  (partial-unify-triples
   (gensymize-and-collect-triples lit-list-1 lit-list-2)
   bindings))

(defun partial-unify-triples (list-of-triples
			      &optional
			      (bindings no-bindings)
			      triples-matched)
  (cond ((null list-of-triples)
	 (list (list bindings triples-matched)))
	(t
	 (let* ((next-triple (first list-of-triples))
		(next-bindings (unify (first next-triple)
				      (second next-triple)
				      bindings)))
	   (append (if next-bindings
		       (partial-unify-triples (rest list-of-triples)
					      next-bindings
					      (cons next-triple triples-matched)))
		   (partial-unify-triples (rest list-of-triples)
					  bindings
					  triples-matched))))))

(defun collect-literal-heads (lit-list)
  (loop with lit-heads = nil
     for lit in lit-list
     for lit-head = (literal-head lit)
     unless (member lit-head lit-heads :test #'equal)
     do
     (push lit-head lit-heads)
     finally
     (return lit-heads)))

(defun literal-head (literal)
  (if (eql 'not (first literal))
      (list 'not (first (second literal)))
      (first literal)))
       

(defun make-bucket-list (lit-heads)
  (loop for lit-head in lit-heads
       collect (list lit-head nil nil nil)))

(defun find-bucket (literal bucket-list)
  (first (member (literal-head literal) bucket-list :key #'first :test #'equal)))

(defun bucketize (lit-list bucket-list &key (field 1)) ; fields = 1, 2, or 3
  (loop for lit in lit-list
       for bucket = (find-bucket lit bucket-list)
       when bucket
       do
       (push lit (nth field bucket))))


(defun gensymize-and-collect-triples (lit-list-1 lit-list-2)
  (let* ((common-lit-heads (intersection (collect-literal-heads lit-list-1)
					 (collect-literal-heads lit-list-2)
					 :test #'equal))
	 (gensymized-lit-list-2 (gensymize-vars lit-list-2))
	 (bucket-list (make-bucket-list common-lit-heads)))
    (bucketize lit-list-1 bucket-list :field 1)
    (bucketize gensymized-lit-list-2 bucket-list :field 2)
    (bucketize lit-list-2 bucket-list :field 3)
    (loop for bucket in bucket-list
       append (make-triples-from-bucket bucket))))
    

(defun make-triples-from-bucket (bucket)
  (loop for lit1 in (nth 1 bucket)
     append
     (loop for lit2 in (nth 2 bucket)
	for lit3 in (nth 3 bucket)
	collect
	(list lit1 lit2 lit3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "Exact" Unification
;;;   (only match variables with variables and constants with constants)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; THIS APPROACH IS FLAWED:
;;;    Unification is not what we want, since it will result in matches that shouldn't be,
;;;      For example  (on ?x ?y)  will match (on ?z ?z) because it will unify ?x and ?y.
;;;         BUT these are not the same goal-literal

;; returns NIL if EXACT unification failed, otherwise returns norvig-bindings
;;   Note that "empty bindings" result is Norvig's no-bindings, ie (T . T)
;;   The bindings list returned is an extension of the input bindings
(defun exact-unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail) fail)
        ((eql x y) bindings)
        ((and (variable-p x)
	      (variable-p y))
	 (unify-variable x y bindings))
        ((and (consp x) (consp y))
         (exact-unify (rest x) (rest y) 
		      (exact-unify (first x) (first y) bindings)))
        (t fail)))

(defun exists-exact-unify-with-permuted-literals (literals-1 literals-2)
  (cond ((null literals-1)
	 (null literals-2))
	(t
	 (loop with lit-1 = (first literals-1)
	    for lit-2 in literals-2
	    thereis
	    (and (exact-unify lit-1 lit-2)
		 (exists-exact-unify-with-permuted-literals (cdr literals-1)
							    (remove lit-2
								    literals-2
								    :test #'equal)))))))

(defun careful-exists-exact-unify-with-permuted-literals (literals-1 literals-2)
  (and (= (length literals-1)
	  (length literals-2))
       (exists-exact-unify-with-permuted-literals literals-1
						  (gensymize-vars literals-2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple Exact Matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This matches the forms, ensuring that vars of form-1 bind consistently to values in from-2
;;    Vars in Form-2 are treated same as constants, 
;;    but the "exactness" condition requires that variables only bind with variables,
;;       and constants match constants

(defun exists-simple-exact-match-with-permuted-literals (literals-1 literals-2 &optional (bindings no-bindings))
  (cond ((eq bindings fail) fail)
	((null literals-1)
	 (if (null literals-2)
	     bindings
	     fail))
	(t
	 (loop with lit-1 = (first literals-1)
	    for lit-2 in literals-2
	    thereis
	      (let ((bindings-1-2 (simple-exact-binding-match lit-1 lit-2 bindings)))
		(and bindings-1-2
		     (exists-simple-exact-match-with-permuted-literals (cdr literals-1)
								       (remove lit-2
									       literals-2
									       :test #'equal)
								       bindings-1-2)))))))

(defun careful-exists-simple-exact-match-with-permuted-literals (literals-1 literals-2)
  (and (= (length literals-1)
	  (length literals-2))
       (exists-simple-exact-match-with-permuted-literals literals-1 literals-2)))