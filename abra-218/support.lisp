;;;; BELIEF SUPPORT

;;; the implementation plan is to treat this data structure 
;;; as entirely encapsulated by a world. only the world should
;;; be able to see the structure and its functions. And even
;;; worlds should not peer inside the implementation of bs.

;;; track support for each belief
;;; for now, just keep a list for each belief that contains
;;; 'fact or a justification that the belief matches. 
;;;
;;; eventually, we want the rules to be instantiations and we
;;; may want to make the structure a bit more exciting
(defstruct bs
  ;; instantiated rule index to instantiated rule
  (idx-map (make-hash-table :test #'eq))
  ;; positive literals => rule
  (pos-pred (make-hash-table :test #'eq))
  ;; negative literals => rule
  (neg-pred (make-hash-table :test #'eq))
  ;; belief ID to list of justifications
  (supported-by (make-hash-table :test #'eq))
  ;; justification ID to list of beliefs
  (supports (make-hash-table :test #'eq)))

;; bs-get-justification
;;
;; given the ID for a justification and a support structure,
;; returns the associated justification.
(defun bs-get-justification (id bs)
  (gethash id (bs-idx-map bs)))

;; bs-get-all-justifications
;; 
;; given a support structure, returns all its justifications
(defun bs-get-all-justifications (bs)
  (hash-table-values (bs-idx-map bs)))

;; bs-get-justifications
;; 
;; returns all justifications in the support structure that 
;; match the literal and its negation status.
(defun bs-get-justifications (lit bs)
  (if (negated? lit) 
      (gethash (predicate-name lit) (bs-neg-pred bs))
      (gethash (predicate-name lit) (bs-pos-pred bs))))

;; bs-find-justification-id
;; 
;; looks for a justification whose content is identical to j's and
;; returns its ID.
(defun bs-find-justification-id (j bs)
  (maphash #'(lambda (idx jst)
	       (when (justification-equal? jst j)
		 (return-from bs-find-justification-id idx))) 
	   (bs-idx-map bs)))

;; bs-find-subsumed-justification-id
;;
;; returns the ID of a justification in the support structure that
;; is more specific than the given justification.
(defun bs-find-subsumed-justification-id (r bs)
  (maphash #'(lambda (idx rule)
	       (when (justification-subsumes? r rule)
		 (return-from bs-find-subsumed-justification-id idx))) 
	   (bs-idx-map bs)))

#|
;; testing: find most specific rule that's r subsumes
(defun bs-find-subsumed-justification-id (r bs)
  (first 
   (sort (loop for j being the hash-values of (bs-idx-map bs)
	    when (justification-subsumes? r j)
	    collect (list (bs-count-skolems j) (justification-id j)))
	 #'<
	 :key #'first)))
	

;; testing
(defun bs-count-skolems (j)
  (let ((ht (make-hash-table)))
    (dolist (lit (justification-literals j))
      (dolist (a (literal-args lit))
	(when (skolem? a) (setf (gethash a ht) t))))
    (hash-table-count ht)))
|#

;; bs-store-justification
;;
;; stores a justification in the support structure.
(defun bs-store-justification (j bs)
  (setf (gethash (justification-id j) (bs-idx-map bs)) j)
  (dolist (lit (justification-literals j))
    (if (negated? lit) 
	(push j (gethash (predicate-name lit) (bs-neg-pred bs)))
        (push j (gethash (predicate-name lit) (bs-pos-pred bs))))))

;; bs-remove-justification
;;
;; returns the beliefs that are no longer supported by justifications
;; within this world. note that the beliefs may be retained as facts
;; or may have support in other worlds if they are imported.
(defun bs-remove-justification (jid local-p bs)
  ;; disassociate the justification from the predicates.
  (dolist (lit (justification-literals (gethash jid (bs-idx-map bs))))
    (let ((pht (if (negated? lit) (bs-neg-pred bs) (bs-pos-pred bs))))
	(setf (gethash (predicate-name lit) pht)
	      (remove (bs-get-justification jid bs) 
		      (gethash (predicate-name lit) pht)))))

  ;; disassociate justification from beliefs.
  ;; collect beliefs that lack support.
  ;; remove the justification from other hash tables.
  (let (ebs)
    (dolist (wb (gethash jid (bs-supports bs)))
      (if (not (funcall local-p wb)) ;; belief exists in another world, so break weak links
	  (progn (setf (belief-weak-links (second wb))
		       (remove-if #'(lambda (wj) (eql (justification-id (second wj)) jid))
				  (belief-weak-links (second wb))))
		 ;; remove distant beliefs that are only supported by this justification
		 (if (and (null (belief-weak-links (second wb)))
			  (null (gethash (belief-id (second wb)) (bs-supported-by bs)))
			  (not (belief-fact (second wb))))
		     (push (second wb) ebs)))
	  ;; belief is local, so remove it if it has lost all its justifications
	  (unless (setf (gethash (belief-id (second wb)) (bs-supported-by bs))
			(remove-if #'(lambda (j) (eql (justification-id j) jid))
				   (gethash (belief-id (second wb)) (bs-supported-by bs))))
	    (push (second wb) ebs))))
    (remhash jid (bs-supports bs))
    (remhash jid (bs-idx-map bs))
   ;; (print ebs)
    ebs))


(defun bs-supported-beliefs (j bs)
  (gethash (justification-id j) (bs-supports bs)))

(defun bs-supporting-justifications (b bs)
  (gethash (belief-id b) (bs-supported-by bs)))
