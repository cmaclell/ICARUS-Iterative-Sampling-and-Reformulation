;;;; ELEMENT COLLECTION
;;
;; Stores literals, which may encode observations, goals, beliefs, etc.

(defstruct ec
  (pos-pred (make-hash-table :test #'eq))
  (neg-pred (make-hash-table :test #'eq))
  (idx-map (make-hash-table :test #'eq)))

(defun ec-get-element (eid ec)
  (gethash eid (ec-idx-map ec)))

(defun ec-get-elements (pred negated? ec)
  (gethash pred (if negated? (ec-neg-pred ec) (ec-pos-pred ec))))

(defun ec-get-all (ec)
  (hash-table-values (ec-idx-map ec)))

;; returns the added element
(defun ec-add-element (e contentf idf ec)
  (let ((ect (funcall contentf e))
	(eid (funcall idf e)))
    (push e (gethash (predicate-name ect) 
		     (if (negated? ect) (ec-neg-pred ec) (ec-pos-pred ec))))
    (setf (gethash eid (ec-idx-map ec)) e)))

(defun ec-remove-element (e contentf idf ec)
  (let* ((ect (funcall contentf e))
	 (eid (funcall idf e))
	 (ht (if (negated? ect) (ec-neg-pred ec) (ec-pos-pred ec))))
    (setf (gethash (predicate-name ect) ht)
	  (delete-if #'(lambda (x) (eq eid (funcall idf x)))
		     (gethash (predicate-name ect) ht)))
    (remhash eid (ec-idx-map ec))))
