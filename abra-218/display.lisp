;;;;; 
;;;;; Display code
;;;;; 

(defun output-dot (wm &key (file-name "foo.dot") (show-worlds? t))
  (with-open-file (str (make-pathname :name file-name)
			 :direction :output :if-exists :supersede)
    (format str (beliefs-to-dot wm show-worlds?))))


(defun beliefs-to-dot (wm sw?)
  (let ((s (format nil 
		   "digraph G {~%size=\"10,10\";concentrate=true;rankdir=LR;edge[dir=none];ranksep=\"0.9\";~%"))
	(lnks '())
	(ht (make-hash-table)))
    (setf s (concatenate 'string s (format nil (if sw? 
						 "clusterrank=local;~%" 
						 "clusterrank=global;~%"))))
    ;; build the string that describes each world subgraph
    (loop for w being the hash-values of (wm-worlds wm)
       do (multiple-value-bind (x y) (world-to-dot w)
	    (setf lnks (append y lnks)) ;; fresh with side effects!!
	    (setf s (concatenate 'string s x))))

    ;; build cross-world links
    (dolist (k lnks)
      (unless (or (member (first k) (gethash (second k) ht))
		  (member (second k) (gethash (first k) ht)))
	(push (second k) (gethash (first k) ht))))
    
    ;; build the string for the cross-world links
    (loop for k being the hash-keys of ht
	 do 
	 (dolist (x (gethash k ht))
	   (setf s (concatenate 'string s (format nil "~A -> ~A;~%" k x)))))
    (concatenate 'string s (format nil "}~%"))))

(defun world-to-dot (w)
  ;; map justifications in wrld to beliefs in wrld
  ;; ignore beliefs in other worlds
  (let ((local (make-hash-table)) distant s)
    (dolist (j (get-justifications w))
      (let ((bfs (get-supported-beliefs j w)))
	(dolist (b1 bfs)
	  (dolist (b2 bfs)
	    (if (and (eql (car b1) w) (eql (car b2) w))
		(unless (or (eql (second b1) (second b2))
			    (member (second b2) (gethash (second b1) local))
			    (member (second b1) (gethash (second b2) local)))
		  (push (second b2) (gethash (second b1) local)))
		(unless (eql (second b1) (second b2))
		  (push (list (belief-id (second b1)) (belief-id (second b2))) distant)))))))
    (setf s (format nil "subgraph cluster~A {~%label = \"~A\"; color=black;" 
		    (remove #\. (symbol-name (world-id w))) (world-id w)))
    (setf s (concatenate 'string s 
			(format nil "~%edge[dir=none];~%")))
    (dolist (k (hash-table-keys local))
      (dolist (x (gethash k local))
	(setf s (concatenate 'string s (format nil "~A -> ~A;~%" (belief-id k) (belief-id x))))))

    (dolist (wb (get-local-beliefs w))
      (setf s (concatenate 'string s (format nil "~A [label=\"~A\"];~%" 
					     (belief-id (second wb)) (belief-content (second wb))))))
    (values (concatenate 'string s (format nil "}~%")) distant)))
    

(defun output-rls (rls &key (file-name "foor.dot"))
  (with-open-file (str (make-pathname :name file-name)
			 :direction :output :if-exists :supersede)
    (format str (rules-to-dot rls))))

;; XXX: ignores modals and negation
(defun rules-to-dot (rls)
  (let ((s (format nil
		   "digraph G {~%size=\"10,10\";concentrate=true;rankdir=LR;edge[dir=none];ranksep=\"0.9\";~%"))
	(ht (make-hash-table)))
    ;; build a triangular adjacency matrix based on the predicates that appear in each rule.
    (dolist (r rls)
      (let ((pds (list-predicates r)))
	(dolist (p1 pds)
	  (dolist (p2 pds)
	    (unless (or (eql (first p1) (first p2))
			(member (first p2) (gethash (first p1) ht))
			(member (first p1) (gethash (first p2) ht)))
	      (push (first p2) (gethash (first p1) ht)))))))

    ;; print the connections.
    (dolist (k (hash-table-keys ht))
      (dolist (x (gethash k ht))
	(setf s (concatenate 'string s (format nil "\"~A\" -> \"~A\";~%" k x)))))

    ;; close the string.
    (concatenate 'string s (format nil "}~%"))))
