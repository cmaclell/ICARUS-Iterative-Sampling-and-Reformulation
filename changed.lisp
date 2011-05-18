;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file has functions from the old code that have been
;;; modified to some degree to make them compatible.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quote-constants (action)
;  (mapcar #'quote-constants-aux action)
  (cond ((null action) nil)		; added so that when list is NIL, this returns NIL instead of (NIL)
	(t (quote-constants-aux action))))

; QUOTE-CONSTANTS-AUX takes a list and returns the list with all the constants
; quoted for later evaluation. It assumes that the first element of the list
; is a function name, and therefore does not quote it. This may not be necessary
; because the function name can be evaluated too, but it is there anyway.
(defun quote-constants-aux (list)
  (cond ((equal (car list) 'quote)
	 list)
	(t
	 (append (list (car list))
		 (mapcar #'(lambda (one)
			     (cond ((listp one)
				    (quote-constants-aux one))
				   ((or (variablep one)
					(numberp one)
					(boundp one)) ; this seems odd - what if symbol is accidentally bound for some other purpose?
				    one)
				   (t
				    (list 'quote one))))
			 (cdr list))))))
			 
(defparameter pprint-state* nil)
(defparameter pat-trace* nil)
			 
(defun run-aux (n
		&key (timelimit 0) (mode nil))
  (check-essential-functions)

  ;; Compile a list of unchainable conditions for each skill
  (if (< cycle* 2)
      (extract-unchainable-conditions cltm* sltm*))

  (do ()
      ((or (= cycle* n)
	   (and (> timelimit 0)
		(>= time* timelimit))
	   halt*)
       (cond ((and (> timelimit 0)
		   (>= time* timelimit))
	      (print-timeout))
	     ((equal halt* 'noskill)
	      (print-no-skill))
	     ((equal halt* 'success)
	      (print-achieved))))
      (setq current-resources* nil)
      (setq learned-paths-used* nil)
      (sleep 0.1)
      (let ((starttime (get-internal-real-time))
	    percepts
	    agent-level-results)
	(setq pstm* (preattend))
	(setq percepts (copy-list pstm*))
	(cond ((equal inference* 'STANDARD) 
	       (setq cstm* (infer cltm* pstm* cstm*)))
	      ((equal inference* 'FAST)
	       (run-inference))
	      (t
	       (error "The parameter INFERENCE* is not set up properly!")))
	(setq astm* nil)
	;; update the belief heads (literals)
	(setf cstmh*
	      (mapcar #'cinstance-head cstm*))
      ;Store the initial state
	(cond ((= cycle* 1)
	       (push pstm* pstatetrace*)
	       (push cstm* cstatetrace*)))
      ;If some action was taken in the previous cycle,
      ;store the modified state.
	;(cond ((not (null last-execution*))
	       ;(push pstm* pstatetrace*)
	       ;(push cstm* cstatetrace*)))
	;(setq last-execution* nil)
	(if pat-trace*
	    (print-pat-memory-traces)
	    (print-memory-traces))
	;;; REWRITE CODE FOLLOWS:
	(select-active-problem)
	(solve active-problem* executing-intention*)
	)
    (update-world)
    (when pprint-state*
      (pprint-state))
    (setq cycle* (1+ cycle*))))

(defun print-icarus-settings ()
  (unless pat-trace*
    (terpri)(princ "========= Current Icarus Setting =========")
    (terpri)(princ " Inference Engine: ")
    (print-setting inference* '(standard "Standard")
		   '(fast "Fast Matcher"))
    (terpri)(princ " Problem Solver: ")
    (print-setting problem-solver-enabled* '(t "ON") '(nil "OFF"))
    (when problem-solver-enabled*
      (terpri)(princ " Skill Selection Heuristic: ")
      (print-setting skill-selection-heuristic* 
		     '(:MAX-EFFECTS-MATCHED "Maximize Matched Effects")
		     '(:MIN-UNSATISFIED-CONDITIONS "Minimize Unsatisfied Conditions")))
    (terpri)(princ "==========================================")))

(defun print-pat-memory-traces ()
  (let ((*print-right-margin* 78))
    (cond ((not (null ctrace*))
	   (terpri)(princ "----------")
	   (terpri)(princ "Cycle ")(princ cycle*)))
    (cond ((and (not (null gtrace*))
		problem-solver-enabled*)
	   (print-problems)))
    (cond ((or pat-trace*
	       (equal ptrace* 2))
	   (print-percepts T)) ; T = "raw" -- always use when pat-trace* is T
	  ((equal ptrace* 1)
	   (print-percepts)))
    (cond ((not (null btrace*))
	   (terpri) (princ "Beliefs:")
	   (pprint cstmh*)))
    nil))