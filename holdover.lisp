;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file has functions (held over) from code that are still
;;; being used but no modifications have been made to them.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;******************************************************************************
; Global Variable Definitions, their Initializations, and Support Functions from compiler
;******************************************************************************
(defvar cltm* nil)

; CONCEPTS* stores heads of defined concepts
(defvar concepts* nil)
(defvar primitive-concepts*)
(defvar gstm* nil)
(defvar sltm* nil)
(defvar id-count*)


(setq id-count* 0)
(setq cltm* nil)
(setq sltm* nil)

(setq primitive-concepts* nil)

(defun clear-concepts ()
  (setq cltm* nil))

(defun remove-concepts ()
  (setq cltm* nil))

(defun rc ()
  (remove-concepts))

(defun clear-skills ()
  (setq sltm* nil))

(defun remove-skills ()
  (setq sltm* nil))

(defun rs ()
  (remove-skills))

(defun clear-goals ()
  (setq gstm* nil))

(defun remove-goals ()
  (setq gstm* nil))

(defun rg ()
  (remove-goals))

(defun clear ()
  (clear-concepts)
  (if (equal inference* 'fast) (clear-fast-matcher))
  (clear-skills)
  (clear-goals)
  (setq id-count* 0)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;pivot marks the variable(s) used for partial matching.
(defstruct concept head id percepts relations positives negatives tests 
	   constraints pivot threshold
           value duration expected pschildren ngchildren siblings
           instances rmdup attributes calculates counters)

(defstruct cinstance head id bindings subgoals 
           (degmatch 1.0) (timestamp cycle*) pos-dependencies
           neg-dependencies percepts total-percepts probability
	   (time-stamps (cons nil 'NOW)))

; SAME-CINSTANCE checks two instantiated literals to determine if they are
; the same. Therefore, hidden bindings can be different.
(defun same-cinstance (cinst1 cinst2)
  (cond ((and (equal (cinstance-head cinst1) (cinstance-head cinst2)))
	 t)))

(defun cinstance-name (cinstance)
  (let ((head (cinstance-head cinstance)))
    (if (time-stamped head)
	(caar head)
	(car head))))

(defun cinstance-args (cinstance)
  (let ((head (cinstance-head cinstance)))
    (if (time-stamped head)
	(cdar head)
	(cdr head))))

(defun concept-name (concept)
  (car (concept-head concept)))

(defun start-time (belief)
  (car (cinstance-time-stamps belief)))

(defun end-time (belief)
  (cdr (cinstance-time-stamps belief)))

; TIME-STAMPED inputs a positive concept head that may be time-stamped, and
; returns the head if it is indeed time-stamped, and nil if not.
(defun time-stamped (head)
  (cond ((and (listp (car head))
	      (or (= (length head) 2)
		  (= (length head) 3)))
	 (if (and (fboundp 'start-time)
		  (fboundp 'end-time))
	     head
	     (error "Time-stamped expression exists in the knowledgebase, but temporal.lisp is not loaded.")))))

;******************************************************************************
; Main Macros (Compiler)
;******************************************************************************
(defmacro create-concepts (&rest concepts)
  `(let ((concepts (quote ,concepts))
	 (is-null-result nil))
     (do* ((next-concept (car concepts) (car concepts)))
	  ;exit condition
	  ((or (null concepts) is-null-result)
	   (if is-null-result
	       nil			; exit w/o reversing cltm*
	     (progn
	       (setq cltm* (reverse cltm*))
	       nil)))
	  ;do body
	  (let ((result (create-concept next-concept)))
	    (if (null result)
		(setf is-null-result t)
	      (progn
		(setf is-null-result nil)
		(push result cltm*))))
	  (pop concepts))))

(defun create-concept (concept)
  (let ((defined (car (member (caar concept) concepts*
			      :key #'car :test 'equal))))
    (cond ((and defined
		(not (equal (car concept) defined)))
	   (terpri)
	   (princ "The concept head does not match with already defined ones.")
	   (terpri)(princ "Please use the same set of variables for the head.")
	   (return-from create-concept nil))
	  ((null defined)
	   (push (car concept) concepts*))))
  (let ((head (car concept))
	(percepts nil)
	(positives nil)
	(negatives nil)
	(relations nil)
	(tests nil)
	(pivot nil)
	(threshold nil)
	(value 0.0)
	(pschildren nil)
	(ngchildren nil)
	(siblings   nil)
	(instances  nil)
	(rmdup      nil)
	(attributes nil)
	(expected 0.0)
        (duration (list 1.0 1.0)))
    (setq concept (cdr concept))
    (do ((next-field (car concept) (car concept))
	 (next-spec (cadr concept) (cadr concept)))
	((null concept)
         (cond ((and (null positives) (null negatives)
                     (not (member (car head) primitive-concepts*)))
                (push (car head) primitive-concepts*)))
	 (make-concept :id (gensym "CNPT")
		       :attributes attributes
		       :head head :percepts percepts :relations relations
		       :positives positives :negatives negatives
		       :tests tests :pivot pivot :threshold threshold :value value
		       :expected expected :duration duration))
	(cond ((eq next-field ':percepts)(setq percepts next-spec))
	      ((eq next-field ':relations)
	       (setq relations next-spec)
	       (setq positives (get-pos-conds next-spec))
	       (setq negatives (get-neg-conds next-spec)))
#|;TLP_EXP trial
	       (setq positives (mapcar #'(lambda (rel)
					   (make-cinstance :head rel))
				       (get-pos-conds next-spec)))
	       (setq negatives (mapcar #'(lambda (rel)
					   (make-cinstance :head rel
							   :negative T))
				       (get-neg-conds next-spec)))
	       (setq relations (append positives negatives)))
|#
	      ((eq next-field ':tests)(setq tests next-spec))
	      ((eq next-field ':pivot)(setq pivot next-spec))
	      ((eq next-field ':threshold)(setq threshold next-spec))
	      ((eq next-field ':value)(setq value next-spec))
	      ((eq next-field ':expected)(setq expected next-spec))
	      ((eq next-field ':duration)(setq duration next-spec))
	      ((eq next-field ':attributes)(setq attributes next-spec))
	      (t (print-error next-field "field" "concept")))
	(setq concept (cddr concept)))))

(defun get-pos-conds (conditions)
  (cond ((null conditions) nil)
	((not (eq (caar conditions) 'not))
	 (cons (car conditions) (get-pos-conds (cdr conditions))))
	(t (get-pos-conds (cdr conditions)))))

(defun get-neg-conds (conditions)
  (cond ((null conditions) nil)
	((eq (caar conditions) 'not)
	 (cons (cadar conditions) (get-neg-conds (cdr conditions))))
	(t (get-neg-conds (cdr conditions)))))

;; This function is also called by create-skill-clause in solver-rewrite.lisp
(defun print-error (next x y)
  (terpri)(princ "Syntax error: ")
  (princ next)(princ " is not a valid ")
  (princ x)(princ " for an Icarus ")
  (princ y)(princ ".") nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global variables, Initializations and Functions from 
;;; interpreter.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cycle*)
(defvar starttime*)
(defvar pstm*)
(defvar cstm*)
(defvar astm*)

(defvar halt*)
(defvar failed*)

(defvar gtrace*)
(defvar ptrace*)
(defvar btrace*)
(defvar ctrace*)
(defvar atrace*)
(defvar etrace*)

;Switch for debug mode
(defvar debug*)

(defvar inference*)

;The threshold for degree of match below which the system would not
; print out the belief.
(defparameter belief-printout-threshold* 0.5)

(setq cycle* 0)
(setq gtrace* t) ; goal print
(setq ptrace* 1) ; 1 for organized print, and 2 for raw print of percepts
(setq btrace* t) ; belief print
(setq ctrace* t) ; cycle print
(setq atrace* t) ; action print
(setq etrace* t) ; 

(setq debug* nil)

;*** Switch for Inference Mechanism ***
; STANDARD : standard matcher (INFER)
; FAST     : fast matcher (RUN-INFERENCE)
(setq inference* 'fast)

; RUN executes the Icarus interpreter for N cycles or, if called without 
; an argument, indefinitely. 
(defun run (&optional (n -1))
  (setq halt* nil)
  (run-init)
  (run-aux (1+ n)))

; RUN executes the Icarus interpreter for N cycles or until all top-level 
; goals are satisfied. If called without an argument, the system runs
; indefinitely until all goals are satisfied. 

(defun grun (&optional (n -1))
  (setq halt* nil)
  (run-init)
  (run-aux (1+ n)))

; CONT does the same as RUN, except it does not reinitialize variables
; or the environment. 

(defun cont (&optional (n -1))
  (setq halt* nil)
  (run-aux (+ cycle* n)))

; GCONT does the same as GRUN, except it does not reinitialize variables
; or the environment. 

(defun gcont (&optional (n -1))
  (setq halt* nil)
  (run-aux (+ cycle* n)))

(defun timed-grun (timelimit)
  (setq halt* nil)
  (run-init)
  (run-aux 0 :timelimit timelimit))

; RUN-INIT initializes global variables and the state of the simulated
; environment in preparation for an Icarus run. 
(defun run-init ()
  (setq cycle* 1)
  (setq starttime* (get-universal-time))
  (setq new-skills* nil)
  (setq new-concepts* nil)
  (setq pstatetrace* nil)
  (setq cstatetrace* nil)
  (setq skilltrace* nil)
  (setq failcount* 0)

  (cond ((equal inference* 'fast)
	 (clear-fast-matcher)))

  (if (fboundp 'initialize-world)
      (initialize-world)
    (error "INITIALIZE-WORLD function is not defined.~%Make sure the simulator has the definition."))
  (print-icarus-settings))

(defun check-essential-functions ()
  (cond ((and (equal inference* 'STANDARD)
	      (not (fboundp 'infer)))
	 (error "INFER function is not defined.~%Make sure you are loading MATCHER.LISP."))
	((and (equal inference* 'FAST)
	      (not (fboundp 'run-inference)))
	 (error "RUN-INFERENCE function is not defined.~%Make sure you are loading the fast matcher.")))

  (if (not (fboundp 'update-world))
      (error "UPDATE-WORLD function is not defined.~%Make sure the simulator has the definition.")))

(defun print-setting (param &rest options)
  (cond ((null options)
	 (if (null param) (princ "OFF") (princ "ON")))
	(t
	 (let (setting)
	   (mapcar #'(lambda (option)
		       (if (equal (car option) param)
			   (setq setting (cadr option))))
		   options)
	   (if (null setting) (princ "UNKNOWN") (princ setting))))))

(defun print-memory-traces ()
  (cond ((not (null ctrace*))
	 (terpri)(princ "----------")
	 (terpri)(princ "Cycle ")(princ cycle*)))
  (cond ((not (null gtrace*))
	 (print-problems)))
  (cond ((equal ptrace* 2)
	 (print-percepts T))
	((equal ptrace* 1)
	 (print-percepts)))
  (cond ((not (null btrace*))
	 (pretty-print-beliefs)))
  nil)

(defun variablep (sym-name)
 (cond ((atom sym-name)
	(cond ((numberp sym-name)
	       nil)
	      (t
	       (eq (elt (symbol-name sym-name) 0) #\?))))))

(defun rcopy (x)
  (cond ((atom x) x)
        (t (mapcar #'rcopy x))))

(defvar max-length-print-full-percept* 40)   ;; Glenn - Was 2, made it to 40.

(defun print-percepts (&optional (raw nil) &aux (localpercepts))
  ;; do setf so will work in SBCL
  (setf localpercepts (sort (copy-list pstm*) #'string< :key #'car))
  (terpri)(princ "Percepts:")
  (cond (raw (pprint localpercepts))
	(t
	 (let ((last nil))
	   (mapcar #'(lambda (percept)
		       (cond ((or (null last)
				  (not (equal (car percept) last)))
			      (setq last (car percept))
			      (terpri)(princ "   ")(princ last)(princ ":")
			      (terpri)(princ "      ")
			      (cond ((> (length percept)
					max-length-print-full-percept*)
				     (princ "(")
				     (mapc
				      #'(lambda (element)
					  (princ element)
					  (princ " "))
				      (subseq percept
					      0
					      max-length-print-full-percept*))
				     (princ "...)"))
				    (t
				     (princ percept))))
			     (t
			      (terpri)(princ "      ")
			      (cond ((> (length percept)
					max-length-print-full-percept*)
				     (princ "(")
				     (mapc
				      #'(lambda (element)
					  (princ element)
					  (princ " "))
				      (subseq percept
					      0
					      max-length-print-full-percept*))
				     (princ "...)"))
				    (t
				     (princ percept))))))
		   localpercepts)))))

(defun pp () (print-percepts))

(defun pretty-print-beliefs (&aux (localcstm))
  (setf localcstm (sort (copy-list cstm*) #'string< :key #'cinstance-name))
  (terpri)(princ "Beliefs:")
  (if (null localcstm) (princ " NONE")
      (let ((last nil))
	(mapcar #'(lambda (belief)
		    (let ((name (cinstance-name belief))
			  (head (cinstance-head belief))
			  (degmatch (cinstance-degmatch belief)))
		      (unless (and (numberp degmatch)
				   (< degmatch belief-printout-threshold*))
			(cond ((or (null last)
				   (not (equal name last)))
			       (setq last name)
			       (terpri)(princ "   ")(princ last)(princ ":")
			       (terpri)(princ "     ")(princ head)
			       (cond ((numberp degmatch)
				      (cond ((< degmatch 1.0)
					     (princ " ")(princ degmatch))
					    ((= degmatch 1.0))
					    (t
					     (princ " * INVALID DEGMATCH *"))))
				     (t
				      (princ " * NO DEGMATCH *"))))
			      (t
			       (terpri)(princ "     ")(princ head)
			       (cond ((numberp degmatch)
				      (cond ((< degmatch 1.0)
					     (princ " ")(princ degmatch))
					    ((= degmatch 1.0))
					    (t
					     (princ " * INVALID DEGMATCH *"))))
				     (t
				      (princ " * NO DEGMATCH *"))))))))
		localcstm)))
  T)

(defun pb () (pretty-print-beliefs))

(defun print-concepts ()
  (terpri)(princ "Concepts:")(terpri)
  (mapc #'(lambda (c) (print-concept c)(terpri)) cltm*) 
  nil)

(defun print-concept (concept)
  (terpri)(princ "(")(princ (concept-head concept))
  (terpri)(princ " :percepts  ")(princ (concept-percepts concept))
  (cond ((not (null (concept-relations concept)))
	 (terpri)(princ " :relations ")(princ (concept-relations concept))))
  (cond ((not (null (concept-tests concept)))
	 (terpri)(princ " :tests     ")(princ (concept-tests concept))))
  (princ ")"))

(defun pc () (print-concepts))

; SATISFIED inputs a single goal element, with or without unbound 
; variables, and a list of memory elements from CSTM*. The function 
; returns ( flag . bindings ) form, where flag is T if the goal
; is satisfied and NIL otherwise. It calls on
; different functions depending on whether the goal is positive 
; (e.g., (on ?X B)) or negative (e.g., (not (on ?X B))). 
;  NOTE: the bindings represent the match found, in the case
;        of negative literal not satisfied, bindings is the match that 
;        was found that made the negative literal unsatisfied.

(defun satisfied (goal cstm)
  (if (null goal) (list T)
    (cond ((eq (car goal) 'not)
	   (satisfied-negative (cadr goal) cstm))
	  (t (satisfied-positive goal cstm)))))

;TLP_EXP - now it checks if the goal concept is partially matched or not.
; SATISFIED-POSITIVE inputs a positive goal element like (on ?X B) 
; and a list of memory elements from CSTM*. It returns T if the list 
; contains an element that matches the goal with the degree of match of 1.0.
; If the goal is partially matched, it returns the degree of match. Otherwise it
; returns nil.

(defun satisfied-positive (goal cstm)
  (let (result flag)
    (do ((i 0 (1+ i)))
	((or (equal i (length cstm))
	     flag)
	 result)
      (setq result (bmatches goal (nth i cstm) nil))
      (setq flag (car result)))))

;TLP_EXP: modified to return nil if there is a matching concept instance
; with full degree of match, T if there is no such concept instance (fully or
; partially matched), and the maximum degree of match if there is only partially
; matched concept instances.
; SATISFIED-NEGATIVE inputs a negative goal element like (not (on ?X B)) 
; and a list of memory elements from CSTM*. It returns T if the list 
; does NOT contain any elements that match the goal expression and NIL
; if the list contains one or more elements that match the expression with
; the degree of match 1. It returns (1-max. degree of match) if the list
; contains only such expressions with partial degree of match.

(defun satisfied-negative (goal cstm)
  (let ((flag T)
	max max-result result)
    (do ((i 0 (1+ i)))
	((or (equal i (length cstm))
	     (null flag))
	 (cond ((null flag) (cons flag (cdr result)))
	       ((numberp max) (cons (- 1 max) (cdr max-result)))
	       (t (list flag))))
      (setq result (bmatches goal (nth i cstm) nil))
      (cond ((numberp (car result))
	     (cond ((and (numberp max)
			 (> (car result) max))
		    (setq max (car result))
		    (setq max-result result))
;		   ((not (numberp max))
		   ((null max)
		    (setq max (car result))
		    (setq max-result result))))
	    ((car result)
	     (setq flag nil))))))