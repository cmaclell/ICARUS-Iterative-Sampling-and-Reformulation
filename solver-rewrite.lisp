
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Expermental results code
(defparameter nodes-explored* 0)
(defparameter nodes-failed* 0)

(defun get-results ()
  (time (grun))
  (format t "~%~%;;;;;;;;;;;;;;;;~%")
  (format t "No of nodes explored: ~a~%" nodes-explored*)
  (format t "No of nodes failed: ~a~%" nodes-failed*)
  (format t ";;;;;;;;;;;;;;;;~%~%"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code for Icarus Rewrite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter problem-solver-enabled* t)
(defparameter active-problem* nil)
(defparameter executing-intention* nil)
(defparameter constraint-memory* nil)
(defparameter pos-unchainable-conditions* nil)
(defparameter neg-unchainable-conditions* nil)

;; There are three possible options for this global switch:
;;    1. :BACKWARD (default)
;;    2. :FORWARD
(defparameter search-direction* :BACKWARD)

;; There are three possible options for this global switch:
;;    1. :MAX-EFFECTS-MATCHED (default)
;;    2. :MIN-UNSATISFIED-CONDITIONS
;;    3. :BOTH-1-2 (yet to be implemented)
(defparameter skill-selection-heuristic* :MIN-UNSATISFIED-CONDITIONS)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions called from RUN-AUX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun select-active-problem ()
  (when (not active-problem*)
    (loop with unsatisfied-problem-found? = nil
	  for problem in gstm*
	  while (not unsatisfied-problem-found?)
	  do
	  (when (not (problem-satisfied-with-bindings problem))
	    (setq active-problem* problem)
	    (setq unsatisfied-problem-found? t)))))
	
(defun solve (active-problem executing-intention) 
  ;; Expect Problem to be an active Problem that is not satisfied, and executing-intention is an "Executing Intention" or NIL
  (print-problem-and-exec-intent active-problem executing-intention)

  (cond	(executing-intention ; If there is an intention currently executing
	 (execute-one-step executing-intention))
	((and active-problem
	      problem-solver-enabled*)
	 (backward-chain-problem-solve active-problem))
	(problem-solver-enabled*
	 (setf halt* t)
	 (print "No active problem and no executing intention"))))

(defun report-toplevel-problem-satisfied ()
  (format t "~%**************************************************")
  (format t  "~%All Top-Level Goals of Active Problem Satisfied~%")
  (format t "**************************************************~%"))

(defun print-problem-and-exec-intent (active-problem executing-intention)
  (unless pat-trace* 
    (format t "~%Call to SOLVE:"))
  (when problem-solver-enabled*
  (cond (active-problem
	 (format t "~%  Active Problem:")
	 (pprint-problem active-problem))
	  (t (format t "~%  Active Problem: NIL"))))
  (cond (executing-intention
	 (if pat-trace*
	     nil ;(format t "~%Active Intention:")     ; moved to when intention set
	     (format t "~%  Executing Intention:"))
	 (if pat-trace*
	     nil ;(pprint-pat-intention executing-intention)  ; moved to  when intention set
	     (pprint-intention executing-intention)))
	(t (if pat-trace*
	       nil ; (format t "~%Active Intention: None")   ;moved to when intention set
	       (format t "~%  Executing Intention: NIL")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Problem functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct problem
  id					; how are id's generated (for goals, skills, etc.)?
  goals
  objectives
  pos-objectives
  neg-objectives			; note that these still include the NOT's
  bindings
  bindings-selected?
  intention
  focus
  i-parent
  target-goals                          ;; goals that have been set as targets for the selected intention with the matching order preserved
  successful-subskill-executions
  failure-context-list
  disjunctive-goal-list?              ;;denotes if a problem has a set of disjunctive goals.
  visited-states                      ;;stores a list of visited states. Instantiated in the top level problem only.
  repeating-intentions)                   ;;stores a list of repeating intentions. Instantiated in the top level problem only.
 

(defstruct goal 
  objective 
  sfailed 
  cfailed 
  problem 
  status)

(defun make-problem-from-goal-literal-list (list-of-goal-literals)
  (loop with new-problem = (make-problem)
     for objective in list-of-goal-literals
     for new-goal = (make-goal :objective objective)
     do
       (when debug* (print new-goal))
       (setf (goal-problem new-goal)
	     new-problem)
       (push new-goal (problem-goals new-problem))
     finally
     (setf (problem-objectives new-problem)
	   (loop for goal in (problem-goals new-problem)
	      collect (goal-objective goal)))
     (setf (problem-pos-objectives new-problem)
	   (collect-pos-conditions (problem-objectives new-problem)))
     (setf (problem-neg-objectives new-problem)
	   (collect-neg-conditions (problem-objectives new-problem)))
     (return new-problem)))

(defun pprint-problem (problem
		       &key
		       (subst-bindings? t)
		       (indent "    ")
		       (selectors '(:objectives :bindings :bindings-selected?
				    :focus :target-goals :intention)))
  (when problem
    (format t "~%~aPROBLEM:" indent)
    (loop for selector in selectors
	  do
	  (case selector
	    (:objectives (format t "~%~a  Objectives: ~a" 
				 indent
				 (if subst-bindings? 
				     (subst-bindings (problem-bindings problem)
						     (problem-objectives problem))
				     (problem-objectives problem))))
	    (:bindings (format t "~%~a  Bindings: ~a" 
			       indent
			       (problem-bindings problem)))
	    (:bindings-selected?  (format t "~%~a  Bindings-selected?: ~a" 
					  indent
					  (problem-bindings-selected? problem)))
	    (:target-goals (format t "~%~a  Target-Goals: ~a" 
				   indent
				   (if subst-bindings?
				       (subst-bindings (problem-bindings problem)
						       (problem-target-goals problem))
				       (problem-target-goals problem))))
	    (:focus (format t "~%~a  Focus: ~a" 
			    indent
			    (if subst-bindings? 
				(subst-bindings (problem-bindings problem)
						(problem-focus problem))
				(problem-focus problem))))
	    (:intention (let ((short-intention (if (problem-intention problem)
						   (intention-head (problem-intention problem))
						   nil)))
			  (format t "~%~a  Intention: ~a" 
				  indent
				  (if subst-bindings? 
				      (subst-bindings (problem-bindings problem)
						      short-intention)
				      short-intention))))))))

(defun collect-pos-conditions (condition-list)
  (loop for condition in condition-list
     unless (eql (first condition) 'not)
     collect condition))

(defun collect-neg-conditions (condition-list)
  (loop for condition in condition-list
     when (eql (first condition) 'not)
     collect condition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skill and Intention functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct sclause
  head				
  id 
  percepts				; only for primitive skills ?
  tests
  conditions
  effects
  subskills				; NIL for primitive skills
  action				; only a single action (but could be a lisp function to call sequence of actions)
  )

(defstruct intention
  id                   ; id of the skill this intention was instantiated from
  problem-parent
  execution-i-parent
  head                 ; similar to old head, but does not have to match a concept.  Used for calling subskills
  conditions
  effects
  targets	       ; subset of effects serving as terminating conditions (desired effects)
  bindings
  subskills            ; list of subskill calls.  NIL for primitive intentions/skills
  remaining-subskills  ; remaining subskills to execute (previous already succeeded)
  execution-flag       ; T for executing, NIL otherwise
  action               ; only used for primitive intentions.  Then is a single action that can be executed.
  action-executed?     ; used to check if action has been executed at least once
  percepts	       ; only for primitive intentions to use to get bindings for action call
  tests
  successful-subskill-executions
  failed-subskill-executions
 )

(defmacro create-skills (&rest skills)
  `(let ((skills (quote ,skills))
	 result)
     (do ((next-skill (car skills) (car skills)))
	 ((null skills)
	  (setq sltm* (append sltm* (reverse result))) nil)
       (push (create-skill-clause next-skill) result)
       (pop skills))))

(defun create-skill-clause (skill)
  (let ((head (car skill))
	(id nil)
	(percepts nil)
	(tests nil)
	(conditions nil)
	(effects nil)
	(subskills nil)
	(action nil))
    (pop skill)				; get rid of head, rest is alternating :key <spec> pairs
    (do ((next-field (car skill) (car skill))
	 (next-spec (cadr skill) (cadr skill)))
	((null skill)
	 (cond ((null id) (setq id (incf id-count*))))
	 (make-sclause :head head
		       :id id
		       :percepts percepts
		       :tests tests
		       :conditions conditions
		       :effects effects
		       :subskills subskills
		       :action (quote-constants action))) ; check out / find out what this does 
      ;; do keywords need to be quoted below?
      (cond ((eq next-field ':percepts)(setq percepts next-spec))
	      ((eq next-field ':id)(setq id next-spec))
	      ((eq next-field ':tests)(setq tests next-spec))
	      ((eq next-field ':conditions)(setq conditions next-spec))
	      ((eq next-field ':effects)(setq effects next-spec))
	      ((eq next-field ':subskills)(setq subskills next-spec))
	      ((eq next-field ':action)(setq action next-spec))
	      (t (print-error next-field "field" "skill")))
      (setq skill (cddr skill)))))

(defun create-intention (skill bindings &optional execution-flag)
  (let ((new-intention (make-intention :id (sclause-id skill)
				       :head (subst-bindings bindings (sclause-head skill))
				       :conditions (subst-bindings bindings (sclause-conditions skill))
				       :effects  (subst-bindings bindings (sclause-effects skill))
				       :bindings bindings
				       :subskills (subst-bindings bindings (sclause-subskills skill))
				       :execution-flag execution-flag
				       :action (qsubst-bindings bindings (sclause-action skill))
				       :action-executed? nil
				       :percepts (subst-bindings bindings (sclause-percepts skill))
				       :tests  (subst-bindings bindings (sclause-tests skill)))))
    (setf (intention-remaining-subskills new-intention)
	  (intention-subskills new-intention))
    (setf (intention-targets new-intention)
	  (intention-effects new-intention)) ; the default is for targets to be all of effects
    new-intention))

(defun create-dummy-intention-for-concept-chaining (triplet)
  (let ((head (first triplet))
	(conditions (second triplet))
	(bindings (third triplet))
	(result nil))
    
    (cond ((not (eq (car head) 'not))
	   (make-intention :id nil
			   :head head
			   :conditions conditions
			   :effects  head
			   :bindings bindings
			   :subskills nil
			   :execution-flag nil
			   :action nil
			   :action-executed? nil
			   :percepts nil
			   :tests  nil))
	  (t
	   
	   ;;Get bound negations to avoid illegal bindings of negated conditions.
	   (setq result (get-chainable-negations (subst-bindings bindings conditions)))
	   (setq conditions (car result))
	   (setq bindings (append bindings (cadr result)))

	   (make-intention :id nil
			   :head head
			   :conditions conditions
			   :effects  head
			   :bindings bindings
			   :subskills nil
			   :execution-flag nil
			   :action nil
			   :action-executed? nil
			   :percepts nil
			   :tests  nil)))))

(defun primitive-intention? (intention)
  (null (intention-subskills intention)))

(defun pprint-intention (intention
			 &key
			 (subst-bindings? t)
			 (indent "    ") 
			 (selectors '(:head :bindings :subskills
				      :remaining-subskills :conditions
				      :effects :targets :action)))
  (when intention
    (format t "~%~aINTENTION:" indent)
    (loop for selector in selectors
       do
       (case selector
	 (:head (format t "~%~a   Head: ~a" indent
				(if subst-bindings? 
				    (subst-bindings (intention-bindings intention)
						    (intention-head intention))
				    (intention-head intention))))
	 (:bindings (format t "~%~a   Bindings: ~a" indent
			    (intention-bindings intention)))
	 (:subskills (format t "~%~a   Subskills: ~a" indent
			     (if subst-bindings? 
				 (subst-bindings (intention-bindings intention)
						 (intention-subskills intention))
				 (intention-subskills intention))))
	 (:remaining-subskills (format t "~%~a   Remaining-Subskills: ~a" indent
				       (if subst-bindings? 
					   (subst-bindings (intention-bindings intention)
							   (intention-remaining-subskills intention))
					   (intention-remaining-subskills intention))))
	 (:conditions (format t "~%~a   Conditions: ~a" indent
			      (if subst-bindings? 
				  (subst-bindings (intention-bindings intention)
						  (intention-conditions intention))
				  (intention-conditions intention))))
	 (:effects  (format t "~%~a   Effects: ~a" indent
			    (if subst-bindings? 
				(subst-bindings (intention-bindings intention)
						(intention-effects intention))
				(intention-effects intention))))
	 (:targets  (format t "~%~a   Targets: ~a" indent
			    (if subst-bindings? 
				(subst-bindings (intention-bindings intention)
						(intention-targets intention))
				(intention-targets intention))))
	 (:action  (format t "~%~a   Action: ~a" indent
			   (if subst-bindings? 
			       (qsubst-bindings (intention-bindings intention)
						(intention-action intention))
			       (intention-action intention))))))))

(defun pprint-pat-intention (intention
			     &key
			     (subst-bindings? t)
			     (indent "    ") 
			     (selectors '(:head :bindings :conditions
					  :subskills :action :effects)))
  (when intention
    (unless pat-trace*
      (format t "~%~aINTENTION:" indent))
    (loop for selector in selectors
       do
       (case selector
	 (:head (format t "~%~a   Head: ~a" indent
			(if subst-bindings? 
			    (subst-bindings (intention-bindings intention)
					    (intention-head intention))
			    (intention-head intention))))
	 (:bindings (format t "~%~a   Bindings: ~a" indent
			    (intention-bindings intention)))
	 (:subskills (when (intention-subskills intention)
		       (format t "~%~a   Subskills: ~a" indent
			       (insert-caret (- (length (intention-subskills intention))
						(length (intention-remaining-subskills intention)))
					     (if subst-bindings? 
						 (subst-bindings (intention-bindings intention)
								 (intention-subskills intention))
						 (intention-subskills intention))))))
	 (:remaining-subskills (format t "~%~a   Remaining-Subskills: ~a" indent
				       (if subst-bindings? 
					   (subst-bindings (intention-bindings intention)
							   (intention-remaining-subskills intention))
					   (intention-remaining-subskills intention))))
	 (:conditions (format t "~%~a   Conditions: ~a" indent
			      (if subst-bindings? 
				  (subst-bindings (intention-bindings intention)
						  (intention-conditions intention))
				  (intention-conditions intention))))
	 (:effects  (format t "~%~a   Effects: ~a" indent
			    (if subst-bindings? 
				(subst-bindings (intention-bindings intention)
						(intention-effects intention))
				(intention-effects intention))))
	 (:targets  (format t "~%~a   Targets: ~a" indent
			    (if subst-bindings? 
				(subst-bindings (intention-bindings intention)
						(intention-targets intention))
				(intention-targets intention))))
	 (:action (when (intention-action intention)
		    (format t "~%~a   Action: ~a" indent
			    (if subst-bindings? 
				(qsubst-bindings (intention-bindings intention)
						 (intention-action intention))
				(intention-action intention)))))))))

(defun insert-caret (pos list)
  (append (subseq list 0 pos)
	  (list '-->)
	  (subseq list pos)))

(defun set-executing-intention (intention)
  (setf executing-intention* intention)
  (cond (intention
	 (setf (intention-execution-flag intention) t)
	 (cond (pat-trace* 
		(format t "~%Setting Current Intention:")
		(pprint-pat-intention intention)
		;(format t "~%Setting Executing Intention to be: ~a"
		;          (intention-head-with-bindings intention)))
		)
	       (t
	 (format t "~%Setting Executing-Intention* to be:")
		(pprint-intention intention))))
	(t (unless pat-trace*  
	     (format t "~%Setting Executing-Intention* to be: NIL")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Matching Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; predicate -- T if satisfied with any bindings
(defun goal-literal-satisfied? (goal-literal)
  (car (goal-literal-satisfied goal-literal)))

;;; returns (flag . bindings)
(defun goal-literal-satisfied (goal-literal)
  ;;; SATISFIED function defined in holdover.lisp
  (satisfied goal-literal cstm*))

;; substitutes given bindings, and returns (flag . new-bindings) form for attempted match
(defun goal-literal-satisfied-with-bindings (goal-literal bindings)
  (goal-literal-satisfied
   (subst-bindings bindings goal-literal)))


;; doesn't make sense for negative literals
(defun find-all-match-bindings (pos-goal-literal cstm)
  ;; like satisfied-positive, but finds all match bindings
  (loop for belief in cstm
     for match-result = (bmatches pos-goal-literal belief nil)
     when (first match-result)
     collect
     (rest match-result)))

;; returns a list of all possible bindings that satisfy the literals.
;; this takes literals in any order (it separates the positive and negative bindings)
(defun find-all-match-bindings-for-literals (literals start-bindings cstm)
  (find-all-match-bindings-for-sorted-literals (collect-pos-conditions literals)
					       (collect-neg-conditions literals)
					       start-bindings
					       cstm))

;; returns a list of all possible bindings that satisfy the literals.
;;; assume already divided into pos and neg lists
(defun find-all-match-bindings-for-sorted-literals (pos-literals neg-literals start-bindings cstm)
  (cond ((null pos-literals)
	 (if (loop for neg-literal in (subst-bindings start-bindings neg-literals)
		always
		(first (satisfied neg-literal cstm)))
	     (list start-bindings)
	     nil))
	(t
	 (loop for add-bindings in (find-all-match-bindings (subst-bindings start-bindings
									    (first pos-literals))
							    cstm)
	    append
	    (find-all-match-bindings-for-sorted-literals (rest pos-literals) 
							 neg-literals
							 (append add-bindings start-bindings)
							 cstm)))))

;; version of next function that presorts pos and neg literals
;; Returns T or NIL
(defun exists-match-bindings-for-literals? (literals start-bindings cstm)
  (exists-match-bindings-for-sorted-literals? (collect-pos-conditions literals)
					      (collect-neg-conditions literals)
					      start-bindings
					      cstm))

;; Quicker function to check if pos and neg literals are satisfied
;;   Returns T or NIL
;;   Stops when first match (if any) is found
(defun exists-match-bindings-for-sorted-literals? (pos-literals neg-literals start-bindings cstm)
  (cond ((null pos-literals)
	 (loop for neg-literal in (subst-bindings start-bindings neg-literals)
	    always
	    (first (satisfied neg-literal cstm))))
	(t
	 (loop for add-bindings in (find-all-match-bindings (subst-bindings start-bindings
									    (first pos-literals))
							    cstm)
	    thereis
	    (exists-match-bindings-for-sorted-literals? (rest pos-literals) 
							neg-literals
							(append add-bindings start-bindings)
							cstm)))))

;; Checks for the satisfaction of a problem using the bindings within the problem. Returns T or nil.
;; This function does not extend the problem bindings.
(defun problem-satisfied-with-bindings (problem)
  (exists-match-bindings-for-sorted-literals? (problem-pos-objectives problem)
					      (problem-neg-objectives problem)
					      (problem-bindings problem)
					      cstm*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Partial Matching Functions
;;;  (non-greedy, matching against belief memory as structures)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-all-max-size-partial-matches-for-literals (literals start-bindings cstm &optional already-matched-literals)
  (find-all-max-size-partial-matches-for-sorted-literals (collect-pos-conditions literals)
							 (collect-neg-conditions literals)
							 start-bindings
							 cstm
							 already-matched-literals))

(defun find-all-max-size-partial-matches-for-sorted-literals (pos-literals neg-literals start-bindings cstm &optional already-matched-literals)
  (let ((all-partial-matches (find-all-partial-match-bindings-for-sorted-literals pos-literals
										  neg-literals
										  start-bindings
										  cstm
										  already-matched-literals)))
    (loop with max-size-matches = nil
       with max-match-size = 0
       for match-pair in all-partial-matches
       for matched-literals = (first match-pair)
       for match-length = (length matched-literals)
       do
	 (cond ((> match-length max-match-size)
		(setf max-match-size match-length
		      max-size-matches (list match-pair)))
	       ((= match-length max-match-size)
		(push match-pair max-size-matches)))
       finally
	 (return max-size-matches))))

(defun find-all-max-size-partial-matches-for-sorted-literals-for-binding-set (pos-literals neg-literals binding-set cstm &optional already-matched-literals)
  (let ((all-partial-matches nil))

    (loop for bindings in binding-set
	  append (find-all-partial-match-bindings-for-sorted-literals pos-literals
								      neg-literals
								      bindings
								      cstm
								      already-matched-literals)
	  into binding-list
	  finally 
	  (setq all-partial-matches binding-list))
    
    (loop with max-size-matches = nil
	  with max-match-size = 0
	  for match-pair in all-partial-matches
	  for matched-literals = (first match-pair)
	  for match-length = (length matched-literals)
	  when (and (not (member-failure-context-list? (second match-pair)))
		    (not (violates-binding-constraints? (subst-bindings (second match-pair)
									(append pos-literals neg-literals)))))
	  do
	  (cond ((> match-length max-match-size)
		 (setf max-match-size match-length
		       max-size-matches (list match-pair)))
		((= match-length max-match-size)
		 (push match-pair max-size-matches)))
	  finally
	  (return max-size-matches))))

;;; assume already divided into pos and neg lists
;;;  returns list of pairs: ( ( matched-literals-1 bindings-1 ) ( matched-literals-2 bindings-2 ) ....  )
(defun find-all-partial-match-bindings-for-sorted-literals (pos-literals neg-literals start-bindings cstm &optional already-matched-literals)
  (cond ((null pos-literals)
	 (list (list (append already-matched-literals
			     (loop for neg-literal in neg-literals
				for instantiated-neg-literal = (subst-bindings start-bindings neg-literal)
				when (first (satisfied instantiated-neg-literal cstm))
				collect neg-literal))
		     start-bindings)))
	(t
	 (append (find-all-partial-match-bindings-for-sorted-literals (rest pos-literals) 
								      neg-literals
								      start-bindings
								      cstm
								      already-matched-literals)
		 (loop for add-bindings in (find-all-match-bindings (subst-bindings start-bindings
										    (first pos-literals))
								    cstm)
		    append
		    (find-all-partial-match-bindings-for-sorted-literals (rest pos-literals) 
									 neg-literals
									 (append add-bindings start-bindings)
									 cstm
									 (cons (first pos-literals)
									       already-matched-literals)))))))

(defun move-negs-to-end (goals negs)
 (cond ((null goals) negs)
       ((eq (caar goals) 'not)
        (move-negs-to-end (cdr goals) (cons (car goals) negs)))
       (t (cons (car goals) (move-negs-to-end (cdr goals) negs)))))

;; Matches a given set of literals with variables with another set. Renamed from get-satisfied.
(defun pattern-match-literals (goals beliefs &optional return-bindings?)
 (let ((result nil)
       (bindings nil))
   (do ((gnext (car goals) (car goals)))
       ((null goals)
        (if return-bindings?
            (list result bindings)
            (subst-bindings bindings result)))
       (let ((tbindings (pattern-match-literal gnext beliefs bindings)))
         (cond ((not (null tbindings))
                (setq bindings (cdr tbindings))
                (push gnext result))))
       (pop goals))))

;; Matches positive and negative literals seperately. Renamed from match-goal.
(defun pattern-match-literal (goal beliefs bindings)
 (cond ((eq (car goal) 'not)
        (pattern-match-neg-literal (cadr goal) beliefs bindings))
       (t (pattern-match-pos-literal goal beliefs bindings))))

;; Renamed from match-pos-goal
(defun pattern-match-pos-literal (goal beliefs bindings)
 (cond ((null beliefs) nil)
       (t (let ((tbindings (pattern-matcher goal (car beliefs) bindings)))
            (cond ((null (car tbindings))
                   (pattern-match-pos-literal goal (cdr beliefs) bindings))
                  (t tbindings))))))

;; Renamed from match-neg-goal
(defun pattern-match-neg-literal (ngoal beliefs bindings)
 (cond ((null beliefs)
        (cons t bindings))
       (t (let ((tbindings (pattern-matcher ngoal (car beliefs) bindings)))
            (cond ((not (null (car tbindings))) nil)
                  (t (pattern-match-neg-literal ngoal (cdr beliefs) bindings)))))))

;; Same as bmatches but does not require belief structures. Renamed from bmatches-pat.
(defun pattern-matcher (clist elist bindings)
 (cond ((not (eq (length clist) (length elist)))
        (cons nil bindings))
       (t (let ((flag t))
            (do ((c (car clist) (car clist))
                 (e (car elist) (car elist))
                 (b nil nil))
                ((or (null flag) (null clist))
                 (cons flag bindings))
                (cond ((not (variablep c))
                       (cond ((not (equal c e))(setq flag nil))))
                      ((setq b (assoc c bindings))
                       (cond ((not (equal (cdr b) e))
                              (setq flag nil))))
                      (t (push (cons c e) bindings)))
                (pop clist)
                (pop elist))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXECUTION routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *repeat-macros?* nil)	        ; control whether to fail or allow repeat if macro targets not satisfied at end
(defparameter *complete-macro-execution?* t)	; control whether to require complete execution of macro subskills (or interrupt when targets satisfied?)


(defun execute-one-step (executing-intention)
  (cond ((execution-completed-successfully? executing-intention) ; modify so in special case of Null Effects, will execute exactly once (both primitive and macro)
	 (let ((current-state (get-current-state cstm*)))
 	   (record-success executing-intention)
 	   (return-to-caller executing-intention) ; should update executing-intention*, transmit new bindings, advance to next subskill
 	   
	   ;;Avoid checking for execution loop while popping of to an intention parent.
	   (when (intention-problem-parent executing-intention)
	     (cond ((repeated-state? current-state)
		    (print "Execution loop detected. Storing the current intention-head.")
		    (setq execution-loop* t)
		    (store-intention (problem-intention active-problem*)))
		   (t
		    (store-state current-state))))))
	
	((primitive-intention? executing-intention)
	 (cond ((ready-to-execute? executing-intention) ; this should check conditions, extend bindings (with percepts as well),
					                ; and check if all variables of action-form are bound
		(cond ((execute-action executing-intention) ; instantiate action, evaluate (run),
		       t)				    ; let next cycle check if targets satisfied
		      (t 
		       ;(record-failure executing-intention)   ;; record/save failed intention in caller-intention or problem
		       (abort-execution 
			executing-intention
			(format nil
				"Failed to Execute-action: ~a"
				(qsubst-bindings (intention-bindings executing-intention)
						 (intention-action executing-intention)))
			))))
	       (t (abort-execution executing-intention "Not ready to execute"))))
	;; Macro intention / execute subskills
	((or (intention-started? executing-intention) ; conditions only need to be satisfied when first started
	     (ready-to-execute? executing-intention)) ; checks conditions and gets bindings
	 (let* ((remaining-subskills (intention-remaining-subskills executing-intention))
		(next-subskill (if remaining-subskills
				   (subst-bindings (intention-bindings executing-intention)
						   (first remaining-subskills)))))
	   (cond (next-subskill
		  (cond ((call-intention next-subskill executing-intention)
			 t)
			(t
			 ;; call failed
			 (abort-execution executing-intention (format nil
								      "Call to subskill failed: ~a"
								      next-subskill)))))
		 (*repeat-macros?*      ;; only get here if no next subskill and targets not satisfied
		  ;; targets can't be satisfied here (otherwise execution-completed-successfully? would have returned t)
		  ;; (targets-satisfied? executing-intention)    [NOTE: previous line reasoning may not be valid for executing skills w/ "Null Effects",
		  ;;                                                  but in that case expect *repeat-macros?* to be NIL, so still won't get here]
		  (setf (intention-remaining-subskills executing-intention) ; reset to try executing macro again
			(intention-subskills executing-intention)))
		 (t   ;; fail
		  (abort-execution executing-intention "Aborting execution of macro-intention because targets not satisfied")
		  ))))
	(t 
	 (abort-execution executing-intention "Aborting execution of macro-intention because Ready-to-Execute returned NIL")
	 (format t "~%  Intention that was Not Ready-to-execute:")
	 (pprint-intention executing-intention)
	 )))

 ; modify so in special case of Null Effects, will execute exactly once (both primitive and macro)
(defun execution-completed-successfully? (executing-intention)
  (cond ((and (null (intention-effects executing-intention)) 
	      (primitive-intention? executing-intention))
	 ;; new code for primitive intentions with null effects
	 ;; NOTE: problem-solving will never backchain to a skill with no effects!
	 (intention-action-executed? executing-intention))
	(t (and (or (not *complete-macro-execution?*) ; old code for when effects are not empty
		    (null (intention-remaining-subskills executing-intention)))
		(targets-satisfied? executing-intention)))))

(defun record-success (intention)
  (when pat-trace*
    (format t "~%INTENTION COMPLETED SUCCESSFULLY: ~a"
	    (intention-head-with-bindings intention)))
  (let ((i-parent (intention-execution-i-parent intention)))
    (cond (i-parent
	   (push intention
		 (intention-successful-subskill-executions i-parent)))
	  (t
	   (let ((problem-parent (intention-problem-parent intention)))
	     (if problem-parent
		 (push intention (problem-successful-subskill-executions problem-parent))))))))

(defun intention-head-with-bindings (intention)
  (when intention
    (subst-bindings (intention-bindings intention)
		    (intention-head intention))))

#|(defun record-failure (intention)
  (let ((i-parent (intention-execution-i-parent intention)))
    (cond (i-parent
	   (push intention
		 (intention-failed-subskill-executions i-parent)))
	  (t
	   (let ((problem-parent (intention-problem-parent intention)))
	     (if problem-parent
		 (push intention (problem-failed-subskill-executions problem-parent))))))))|#
  
;; should update executing-intention*, transmit new bindings, advance to next subskill
;; updates executing-intention*, and passes back any new bindings through head
(defun return-to-caller  (satisfied-intention) ; assumes targets verified to be satisfied (so bindings updated)
  (let ((next-subskill-called? nil)
	(i-parent (intention-execution-i-parent satisfied-intention))
	(instantiated-head (subst-bindings (intention-bindings satisfied-intention)
						   (intention-head satisfied-intention))))
    (cond (i-parent
	   ;; pass back bindings to i-parent
	   (let* ((i-parent-bindings (intention-bindings i-parent))
		  (i-parent-head (subst-bindings i-parent-bindings
							 (first (intention-remaining-subskills i-parent)))) ; is this right?????
		  (new-bindings-form (unify-match instantiated-head i-parent-head))
		  (full-bindings (append (if (first new-bindings-form)
					     (rest new-bindings-form)
					     nil)
					 i-parent-bindings)))
	     (setf (intention-bindings i-parent)
		   full-bindings))
	   ;; advance to next subskill
	   (pop (intention-remaining-subskills i-parent))
	   ;; when pat-trace* try going directly to next subskill
	   (when (and pat-trace*
		      (intention-remaining-subskills i-parent)
		      ;; also check repeat-macros? and targets ??
		      )
	     (format t "~%Proceeding directly to next subskill of intention: ~a"
		     (intention-head-with-bindings i-parent))
	     (let* ((remaining-subskills
		     (intention-remaining-subskills i-parent))
		    (next-subskill (subst-bindings (intention-bindings i-parent)
						   (first remaining-subskills))))
	       (call-intention next-subskill i-parent)
	       (setf next-subskill-called? t))))
	  ;; only do the following if there is actually a problem-parent
	  ((intention-problem-parent satisfied-intention)
	   (return-bindings-to-problem-caller satisfied-intention)))
    (unless next-subskill-called?
      (set-executing-intention i-parent))))

;; this is used when the satisfied-intention has no i-parent
(defun return-bindings-to-problem-caller (satisfied-intention)
  (let* ((target-literals (subst-bindings (intention-bindings satisfied-intention)
					  (intention-targets satisfied-intention)))
	 (problem-parent (intention-problem-parent satisfied-intention))
	 (target-goal-literals (problem-target-goals problem-parent))
	 (match-form			; ( flag . bindings )
	  (unify-match target-literals 
		       target-goal-literals))
	 (match-flag (car match-form))
	 (match-bindings (rest match-form)))
    (cond (match-flag	; unify-match succeeded
	   (setf (problem-bindings problem-parent)
		 (merge-bindings (problem-bindings problem-parent)
				 match-bindings))) ; note: these bindings dominate, if inconsistent
	  (t  ; match failed - shouldn't happen
	   (warn "Unify Match Failed - this shouldn't happen")))))

;; merge bindings when consistent
;;    if inconsistent, use binding from bindings-2
;;      (think "merge bindings-1 into bindings-2, unless inconsistent)
(defun merge-bindings (bindings-1 bindings-2)
  (loop with merged-bindings = bindings-2
     for bindings-1-form in bindings-1
     for var = (car bindings-1-form)
     for value = (cdr bindings-1-form)
     for bindings-2-lookup = (assoc var bindings-2)
     do
       (cond (bindings-2-lookup
	      (cond ((equal value (cdr bindings-2-lookup))
		     ;; consistent and already there - do nothing
		     )
		    (t ;; inconsistent - keep bindings-2 binding
		     (warn "For Var ~a, inconsitent values ~a and ~a, keeping latter"
			   var value (cdr bindings-2-lookup)))))
	     (t				; var not bound in bindings-2
	      (push bindings-1-form 
		    merged-bindings)))
     finally
       (return merged-bindings)))


;; this should verify that conditions match beliefs, and extend bindings appropriately
;; Returns T or NIL
(defun conditions-satisfied? (intention)
  (let* ((bindings (intention-bindings intention))
	 (preconds (subst-bindings bindings (intention-conditions intention)))
	 (all-match-bindings
	  (find-all-match-bindings-for-literals preconds
						bindings
						cstm*)))
    (cond (all-match-bindings
	   (setf (intention-bindings intention)
		 (random-choose all-match-bindings))
	   t)
	  (t nil))))

;; this should verify that targets match beliefs, and extend bindings appropriately
;; Returns T or NIL
(defun targets-satisfied? (intention)
  (let* ((bindings (intention-bindings intention))
	 (raw-targets (intention-targets intention)))
    (when (exists-match-bindings-for-literals? raw-targets bindings cstm*)
      (let* ((instantiated-targets (subst-bindings bindings raw-targets))
	     (all-match-bindings
	      (find-all-match-bindings-for-literals instantiated-targets
						    bindings
						    cstm*)))
	(when all-match-bindings
	  (setf (intention-bindings intention)
		(random-choose all-match-bindings)))
	t))))


(defun random-choose (list)
  (nth (random (length list)) list))

(defun variable-free? (form)
  (cond ((variable-p form) nil)
	((atom form) t)
	(t (and (variable-free? (first form))
		(variable-free? (rest form))))))

(defun ready-to-execute? (exec-intent)
  (when (and (intention-execution-flag exec-intent) ; could assume this is always true when called
	     (conditions-satisfied? exec-intent))
    ;(intention-started? exec-intent)
    ;(setf (intention-already-started? exec-intent) T)
    (when (primitive-intention? exec-intent)
      (extend-bindings-with-percepts exec-intent))
    t))

(defun intention-started? (exec-intent)
  (not (eql (intention-remaining-subskills exec-intent)
	    (intention-subskills exec-intent))))


(defun extend-bindings-with-percepts (exec-intent)
  (let* ((bindings (intention-bindings exec-intent))
	 (percepts (subst-bindings bindings (intention-percepts exec-intent)))
	 (pmatch (match-pconds percepts pstm* nil nil))
	 (add-bindings nil))
    (cond (pmatch
	   (setf add-bindings (first (random-choose pmatch)))
	   (loop for binding-pair in add-bindings
	      when (variable-p (first binding-pair))
	      do
		(push binding-pair bindings)
	      finally
		(setf (intention-bindings exec-intent)
		      bindings))
	   t)
	  (t nil))))

;; predicate - returns T if succeeds, NIL otherwise
(defun execute-action (exec-intent)
  (let ((action-form (qsubst-bindings (intention-bindings exec-intent)
				      (intention-action exec-intent))))
    (format t "~%Executing Action Form: ~a" action-form)
    (cond ((variable-free? action-form)
	   (eval action-form)
	   ;; next for null subskills hack - guarantee action gets executed exactly once
	   (setf (intention-action-executed? exec-intent) t)
	   t)
	  (t (format t "~%  Failed because action-form contained unbound variables")
	     nil))))

(defun abort-execution (exec-intent &optional (message "Aborting Execution"))
  (format t "~% ~a" message)
  ;; Need to (unwind-and-record failed intentions)
  (when active-problem*
    (create-and-store-failure-context (problem-bindings active-problem*)
				      (problem-focus active-problem*)
				      (problem-intention active-problem*))
    ;;Set the bindings to nil to take care of the fact that the system 
    ;;might have executed actions before failing.
    (setf (problem-bindings-selected? active-problem*) nil)
    (setf (problem-bindings active-problem*) nil))
  (set-executing-intention nil))

;; Question: Does a head have to be completely instantiated?
;;      NO -- Free Variables are possible, and can get bound during execution
;; Predicate: return T or NIL. Side effects: try to create-intention for head, link to calling-intention
(defun call-intention (head &optional calling-intention) ; head already has bindings substituted
  (cond (pat-trace*
	 (format t "~%SKILL CALL: ~a " head))
	(t (format t "~%CALLING INTENTION: ~a " head)))
  (let* ((candidate-skills (find-skills-with-name (first head)))
	 (eligible-skills-with-bindings
	  (loop for skill in candidate-skills
	     for skill-head = (sclause-head skill)
	     for binding-form = (unify-match head skill-head)
	     when (and binding-form (first binding-form))
	     collect (cons skill (rest binding-form))))
	 (matching-skills ; list of (skill . bindings) that match conditions of skill
	  (loop for (skill . bindings) in eligible-skills-with-bindings
	     for conditions = (subst-bindings bindings (sclause-conditions skill))
	     for all-matches = (find-all-match-bindings-for-literals conditions bindings cstm*)
	     when all-matches
	     collect (cons skill (random-choose all-matches)) ; randomly choose one way to sastisfiy conditions
	     ))
	 (chosen-skill-match (if matching-skills
				 (random-choose matching-skills))))
    (cond (chosen-skill-match
      (let* ((skill (first chosen-skill-match))
	     (bindings (rest chosen-skill-match))
	     (new-intention (create-intention skill bindings t)))
	(setf (intention-execution-i-parent new-intention)
	      calling-intention)
	(set-executing-intention new-intention)
	t
	     ))
	  (t (format t "~%CALL TO INTENTION ~a FAILED BECAUSE NO MATCH FOUND" head)))
    ))


(defun find-skills-with-name (skill-name)
  (loop for next-skill in sltm*
     for next-skill-name = (first (sclause-head next-skill))
     when (eql skill-name next-skill-name)
     collect
     next-skill))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROBLEM SOLVING (backward-chaining)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Alternatively the strategy of "random-restarts-on-failure" could be used.
(defun backward-chain-problem-solve (problem)
  (cond
    
    ;; Step 1. Select bindings
    ((null (problem-bindings-selected? problem))
     (print "Selecting bindings.")
     (let ((result nil))
       (setq result (select-bindings problem))
       (process-select-bindings-result problem result)))

    #|
    ;; Step 2. Select focus
    ((or (not (problem-focus problem))
	 (goal-literal-satisfied? (problem-focus problem)))
     (print "Selecting focus.")
     (setf (problem-intention problem) nil)
     (setf (problem-focus problem) (select-focus problem))

     (cond ((problem-focus problem)
	    (print "Focus selected, proceeding to select a skill for the focus")
	    (print (problem-focus problem)))
	   (t
	    (print "Failed to select focus for the current problem with the given bindings. Backtracking.")
	    (create-and-store-failure-context (problem-bindings problem))
	    (setf (problem-bindings problem) nil)
	    (setf (problem-bindings-selected? problem) nil))))
    |#

    ;; Step 3. Select Skill
    ((not (problem-intention problem))
     (let ((result-triple nil)
	   (intention nil)
	   (focus-breakup-triplet nil))
       (print "Selecting skill.")
       (setq result-triple (select-skill problem))
       
       (cond (result-triple
	      (setq intention 
		    (create-intention (first result-triple) (second result-triple)))
	      (setf (intention-problem-parent intention) problem)
	      (setf (problem-intention problem) intention)
	      
	      ;;TODO: This needs to be modified due to removal of focus-goal
	      ;;(establish-correspondence-for-intention-targets problem)

	      (print "Skill selected")
	      (pprint-intention intention)

	      (make-problem-from-conditions intention)
	      (incf nodes-explored*))

	     ;; select-skill failed. Try concept chaining before recording failure.
	     (t
	      (print "Failed to select skill for the current problem with given bindings. Backtracking.")
	      (create-and-store-failure-context (problem-bindings problem))
	      (setf (problem-bindings problem) nil)
	      (setf (problem-bindings-selected? problem) nil)
	      ))))
	      #|
	      (print "Failed to select skill for the current problem with given bindings and focus. Trying to chain on concept.")
	      
	      ;; Concept chaining.
	      (setq focus-breakup-triplet (find-matching-concept (problem-focus problem)
								 :problem-bindings (problem-bindings problem)
								 :ignore-failure-context? nil))
	      ;; format of focus-breakup-triplet is (head relations bindings)
	      (cond ((and focus-breakup-triplet
			  (second focus-breakup-triplet)) ; Relations must be non-NIL in order to chain --Glenn 10-13-10
		     (setq intention 
			   (create-dummy-intention-for-concept-chaining focus-breakup-triplet))
		     (setf (intention-problem-parent intention) problem)
		     (setf (problem-intention problem) intention)
	      
		     ;; Should (establish-correspondence-for-intention-targets problem) called for a dummy intention,
		     ;; especially when this intention would never actually executed.

		     (print "Concept found for chaining. Dummy intention for the concept created.")
		     (pprint-intention intention)

		     (make-problem-from-conditions intention)
		     
		     (incf nodes-explored*)
		     
		     ;;If chaining of a negative goal, flag the new problem as disjunctive.
		     (if (eq (car (problem-focus problem)) 'not)
			 (setf (problem-disjunctive-goal-list? active-problem*)
			       t)))
		    (t
		     (print "No concept found for concept-chaining for the focus of the current problem. Backtracking.")
		     (create-and-store-failure-context (problem-bindings problem)
						       (problem-focus problem))
		     (setf (problem-focus problem) nil)))))))
	      |#
    (t
     (warn "You should never be here in backward-chain-problem-solve."))))

;; This function generates maximal match for the list of problem goal-literals and randomly selects one.
;; Returns nil if none of the goal-literals match, (nil . bindings) if there is a partial match and
;; (t . bindings) if all goal literals are matched.
;; This function extends the problem bindings(if any) with the bindings of the chosen  maximal match.
(defun select-bindings (problem)
  (let ((pos-literals nil)
	(neg-literals nil)
	(intermediate-bindings nil)
	(results nil)
	(selected-match nil)
	(return-value nil))

    ;; Extend problem bindings with bindings from unchainable goals of the problem.
    (loop for pos-objective in (problem-pos-objectives problem)
	  do
	  (when (member (car pos-objective) pos-unchainable-conditions* :test #'eq)
	    (push pos-objective pos-literals)))
    
    (loop for neg-objective in (problem-neg-objectives problem)
	  do
	  (when (member (caadr neg-objective) neg-unchainable-conditions* :test #'eq)
	    (push neg-objective neg-literals)))
	  
    (setq intermediate-bindings (find-all-match-bindings-for-sorted-literals pos-literals 
									     neg-literals
									     (problem-bindings problem)
									     cstm*))

    (setq results (find-all-max-size-partial-matches-for-sorted-literals-for-binding-set (problem-pos-objectives problem)
											 (problem-neg-objectives problem)
											 intermediate-bindings 
											 cstm*))
    
    (when results
      (setq selected-match (random-choose results))
      
      (cond ((and (problem-disjunctive-goal-list? problem)
		  (>= (length (car selected-match))
		      1))
	     (setq return-value (cons t (second selected-match))))
	    ((and (null (problem-disjunctive-goal-list? problem))
		  (= (length (problem-goals problem))
		     (length (car selected-match))))
	     (setq return-value (cons t (second selected-match))))
	    (t
	     (setq return-value (cons nil (second selected-match))))))
    return-value))

;;Store appropriate context in parent and pop-up to parent problem.
(defun backtrack-to-parent-problem (problem)
  (cond ((problem-i-parent problem)
	 (print "Storing failure context and backtracking to parent.")
	 (setq active-problem* (intention-problem-parent (problem-i-parent problem)))
	 (create-and-store-failure-context (problem-bindings active-problem*)
					   (problem-focus active-problem*)
					   (problem-intention active-problem*))
	 (setf (problem-bindings active-problem*) nil)
	 (setf (problem-bindings-selected? active-problem*) nil)
	 (incf nodes-failed*)
	 )
	(t
	 (warn "Failed to select bindings for the top level problem.")
	 (setq active-problem* nil))))
)

;;Store appropriate context in parent and return to the root problem.
(defun backtrack-to-root-problem (problem)
  (cond ((problem-i-parent problem)
	 (print "Storing failure context and backtracking to root.")
	 (setq active-problem* (intention-problem-parent (problem-i-parent problem)))
	 (create-and-store-failure-context (problem-bindings active-problem*)
					   (problem-focus active-problem*)
					   (problem-intention active-problem*))
	 (setf (problem-bindings active-problem*) nil)
	 (setf (problem-bindings-selected? active-problem*) nil)
	 (incf nodes-failed*)
	 (backtrack-to-root-problem-rec active-problem*)
	 )
	(t
	 (warn "Failed to select bindings for the top level problem.")
	 (setq active-problem* nil))))

;;resets all the bindings to nil and sets the active problem to the root problem
(defun backtrack-to-root-problem-rec (problem)
  (cond ((problem-i-parent problem)
	 (setq active-problem* (intention-problem-parent (problem-i-parent problem)))
	 (setf (problem-bindings active-problem*) nil)
	 (setf (problem-bindings-selected? active-problem*) nil)
	 (backtrack-to-root-problem-rec active-problem*))
	(t
	 nil)))    



(defun process-select-bindings-result (problem result)
  (cond ((null result)
	 ;;This means failure
	 (print "Failed to select bindings for the current problem. ")

	 ;;Select a backtracking mechanism here (backtrack to parent or to root).
	 ;;Backtracking to the root can be used for iterative sampling.
	 ;(backtrack-to-parent-problem problem))
	 (backtrack-to-root-problem problem))
	
	((null (car result))
	 ;; This means that bindings were successfully found for a problem that has unsatisfied goals.
	 (cond ((not (repeated-problem? problem 
					(cdr result)))
		;; This problem has not been encountered till now. Proceed to focus selection.
		(setf (problem-bindings problem) (cdr result))
		(setf (problem-bindings-selected? problem) t)
	
		;; Set both focus and intention to NIL to make sure new ones get chosen
		(setf (problem-focus problem) nil
		      (problem-intention problem) nil)

		(print "Bindings selected.")
		(print (problem-bindings problem))
		
		(format t "~% Unsatisfied goals for selected bindings: (")
		(loop for goal in (subst-bindings (problem-bindings problem)
						  (problem-objectives problem))
		      when (not (goal-literal-satisfied? goal))
		      do
		      (format t "~a" goal))
		(format t ")~%"))
	       (t
		(print "Found a repeated problem! Reporting the current bindings as a failure.")
		(create-and-store-failure-context (cdr result)))))

	((car result)
	 ;; This means that the bindings found are such that all goals of the current problem are satisfied.
	 (cond ((and (problem-i-parent problem)
		     (intention-id (problem-i-parent problem)))
		
		(cond ((and execution-loop*
			    (member (intention-head (problem-i-parent problem))
				    (get-repeating-intentions)
				    :test #'equal))
		       (print "Currrent intention matches a repeating intention. Aborting its execution to break execution loop. Backtracking.")
		       (reset-loop-traces)
		       (setq active-problem* (intention-problem-parent (problem-i-parent problem)))
		       (create-and-store-failure-context (problem-bindings active-problem*)
							 (problem-focus active-problem*)
							 (problem-intention active-problem*))
		       (setf (problem-intention active-problem*) nil)
		       (incf nodes-failed*))
		      (t
		       (print "All skill conditions satisfied, proceeding to execution")
		       
		       (setq active-problem* (intention-problem-parent (problem-i-parent problem)))
		       ;; NOTE: This is another way in which establish-correspondence-for-intention-targets
		       ;; could be used.
		       (setf (intention-bindings (problem-i-parent problem))
			     (append (intention-bindings (problem-i-parent problem))
				     (cdr result)))
		       ;(establish-correspondence-for-intention-targets active-problem*)

		       (set-executing-intention (problem-i-parent problem))
		       (setf (problem-bindings-selected? active-problem*) nil))))
	       
	       ((and (problem-i-parent problem)
		     (null (intention-id (problem-i-parent problem))))
		(print "All conditions of the chained-concept satisfied, selecting new bindings and focus for parent problem.")
		(setq active-problem* (intention-problem-parent (problem-i-parent problem)))
		(setf (problem-bindings-selected? active-problem*) nil))
	       (t
		(report-toplevel-problem-satisfied)
		(setq active-problem* nil))))))

#|
;; This function selects a focus goal literal. It tries to match candidate goals against constraints
;; and if any applicable constraint is found it orders them as specified by the constraint.
(defun select-focus (problem)
  (let ((goals-added nil)
	(unsatisfied-goals nil)
	(goals-to-delete nil))

    ;; Step 1: Get all the goals in the current problem that are unsatisfied and that
    ;; have not failed before.
    (loop for goal in (subst-bindings (problem-bindings problem) 
				      (problem-objectives problem))
	  when (and (not (goal-literal-satisfied? goal))
		    (not (member-failure-context-list? (problem-bindings problem)
						       goal)))
	  do
	  (push goal unsatisfied-goals))

    ;; Step 2: Get all the goals added by add constraints and all the goals
    ;; deleted by ordering constraints.
    (loop for constraint in constraint-memory*
	  when (constraint-active? constraint unsatisfied-goals)
	  do
	  (cond ((constraints-add constraint)
		 (setq goals-added (append (subst-bindings (constraints-bindings constraint)
							   (constraints-add constraint))
					   goals-added))
		 (print "Found an active add constraint."))
		((constraints-delete constraint)
		 (setq goals-to-delete (append (subst-bindings (constraints-bindings constraint)
							       (constraints-delete constraint))
					       goals-to-delete))
		 (print "Found an active delete constraint."))))
    
    ;; Step 3: Remove all goals marked for deletion.
    (loop for goal in goals-to-delete
	  do
	 (setf goals-added (delete goal goals-added :test #'equal))
	 (setf unsatisfied-goals (delete goal unsatisfied-goals :test #'equal)))
    
    ;; Step 4: Select a focus.
    (cond (goals-added
	   (random-choose goals-added))
	  (unsatisfied-goals
	   (random-choose unsatisfied-goals))
	  (t
	   nil))))
|#	   
	
;; returns random-choice of (skill . bindings) or NIL if nothing found
;; where bindings make some effect of skill unify with goal-objective
(defun select-skill (problem &optional (bind-unchainables? t))
  ;; members of candidate-skills are of the form (effect sclause  bindings)
  (let ((candidate-skills nil)
	;;(focus-goal (problem-focus problem))
	(selected-triple nil))

    #|
    (loop for skill in sltm*
	  do
	  (loop for effect in (sclause-effects skill)
		for (flag . bindings ) = (unify-match focus-goal effect)
		do
		(when flag
		  (push (list effect skill bindings)
			candidate-skills))))
    (when debug*
      (print "**** Candidates Matching Focus ****")
      (mapcar #'print candidate-skills))
    |#

    (when bind-unchainables?
      (setf candidate-skills
	    (loop for sclause in sltm*
		  append (find-all-candidates-satisfying-unchainable-conditions sclause nil))))
    (when debug*
      (print "**** Candidates with Unchainable Bindings ****")
      (mapcar #'print candidate-skills))

    (loop with results = nil
	  for (sclause bindings rest) in candidate-skills
	  when (not (member-failure-context-list? (problem-bindings problem)
						  (problem-focus problem)
						  (create-intention sclause bindings)))
	  do
	  (push (list sclause bindings rest) results)
	  finally 
	  (setq candidate-skills results))

    (when candidate-skills
      (let ((max-effects-matched-set nil)
	    (min-unsatisfied-conditions-set nil))
	
	(case search-direction*
	  (:BACKWARD
	   (loop with best-triplets = nil
		 with max-value = -1
		 for triplet in candidate-skills
		 for current-value = (max-effects-matched-heuristic (butlast triplet) problem)
		 do
		 (cond 
		   ((< max-value current-value)
		    (setq max-value current-value
			  best-triplets (list triplet)))
		   ((= max-value current-value)
		    (push triplet best-triplets)))
		 finally
		 (setq max-effects-matched-set best-triplets)
		 (if max-effects-matched-set
		     (setq selected-triple (random-choose max-effects-matched-set)))))
	  
	  (:FORWARD
	   (loop with best-triplets = nil
		 with min-value = nil
		 for triplet in candidate-skills
		 for current-value = (min-unsatisfied-conditions-heuristic (butlast triplet))
		 do
		 (cond 
		   ((or
		     (null min-value)
		     (> min-value current-value))
		    (setq min-value current-value
			  best-triplets (list triplet)))
		   ((= min-value current-value)
		    (push triplet best-triplets)))
		 finally
		 (setq min-unsatisfied-conditions-set best-triplets)
		 (if min-unsatisfied-conditions-set
		     (setq selected-triple (random-choose min-unsatisfied-conditions-set))))))
	
	;;(print max-effects-matched-set)
	;;(print min-unsatisfied-conditions-set)
	;;(print selected-triple)
	))

    ;;Restore original bindings and discard bindings generated due to binding unchainable conditions.
    (if selected-triple
	(setf (nth 1 selected-triple) (nth 2 selected-triple)))
    (butlast selected-triple)))

(defun find-all-candidates-satisfying-unchainable-conditions (sclause bindings)
  (let* ((unchainable-conditions (get-unchainable-relations (sclause-conditions sclause)))
	 (good-bindings (find-all-match-bindings-for-literals unchainable-conditions
							      bindings
							      cstm*)))
    (loop for good-binding in good-bindings
	 collect (list sclause good-binding bindings))))

(defun max-effects-matched-heuristic (pair problem)
  (let* ((goal-literals (problem-objectives problem))
	 (unsatisfied-goal-literals nil)
	 (skill (car pair))
	 (bindings (second pair))
	 (target-literals (subst-bindings bindings
					  (sclause-effects skill)))
	 (result nil))

    (loop for goal in (subst-bindings (problem-bindings problem)
				      goal-literals)
	  when (not (goal-literal-satisfied? goal))
	  do
	  (push goal unsatisfied-goal-literals))

    ;; result is  NIL or (bindings matches-1 matches-2)
    (setq result
	  (repeat-greedy-partial-unify-with-targets unsatisfied-goal-literals 
						    target-literals))
    (if result
	(length (second result))
	0)))

(defun min-unsatisfied-conditions-heuristic (pair)
  (let ((skill (car pair))
	(bindings (second pair)))
    (find-number-of-unsatisfied-conditions skill bindings)))

(defun find-number-of-unsatisfied-conditions (skill bindings)
  (let ((max-match-list nil)
	(max-match nil))

    (setq max-match-list
	  (find-all-max-size-partial-matches-for-literals (sclause-conditions skill) bindings cstm*))
    
    ;; NOTE: This is a possible backtrack point but specific to the min-unsatisfied-conditions heuristic.
    (setq max-match (random-choose max-match-list))

    (- (length (sclause-conditions skill))
       (length (car max-match)))))

(defun find-matching-concept (focus &key problem-bindings ignore-failure-context?)
  (let* ((negated-focus? (eq (car focus) 'not))
	 (to-match (cond (negated-focus?
			  (cadr focus))
			 (t
			  focus))))
    
    (loop with result = nil
	  for concept in cltm*
	  ;; Note: Here the order of arguments to unify-match matters.
	  for (flag . bindings) = (unify-match to-match
					       (concept-head concept))
	  while (not result)
	  when (and flag
		    (or ignore-failure-context?
			(not (concept-member-of-failure-context-list? problem-bindings 
								      focus
								      (if negated-focus?
									  (car (get-chainable-negations (subst-bindings bindings
															(concept-relations concept))))
									  (concept-relations concept))))))
	  do
	  (push (list focus
		      (concept-relations concept)
		      bindings)
		result)
	  finally
	  (when result
	    (return (random-choose result))))))

(defun repeated-problem? (problem bindings)
  (let ((goal-list (subst-bindings bindings
				   (problem-objectives problem))))

    (loop with flag = nil
	  with parent = (if (problem-i-parent problem)
			    (intention-problem-parent (problem-i-parent problem))
			    nil)
	  for target-list = (if parent
				(subst-bindings (problem-bindings parent)
						(problem-objectives parent)))
	  while (and parent 
		     (null flag))
	  do
	  (if (exists-subset-exact-match-with-permuted-literals target-list goal-list)
	      (setq flag t))
	  (if (problem-i-parent parent)
	      (setq parent (intention-problem-parent (problem-i-parent parent)))
	      (setq parent nil))

	  finally	      
	  (return flag))))

;; make a new problem from the conditions of an intention, substitute the bindings from the intention
;; and set it as an active-problem.
(defun make-problem-from-conditions (intention)
  (let ((problem nil)
	(instantiated-conditions (subst-bindings (intention-bindings intention)     ;Substitute bindings from intention into the conditions.
						 (intention-conditions intention))))

    ;Create a new problem from the goals in the goal-list
    (setq problem (make-problem-from-goal-literal-list instantiated-conditions))

    (print "Setting new problem")
    (pprint-problem problem)

    (setf (problem-i-parent problem) intention)

    (setq active-problem* problem)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Failure-context functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct failure-context 
  bindings     ;; bindings that caused the failure
  focus        ;; focus that caused the failure
  intention    ;; intention that caused the failure
  )

(defun create-and-store-failure-context (bindings &optional focus intention)
  (let ((fc-instance (make-failure-context :bindings bindings)))
    (when focus
      (setf (failure-context-focus fc-instance) focus)
      (if intention
	  (setf (failure-context-intention fc-instance) intention)))
    
    (push fc-instance (problem-failure-context-list active-problem*))))

;; return t or nil
(defun member-failure-context-list? (bindings &optional focus intention)
  (let ((fc-list (problem-failure-context-list active-problem*))
	(flag nil))

    (when fc-list
      (setq flag (loop for fc-instance in fc-list
		       thereis (match-against-failure-context-instance? bindings 
									focus 
									intention 
									fc-instance)))
      (if flag
	  (print "Found a matching failure-context."))
      flag)))

(defun match-against-failure-context-instance? (bindings focus intention fc-instance)
  (and (match-intentions intention (failure-context-intention fc-instance))
       (equal focus (failure-context-focus fc-instance))
       (consistent-bindings? bindings (failure-context-bindings fc-instance))))

(defun match-intentions (intention fc-intention)
  (cond ((and (null intention)
	      (null fc-intention))
	 t)
	((and intention
	      fc-intention
	      (eql (intention-id intention)
		   (intention-id fc-intention)))
	 (car (unify-match (subst-bindings (intention-bindings intention)
					   (intention-head intention))
			   (intention-head fc-intention))))
	(t
	 nil)))

(defun concept-member-of-failure-context-list? (bindings head conditions)
  (let ((fc-list (problem-failure-context-list active-problem*))
	(flag nil))

    ;; Only match against dummy intentions, i.e., intentions with nill ids
    (loop for fc-instance in fc-list
	  for fc-intention =  (failure-context-intention fc-instance)
	  when (and (null flag)
		    fc-intention
		    (null (intention-id fc-intention)))
	  do
	  (setq flag (and (car (unify-match head
					    (intention-head fc-intention)))
			  (car (unify-match conditions
					    (intention-conditions fc-intention)))
			  (consistent-bindings? bindings (failure-context-bindings fc-instance)))))

      (if flag
	  (print "Found a matching failure-context for a concept."))
      flag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structures and functions for constraints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;type can be BINDINGS-CONSTRAINT or GOAL-CONSTRAINT
(defstruct constraints
  name
  id
  type
  goal-conditions
  belief-conditions
  add
  delete
  bindings
)

(defmacro create-constraints (&rest constraints)
  `(let ((constraints (quote ,constraints))
	 result)
     (do ((next-constraint (car constraints) (car constraints)))
	 ((null constraints)
	  (setq constraint-memory* (append constraint-memory* (reverse result))) nil)
       (push (create-constraint-clause next-constraint) result)
       (pop constraints))))

(defun create-constraint-clause (constraint)
  (let ((name (car constraint))
	(id nil)
	(type nil)
	(goal-conditions nil)
	(belief-conditions nil)
	(add nil)
	(delete nil))
    
    (pop constraint)				; get rid of name, rest is alternating :key <spec> pairs
    (do ((next-field (car constraint) (car constraint))
	 (next-spec (cadr constraint) (cadr constraint)))
	((null constraint)
	 (cond ((null id) (setq id (incf id-count*))))
	 (make-constraints :name name
		       :id id
		       :type type
		       :goal-conditions goal-conditions
		       :belief-conditions belief-conditions
		       :add add
		       :delete delete))

      (cond ((eq next-field ':id)(setq id next-spec))
	    ((eq next-field ':type)(setq type next-spec))
	    ((eq next-field ':goal-conditions)(setq goal-conditions next-spec))
	    ((eq next-field ':belief-conditions)(setq belief-conditions next-spec))
	    ((eq next-field ':add)(setq add next-spec))
	    ((eq next-field ':delete)(setq delete next-spec))
	    (t (print-error next-field "field" "constraint")))
      (setq constraint (cddr constraint)))))

;; Check if a constraint is active.
(defun constraint-active? (constraint goal-list)
  (let ((constraint-matched? nil)
	(g-list (copy-list goal-list)))

    (when (eq (constraints-type constraint)
	      'GOAL-CONSTRAINT)
      ;; Match goal-conditions
      (loop with i = 1
	    with result = nil
	    with tliteral = nil
	    while (and (<= i (length g-list))
		       (not constraint-matched?))
	    do
	    ;; result is of the form (matched-literals bindings)
	    (setq result (pattern-match-literals (constraints-goal-conditions constraint)
						 g-list
						 t))
	    (cond ((eq (length (constraints-goal-conditions constraint))
		       (length (car result)))
		   (setq constraint-matched? t)
		   (setf (constraints-bindings constraint)
			 (second result)))
		  ;; shuffle the goal-list to generate a different permutations of literals
		  (t
		   (incf i)
		   (setq tliteral (pop g-list))
		   (setq g-list (append g-list (list tliteral))))))

      ;; Match belief-conditions
      (when (and constraint-matched?
		 (constraints-belief-conditions constraint))
	(setq constraint-matched?
	      (exists-match-bindings-for-literals? (constraints-belief-conditions constraint)
						   (constraints-bindings constraint)
						   cstm*))))
    constraint-matched?))

(defun violates-binding-constraints? (g-list)
  (let ((constraint-matched? nil))
    
    (loop for constraint in constraint-memory*
	  while (not constraint-matched?)
	  when (eq (constraints-type constraint)
		   'BINDINGS-CONSTRAINT)
	  do
	  
	  ;; Match goal-conditions
	  (loop with i = 1
		with result = nil
		with tliteral = nil
		while (and (<= i (length g-list))
			   (not constraint-matched?))
		do
		;; result is of the form (matched-literals bindings)
		(setq result (pattern-match-literals (constraints-goal-conditions constraint)
							    g-list
							    t))
		(cond ((eq (length (constraints-goal-conditions constraint))
			   (length (car result)))
		       (setq constraint-matched? t)
		       (setf (constraints-bindings constraint)
			     (second result)))
		      ;; shuffle the goal-list to generate a different permutations of literals
		      (t
		       (incf i)
		       (setq tliteral (pop g-list))
		       (setq g-list (append g-list (list tliteral))))))
	  
	  (when constraint-matched?
	    (setq constraint-matched? (exists-match-bindings-for-literals? (constraints-belief-conditions constraint) 
										(constraints-bindings constraint) 
										cstm*))))
    ;;(if constraint-matched?
	;;(print "Found a violated bindings-constraint. Rejecting the current bindings."))
    
    constraint-matched?))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set Up Correspondence of Goal-Targets and Targets
;;;    (and update bindings of intention based on match)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; assumes problem already has an intention
(defun establish-correspondence-for-intention-targets (problem)
  (let* ((intention (problem-intention problem))
	 (intention-bindings (intention-bindings intention))
	 (target-literals (subst-bindings intention-bindings
					  (intention-effects intention)))
	 (problem-bindings (problem-bindings problem))
	 (goal-literals (remove-satisfied-literals
			 (subst-bindings problem-bindings
					 (problem-objectives problem)))))

    (multiple-value-bind (gensymized-goal-literals gensym-bindings)
	(gensymize-vars goal-literals)
      
      (let* ((correspondence-match ; ( norvig-bindings matched-goals matched-targets)
	      (greedy-partial-unify-with-targets gensymized-goal-literals 
						 target-literals))
	     (correspondence-bindings (first correspondence-match))
	     (all-target-goal-literals ; undo gensymizing of goal-literals
	      (de-gensymize-vars (second correspondence-match)
				 gensym-bindings))
	     (all-target-literals (third correspondence-match))
	     (unification-bindings-for-all-target-literals
	      (unify all-target-literals
		     (norvig-subst-bindings correspondence-bindings
					    all-target-goal-literals)))
	     (icarus-bindings-form-for-targets
	      (convert-norvig-bindings-to-icarus
	       unification-bindings-for-all-target-literals))
	     (icarus-bindings-for-targets
	      (rest icarus-bindings-form-for-targets)))
	;; check whether anything failed (that shouldn't happen)
	;;  Check that all-target-goal-literals and all-target-literals
	;;     are non-null and of same length
	(if (and all-target-goal-literals
		 all-target-literals
		 (not (= (length all-target-goal-literals)
			 (length all-target-literals))))
	    (warn "~%Bad Correspondence-lists:~%   ~a~%   ~a"
		  all-target-goal-literals
		  all-target-literals))
	
	(if (and all-target-goal-literals
		 all-target-literals)
	    
	    ;; set problem-target-goals
	    (setf (problem-target-goals problem)
		  all-target-goal-literals)
	       
	    ;; set intention-bindings -- making sure that the problem-bindings are properly
	    ;; inherited as the intention-bindings
	    (setf (intention-bindings intention)
		  (append icarus-bindings-for-targets
			  intention-bindings))
	    
	    ;; set intention-targets
	    (setf (intention-targets intention)
		  (subst-bindings icarus-bindings-for-targets
				  all-target-literals)))))))

(defun remove-satisfied-literals (literal-list)
  (loop for literal in literal-list
	unless
	(goal-literal-satisfied? literal)
	collect literal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to generate list of unchainable conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-unchainable-conditions(cltm sltm)
  (let ((neg-hierarchical-concepts nil)
	(pos-chainables nil)
	(neg-chainables nil))
    
    ;; Consider all primitive concepts and their negations as possibly unchainable
    ;; Also create a list of negations of hierarchichal concepts.
    (loop with ccopy = nil
	  for concept in cltm
	  do
	  (cond ((null (concept-relations concept))

		 (when (not (member (car (concept-head concept)) 
				    pos-unchainable-conditions* :test #'equal))
		   (push (car (concept-head concept)) 
			 pos-unchainable-conditions*))

		 (when (not (member (car (concept-head concept)) 
				    neg-unchainable-conditions* :test #'equal))
		   (push (car (concept-head concept)) 
			 neg-unchainable-conditions*))

		 (setq cltm
		       (remove concept cltm :test #'equal)))

		(t
		 (setq ccopy (copy-concept concept))
		 (setf (concept-head ccopy)
		       (list 'not (concept-head ccopy)))
		 (push ccopy neg-hierarchical-concepts))))
    
    ;; All concepts that are achieved by a skill are chainable.
    ;; Remove them from the lists (pos-unchainable-conditions*, neg-unchainable-conditions*, cltm,
    ;; neg-hierarchical-concepts), if present.
    (loop for skill in sltm
	  do
	  (loop for effect in (sclause-effects skill)
		do
		(cond ((not (eq (car effect) 'not))
		       (when (not (member (car effect) 
					  pos-chainables :test #'equal))
			 (push (car effect) pos-chainables))
		       (setq pos-unchainable-conditions*
			     (remove (car effect) pos-unchainable-conditions* :test #'equal))
		       (setq cltm
			     (remove (car effect) cltm :test #'(lambda (literal concept)
								 (eq literal (car (concept-head concept)))))))
		      (t
		       (when (not (member (caadr effect) 
					  neg-chainables :test #'equal))
			 (push (caadr effect) neg-chainables))
		       (setq neg-unchainable-conditions*
			     (remove (caadr effect) neg-unchainable-conditions* :test #'equal))
		       (setq neg-hierarchical-concepts (remove (caadr effect) neg-hierarchical-concepts
							       :test #'(lambda (literal concept)
									 (eq literal (caadr (concept-head concept))))))))))

    ;; Loop through remaining hierarchical concepts,
    ;; and determine if they are unchainable. This 
    ;; process is recursive.
    (loop for concept in cltm
	  do
	  (when (and (all-relations-unchainable (concept-relations concept) pos-chainables neg-chainables)
		     (not (member (car (concept-head concept)) 
				    pos-unchainable-conditions* :test #'equal)))
	    (push (car (concept-head concept)) 
		  pos-unchainable-conditions*)))

    ;; Loop through remaining negations of hierarchical concepts,
    ;; and determine if they are unchainable. This 
    ;; process is recursive.
    (loop for concept in neg-hierarchical-concepts
	  do
	  (when (and (all-relations-unchainable (concept-relations concept) pos-chainables neg-chainables)
		     (not (member (caadr (concept-head concept)) 
				    neg-unchainable-conditions* :test #'equal)))
	    (push (caadr (concept-head concept)) 
		  neg-unchainable-conditions*)))))

(defun all-relations-unchainable(relations pos-chainables neg-chainables)
  (loop for relation in relations
	always (is-unchainable relation pos-chainables neg-chainables)))

(defun is-unchainable(relation pos-chainables neg-chainables)
  (let ((concept-triplet (find-matching-concept relation
						:problem-bindings nil
						:ignore-failure-context? t)))
    (cond 
      ;;positive concept
      ((not (eq (car relation) 'not))
       (cond ((member (car relation)
		      pos-unchainable-conditions*
		      :test #'equal)
	      t)
	     ((member (car relation)
		      pos-chainables
		      :test #'equal)
	      ;;Chainable concept
	      nil)
	     (t
	      (all-relations-unchainable (second concept-triplet) 
					 pos-chainables neg-chainables))))
      
      ;;negated-concept
      (t
       (cond ((member (caadr relation)
		      neg-unchainable-conditions*
		      :test #'equal)
	      t)
	     ((member (caadr relation)
		      neg-chainables
		      :test #'equal)
	      ;;Chainable concept
	      nil)
	     (t
	      (all-relations-unchainable (negate-relations 
					  (second concept-triplet))
					 pos-chainables neg-chainables)))))))

(defun negate-relations(relations)
  (let ((negated-relations nil))
    (loop for relation in relations
	  do
	  (cond ((eq (car relation) 'not)
		 (push (cadr relation) negated-relations))
		(t
		 (push (list 'not relation) negated-relations))))
    negated-relations))

(defun get-chainable-negations(relations)
  (let ((bindings nil))
    
    (setq bindings (find-all-match-bindings-for-literals relations nil cstm*))
    (if bindings
	(setq bindings (random-choose bindings)))
    
    (mapcar #'(lambda (relation)
		(setq relations (remove relation relations :test #'equal)))
	    (get-unchainable-relations relations))
    
    (list (negate-relations relations)
	  bindings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Using unchainable relations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First idea:
;;  Use in skill-selection to make sure all candidate skills 
;;    have all unchainable conditions satisfied

(defun unchainable? (relation)
  (cond ((eql (car relation) 'not)
	 (member (car (second relation))
		 neg-unchainable-conditions*))
	(t
	 (member (car relation)
		 pos-unchainable-conditions*))))

(defun get-unchainable-relations (relation-list)
  (loop for relation in relation-list
       when (unchainable? relation)
       collect relation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code to detect execution loops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter execution-loop* nil)

(defun get-current-state (cstm)
  (mapcar #'cinstance-head cstm))

(defun store-intention (intention)
  (if gstm*
      (push (intention-head intention)
	    (problem-repeating-intentions (car gstm*)))))

(defun get-repeating-intentions ()
  (problem-repeating-intentions (car gstm*)))

;;Stores a state with probability P. Presently P = 1/3.
(defun store-state (state)
  (when (and gstm*
	     (= (random 3)
		(random 3)))
    (push state (problem-visited-states
		 (car gstm*)))
    (print "Stored current belief state.")))

(defun repeated-state? (state)
  (let ((visited-states (if gstm* 
			    (problem-visited-states 
			     (car gstm*))))
	(repeated? nil))

    (loop with matched-literals=nil
	  for visited-state in visited-states
	  while (not repeated?)
	  do
	  (setq matched-literals (pattern-match-literals state visited-state))
	  (if (eq (length matched-literals)
		  (length visited-state))
	      (setq repeated? t)))
    repeated?))

(defun reset-loop-traces ()
  (setq execution-loop* nil)
   (setf (problem-repeating-intentions (car gstm*)) nil)
   (setf (problem-visited-states (car gstm*)) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User interface functions (descended from compiler.lisp &
;;; interpreter.lisp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This function takes problem-forms -- a list of goal literals,
;; creates a list of problems from it and assigns the list to 
;; gstm*.
(defmacro create-problems (&rest problem-forms)
  `(setf gstm* 
    (loop for form in (quote ,problem-forms)
     collect 
     (make-problem-from-goal-literal-list form))))

(defun pprob () (print-problems))

(defun print-problems ()
  (cond ((null gstm*)
	 (terpri) (princ "No problems are specified."))
	(t
	 (terpri) (princ "Top level problems")
	 (mapcar #'pprint-problem gstm*))))

(defun print-skills ()
  (terpri)(princ "Skills:")(terpri)
  (mapc #'(lambda (c) (print-clause c)(terpri)) sltm*) 
  nil)

(defun ps () (print-skills))

(defun print-clause (clause)
  (terpri)(princ "(")(princ (sclause-head clause))
  (princ "  :id ")(princ (sclause-id clause))
  (cond ((not (null (sclause-percepts clause)))
	 (terpri)(princ " :percepts ")(princ (sclause-percepts clause))))
  (cond ((not (null (sclause-conditions clause)))
	 (terpri)(princ " :conditions    ")(princ (sclause-conditions clause))))

  (cond ((not (null (sclause-subskills clause)))
	 (terpri)(princ " :subskills ")(princ (sclause-subskills clause))))
  (cond ((not (null (sclause-action clause)))
	 (terpri)(princ " :action  ")(princ (sclause-action clause))))
  (cond ((not (null (sclause-effects clause)))
	 (terpri)(princ " :effects  ")(princ (sclause-effects clause))))
  (princ ")"))

(defun display-active-problem-stack ()
  (cond ((null active-problem*)
	 (format t "~%No Active Problem"))
	(t
	 (loop for problem = active-problem* then (intention-problem-parent intent)
	    for intent = (problem-i-parent problem)
	    while intent
	    do
	      (pprint-problem problem)
	      (pprint-intention intent)
	    finally
	      (pprint-problem problem)))))
	      
(defun change-skill-selection-heuristic()
  (let ((heuristic-list (list 'MAXIMUM-EFFECTS-MATCHED
			      'MINIMUM-UNSATISFIED-CONDITIONS)))

    ;; Print the available options
    (format t "~%~%*********************************")
    (format t "~%Here are the available heuristics:")
    (loop for i from 1
	  for heuristic in heuristic-list
	  do
	  (format t "~%   ~a  ~a" i heuristic))

    ;; Prompt user and read selection
    (format t "~%Enter the number of the heuristic you want to select :")
    (let ((choice (read)))
      (cond ((and (numberp choice)
		  (> choice 0)
		  (<= choice (length heuristic-list)))
	     (cond ((= choice 1)
		    (setq skill-selection-heuristic* :MAX-EFFECTS-MATCHED)
		    (format t "~%Skill selection heuristic set to Maximum Effects Matched."))
		   ((= choice 2)
		    (setq skill-selection-heuristic* :MIN-UNSATISFIED-CONDITIONS)
		    (format t "~%Skill selection heuristic set to Minimum Unsatisfied Conditions."))))
	    (t
	     (format t "~%~%INVALID CHOICE! TRY AGAIN."))))))

(defun set-search-direction()
  (let ((options (list 'BACKWARD
		       'FORWARD)))

    ;; Print the available options
    (format t "~%~%*********************************")
    (format t "~%Here are the available choices:")
    (loop for i from 1
	  for option in options
	  do
	  (format t "~%   ~a  ~a" i option))

    ;; Prompt user and read selection
    (format t "~%Enter your choice :")
    (let ((choice (read)))
      (cond ((and (numberp choice)
		  (> choice 0)
		  (<= choice (length options)))
	     (cond ((= choice 1)
		    (setq search-direction* :BACKWARD)
		    (setq skill-selection-heuristic* :MAX-EFFECTS-MATCHED)
		    (format t "~%Search direction set to backward."))
		   ((= choice 2)
		    (setq search-direction* :FORWARD)
		    (setq skill-selection-heuristic* :MIN-UNSATISFIED-CONDITIONS)
		    (format t "~%Search direction set to forward."))))
	    (t
	     (format t "~%~%INVALID CHOICE! TRY AGAIN."))))))

(defun clear+ ()
  (clear-concepts)                      ; cltm*
  (if (equal inference* 'fast) (clear-fast-matcher))
  (clear-skills)                        ; sltm*
  (clear-goals)                         ; gstm*
  (setq id-count* 0)
  ;; additional clearings
  (setf concepts* nil)
  (setf primitive-concepts* nil)
  (setf pstm* nil)
  (setf cstm* nil)
  (setf cycle* nil)

  (setf cycle* 0)
  (setf gtrace* t)			; goal print
  (setf ptrace* 1) ; 1 for organized print, and 2 for raw print of percepts
  (setf btrace* t)			; belief print
  (setf ctrace* t)			; cycle print
  (setf atrace* t)			; action print
  (setf etrace* t)			;
  (setf debug* nil)

  (setf halt* nil)
  (setf failed* nil)
  (setf inference* 'FAST)
  ;; are these necessary?
  (setf starttime* nil)
  (setf astm* nil)

  ;; globals from solver-rewrite
  (setf active-problem* nil)
  (setf executing-intention* nil)
  (setf constraint-memory* nil
	pos-unchainable-conditions* nil
	neg-unchainable-conditions* nil)

  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User interface functions for execution traces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Utility function
(defun exec-trace (&optional enable-problem-solving?)
  (setf pat-trace* t
	problem-solver-enabled* enable-problem-solving?
	*print-right-margin* 78))

(defmacro execute-skill (skill-form)
  `(progn (exec-trace)  ; execution-only, this turns off problem-solving
	  (run 1)
	  (execute-skill+  ,skill-form)))

(defmacro execute-skill+ (skill-form)
  `(progn (exec-trace)
	  (cond ((call-intention (quote ,skill-form))
		 (loop while executing-intention*
		    do
		      (cont 1))
		 (format t "~%~%Execution stopped because Executing Intention is NIL~%"))
		(t (format t "~%~% Call to skill-form FAILED: ~a~%"
			   (quote ,skill-form))))))