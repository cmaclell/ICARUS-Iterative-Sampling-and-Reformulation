;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PATTERN MATCHING FACILITY
;;;;  (adapted from Norvig auxfns.lisp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconstant fail nil)
(defconstant no-bindings '((t . t)))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun norvig-lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
            nil
            bindings)))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Unifier
;;;;   (adapted from Norvig unify.lisp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *occurs-check* t "Should we do the occurs check?")

;; returns NIL if unification failed, otherwise returns norvig-bindings
;;   Note that "empty bindings" result is Norvig's no-bindings, ie (T . T)
;;   The bindings list returned is an extension of the input bindings
(defun unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail) fail)
        ((eql x y) bindings)
        ((variable-p x) (unify-variable x y bindings))
        ((variable-p y) (unify-variable y x bindings))
        ((and (consp x) (consp y))
         (unify (rest x) (rest y) 
                (unify (first x) (first y) bindings)))
        (t fail)))

(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
         (unify (norvig-lookup var bindings) x bindings))
        ((and (variable-p x) (get-binding x bindings))
         (unify var (norvig-lookup x bindings) bindings))
        ((and *occurs-check* (occurs-check var x bindings))
         fail)
        (t (extend-bindings var x bindings))))

(defun occurs-check (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((and (variable-p x) (get-binding x bindings))
         (occurs-check var (norvig-lookup x bindings) bindings))
        ((consp x) (or (occurs-check var (first x) bindings)
                       (occurs-check var (rest x) bindings)))
        (t nil)))

;;; ==============================

(defun norvig-subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) x)
        ((and (variable-p x) (get-binding x bindings))
         (norvig-subst-bindings bindings (norvig-lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (norvig-subst-bindings bindings (car x))
                       (norvig-subst-bindings bindings (cdr x))
                       x))))

;;; ==============================

(defun unifier (x y)
 "Return something that unifies with both x and y (or fail)."
 (norvig-subst-bindings (unify x y) x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; replaces the variables of x with gensymed vars before calling unifier
;;    unified result (I think) uses variable names from y
(defun safe-unifier (x y)
  (unifier (gensymize-vars x) y))

;; gets bindings of variables from y that unify x and y
(defun safe-unify (x y)
  (unify y (safe-unifier x y)))

;; this function returns two values:
;;     1. a form with gensym vars substituted for original vars
;;     2. norvig-bindings for associating gensym vars with original vars
;;        which has elements of the form (original-var . gensym-var)
(defun gensymize-vars (form)
  (let ((bindings (or (loop for var in (collect-variables form)
				collect
				(cons var (gentemp "?"))) ; maybe should reuse these vars?
			     no-bindings)))
    (values (norvig-subst-bindings bindings
				   form)
	    bindings)))	     ; to use later to undo if needed

(defun collect-variables (x &optional variables-so-far)
  (cond ((variable-p x)
	 (if (member x variables-so-far)
	     variables-so-far
	     (cons x variables-so-far)))
	((atom x) variables-so-far)
	(t
	 (collect-variables (first x)
			    (collect-variables (rest x) variables-so-far)))))

;; this switches the binding order (assumes vars bound only to variables)
;;     as in the gensym-var substitution
(defun reverse-bindings (gensym-bindings)
  (loop for (var1 . var2) in gensym-bindings
       collect (cons var2 var1)))

(defun de-gensymize-vars (form gensym-bindings)
  (norvig-subst-bindings (reverse-bindings gensym-bindings)
			 form))

;; This tries to unify, and returns a (flag . bindings) Icarus-style result
(defun unify-match (x y &optional (safe-unify? t))
  (let ((norvig-bindings
	 (if safe-unify?
	     (safe-unify x y)
	     (unify x y))))
    (cond ((eql norvig-bindings no-bindings)
	   (list t))			; flag is T, bindings = NIL
	  (norvig-bindings
	   (cons t norvig-bindings))   	; flag is T, with bindings
	  (t (list nil)))))		; flag is NIL

;; Example
;;        (unify-match '(?x ?y ?x) '(foo ?x ?y))
;;     -->  (T (?Y . FOO))
;;         The bindings are for the second form, and if substituted
;;             would yield:  (FOO ?X FOO)
;;           NOTE: the first form requires the first and third elements to be the same
;;                 ?x is a free variable in the unified form

;; returns a ( flag . bindings ) form in Icarus style
;;    also needs to remove any unified vars that don't have concrete bindings
(defun convert-norvig-bindings-to-icarus (norvig-bindings)
  (cond ((eql norvig-bindings no-bindings)
	 (list t))			; flag is T, bindings = NIL
	(norvig-bindings
	 (cons t (remove-unbound-variable-values norvig-bindings))) ; flag is T, with bindings
	(t (list nil))))                ; flag is NIL (norvig-bindings NIL means "failed")

(defun remove-unbound-variable-values (norvig-bindings)
  (if (no-variable-values? norvig-bindings)
      norvig-bindings
      (loop for binding in norvig-bindings
	 for (var . val) = binding
	 for final-binding = (if (variablep val)
				 (get-final-binding val norvig-bindings)
				 binding)
	 when final-binding
	 collect final-binding)))

(defun no-variable-values? (norvig-bindings)
  (loop for (var . val) in norvig-bindings
       never (variablep val)))

;; recursively looks up var  until val is non-variable or is a varaible without a binding
;;   returns binding pair if succeeds, or nil if no non-variable binding found
(defun get-final-binding (var norvig-bindings)
  (loop with binding = (get-binding var norvig-bindings)
       while (and binding
		  (variablep (cdr binding)))
       do
       (setf binding (get-binding (cdr binding) norvig-bindings))
       finally
       (return binding)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Greedy Partial Matching
;;;   find maximal match by random greedy process
;;;     Match type 1: goal-literals against target-ground-literals
;;;     Match type 2: goal-literals against target-goal-literals (with distinct vars)
;;;   Use Unification for BOTH TYPES,
;;;     But for Type 1 matches - must treat negated literals specially
;;;     Type 2 matches can be wrapped by a function to gensymize vars of targets,
;;;         then undo gensym-var substitution to recover original targets
;;;   Results of Type 1 matches will be ( bindings matched-goal-literals )
;;;   Results of Type 2 matches will be a triple:
;;;       ( bindings matched-goal-literals matched-target-literals)
;;;         Where the 2nd and 3rd elements have same length,
;;;            and matched terms appear in corresponding positions of these 2 lists)
;;;   DON'T IMPLEMENT TYPE 1 MATCHES
;;;      Instead, use find-all-max-size-partial-matches-for-literals
;;;          or the version for sorted-literals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TYPE 2 MATCH
;;   This match uses UNIFY as the matcher for single-literals
;;   Returns a 3-element list:  ( norvig-bindings matched-goal-list matched-target-list)
;;   NOTE: the norvig-bindings are for substituting in matched-goal-lits
(defun greedy-partial-unify-with-targets (goal-literals
					  target-literals
					  &optional
					  (bindings no-bindings))
  (loop
     with norvig-bindings = bindings
     with matched-literals = nil
     with matched-targets = nil
     with remaining-literals = goal-literals
     with remaining-targets = target-literals
     with random-literal = nil
     with random-match = nil
     with matched-target = nil
     while (and remaining-literals
		remaining-targets)
     do
     (setf random-literal
	   (random-choose remaining-literals))
     (setf random-match	; returns (norvig-bindings matched-target) or NIL if no match
	   (random-unify-with-target random-literal
				     remaining-targets
				     norvig-bindings))
     (when random-match
       (setf norvig-bindings (first random-match))
       (setf matched-target (second random-match))
       (push random-literal matched-literals)
       (push matched-target matched-targets)
       (setf remaining-targets (remove matched-target remaining-targets)))
     (setf remaining-literals
	   (remove random-literal remaining-literals))
     finally
       (return (list norvig-bindings matched-literals matched-targets))))

;; returns (norvig-bindings matched-target) or NIL if no match
(defun random-unify-with-target (goal-literal
				 target-literals
				 &optional
				 (norvig-bindings no-bindings))
  (loop
     with remaining-targets = target-literals
     with random-target = nil
     with match-result = nil		; bindings or NIL for failure
     while (and remaining-targets
		(not match-result))
     do
       (setf random-target (random-choose remaining-targets))
       (setf match-result (unify goal-literal random-target norvig-bindings))
       (unless match-result
	 (setf remaining-targets (remove random-target remaining-targets)))
     finally
       (return 
	 (when match-result
	   (list match-result random-target)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Wrappers for Greedy Unification Partial Matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Repeat Greedy Match (pick "best" = largest match)

;;; repeats try-count time, and returns NIL or ( bindings matches-1 matches-2)
(defun repeat-greedy-partial-unify-with-targets (goal-literals
						 target-literals
						 &key
						 (try-count 10)
						 (norvig-bindings no-bindings))
  (loop repeat try-count
       with match-result = nil		; NIL also signals failure
       with best-match = nil		; NIL also signals failure
       do
       (setf match-result
	     (greedy-partial-unify-with-targets goal-literals
						target-literals
						norvig-bindings))
       (when (and match-result
		  (or (null best-match)
		      (> (length (second match-result))
			 (length (second best-match)))))
	 (setf best-match match-result))
       finally
       (return best-match)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple Exact Matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This matches the forms, ensuring that vars of form-1 bind consistently to values in from-2
;;    Vars in Form-2 are treated same as constants, 
;;    but the "exactness" condition requires that variables only bind with variables,
;;       and constants match constants

;; Use this for focus comparison
(defun simple-exact-binding-match (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail) fail)
        ((variable-p x) (simple-exact-unify-variable x y bindings))
        ((eql x y) bindings)
        ((and (consp x) (consp y))
         (simple-exact-binding-match (rest x) (rest y)
			       (simple-exact-binding-match (first x) (first y) bindings)))
        (t fail)))

(defun simple-exact-unify-variable (var x bindings)
  (cond ((not (variable-p x)) fail)
	((get-binding var bindings)
         (if (equal (norvig-lookup var bindings) x)
	     bindings
	     fail))
        (t (extend-bindings var x bindings))))

;; Use this for repeated-problem detection
;; checks if the list literals-1 is a subset of the list literals-2.
(defun exists-subset-exact-match-with-permuted-literals (literals-1 literals-2 &optional (bindings no-bindings))
  (cond ((eq bindings fail) fail)
	((null literals-1)
	 bindings)
	(t
	 (loop with lit-1 = (first literals-1)
	       for lit-2 in literals-2
	       thereis
	       (let ((bindings-1-2 (simple-exact-binding-match lit-1 lit-2 bindings)))
		 (and bindings-1-2
		      (exists-subset-exact-match-with-permuted-literals (cdr literals-1)
									(remove lit-2
										literals-2
										:test #'equal)
									bindings-1-2)))))))

;; This function checks if fc-bindings are a subset of goal-bindings. If so it returns true. The
;; assumption here is that if a smaller set of bindings has failed earlier then a larger (or equal)
;; set of bindings will fail. This assumption might be wrong and should be verified with test results.
(defun consistent-bindings? (goal-bindings fc-bindings)
  (cond ((and (null goal-bindings)
	      (null fc-bindings))
	 ;; both bindings are nil, so return true
	 t)
	((null fc-bindings)
	 ;; A failure-context with empty bindings means that no possible set of bindings
	 ;; can succeed for this problem since "empty" bindings has failed.
	 nil)
	(t
	 ;; check if all variables bound in fc-binding are bound to the same value
	 ;; in goal-binding. if so then "fc-binding is a subset of goal-binding".
	 (loop for (var . val) in fc-bindings
	       for binding-pair = (get-binding var goal-bindings)
	       always (and binding-pair
			   (eql val (cdr binding-pair)))))))