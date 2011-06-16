;;;;; structures.lisp

;;; contains the data structures for handling beliefs, rules,
;;; instantiated rules, working memory, the knowledge base, and belief
;;; support. contains functions for working with these data
;;; structures.

;;;; SKOLEM CONSTANTS
;;
;; Skolems are unique identifiers for objects not yet attached to 
;; an item, event, relationship, etc. from the world. They act as
;; placeholders in justifications and may be replaced by domain
;; constants when possible.

;; Generate the unique identifier for a Skolem.
(let ((skid 0))
  (defun reset-skolem-id () (setf skid 0))
  (defun get-skolem-id () (symb "SK" (incf skid))))

;; input:
;;   sk - a Skolem
;;   str - a format stream
;;   d - depth
;;
;; Prints the name of the Skolem to the stream.
(defun print-skolem (sk str d)
  (declare (ignore d))
  (format str "~A" (sk-id sk)))

;; Defines a Skolem constant as a specific type.
(defstruct (skolem (:conc-name sk-)
		   (:print-function print-skolem)
		   (:predicate skolem?))
  (id (get-skolem-id) :type symbol :read-only t))

;; input:
;;   sk1, sk2 - Skolems
;;
;; output:
;;   t - when two skolems have the same name.
(defun skolem-equal? (sk1 sk2)
  (and (skolem? sk1) (skolem? sk2)
       (eq (sk-id sk1) (sk-id sk2))))
  

;;;; VARIABLES
;;
;; Variables only appear in the rules that compose the system's
;; background knowledge. They are represented as symbols that begin
;; with a '?'. Currently all variables are universally quantified.

;; input:
;;   x - a symbol
;;   
;; output:
;;   t - when x is a variable (is a symbol that begins with '?')
(defun variable? (x)
  (and (symbolp x)
       (char= (char (symbol-name x) 0) #\?)))

;;;; RULES
;;
;; Rules encode the system's knowledge. They are represented as
;; if--then relationships where the antecedent is a conjunction of
;; positive or negative literals and the consequent takes the same
;; form. Example:
;;    (=> (AND (p1 ?x ?y) (p2 ?y ?z)) (AND (p3 ?x) (p3 ?z)))

;; input:
;;   rl - a rule
;;
;; output:
;;   the antecedent of rl
(defun rule-antecedent (rl)
  (second rl))

;; input:
;;   rl - a rule
;;
;; output:
;;   the consequent of rl
(defun rule-consequent (rl)
  (third rl))

(defun rule-constraints (r1)
  (fourth r1))

;; unused:
;; 
;; consider revising for a top-level representation
(defun make-rule (head body &optional (constraints nil))
  (list '=> 
	body head 
	constraints))

;;;; LITERALS

;; takes a literal and reports whether it is encased in a modal belief
;; operator. this could possibly be more clever.
;;
;; belief modals in rules or justifications will look like
;; (belief ?agent <literal>)
;; this is shorthand for (belief you (belief ?agent <literal>))
;; just like (restaurant-name ?d ?r) is shorthand for 
;; (belief you (restaurant-name ?d ?r))
;;
;; the literal in a belief modal may unpack to be another belief.  the
;; agent in a belief modal may be a variable, which means that the
;; system should search through all the agent worlds that it believes.
;; to represent (belief you (belief ?agent (belief you <literal>)))
;; the rule will use the constant "you" to identify the agent whose
;; world it fires in. this feature is necessary to support perspective
;; shift. otherwise all rules are interpreted as if the system were
;; reasoning from a single, privileged perspective.
(defun belief-modal? (lit)
  (eq (first lit) 'belief))

(defun goal-modal? (lit)
  (eq (first lit) 'goal))

(defun intention-modal? (lit)
  (eq (first lit) 'intention))

(defun modal-lit? (x)
  (or (belief-modal? x)
      (goal-modal? x)
      (intention-modal? x)))

(defun remove-goal-modal (lit)
  (second lit))

(defun remove-intention-modal (lit)
  (second lit))

;; returns t when given a negative literal
;; negated literals will look like (not <literal>)
(defun negated? (lit)
  (eq (first lit) 'not))

;; true when x is a literal (positive or negative)
(defun literal? (x)
  (cond ((negated? x)
	 (literal? (second x)))
	((modal-lit? x)
	 (literal? (third x)))
	(t
	 (not (find-if #'consp (cdr x))))))

;; return a negated version of the literal.
(defun negate (lit)
  (list 'not lit))

;; returns the literal with the opposite sign.
(defun flip-negation (lit)
  (if (eq 'not (first lit))
      (second lit)
      (negate lit)))
	  
;; returns the predicate in a negative or positive literal
(defun predicate-name (lit)
  (cond ((negated? lit)
	 (predicate-name (second lit)))
	((modal-lit? lit)
	 ;; XXX: change this once we add agents to modal operators.
	 (predicate-name (second lit)))
	(t
	 (first lit))))

;; returns t if the literal is fully bound
(defun literal-grounded? (lit)
  (not (find-if #'(lambda (x) (or (skolem? x) (variable? x))) (literal-args lit))))

;; returns the arguments in a literal
(defun literal-args (lit)
  (cond ((negated? lit)
	 (literal-args (second lit)))
	((modal-lit? lit)
	 ;; XXX: fix this once modals have an agent variable
	 (literal-args (second lit)))
	(t 
	 (rest lit))))

;; true if a1 and a2 are the same constant or are both skolems.
(defun equivalent-args? (a1 a2)
  (or (eql a1 a2)
      (and (skolem? a1) (skolem? a2))))

;; literals are equal if
;; (a) they have the same sign
;; (b) they have the same predicate
;; (c) they have the same domain constants in the same positions
;; (d) they have the same modal operators
(defun literal-equal? (lit1 lit2 &key (test #'equivalent-args?))
  (and (literal? lit1) (literal? lit2)
       (match-modals? lit1 lit2)
       (if (negated? lit1) (negated? lit2) (not (negated? lit2)))
       (eql (predicate-name lit1) (predicate-name lit2))
       (= (length (literal-args lit1)) (length (literal-args lit2)))
       (not (mismatch (literal-args lit1) (literal-args lit2)
		      :test test))))

;; literals contradict each other if 
;; (a) they have the same predicate
;; (b) they have the same domain constants in the same positions
;; (c) they have opposite signs
;; (d) they have the same modal operators
(defun literal-contradiction? (lit1 lit2 &key (test #'equivalent-args?))
  (and (literal? lit1) (literal? lit2)
       (match-modals? lit1 lit2)
       (if (negated? lit1) (not (negated? lit2)) (negated? lit2))
       (eql (predicate-name lit1) (predicate-name lit2))
       (= (length (literal-args lit1)) (length (literal-args lit2)))
       (not (mismatch (literal-args lit1) (literal-args lit2)
		      :test test))))

;;; match-predicate?
;;;
;;; input: pred - a predicate in icarus format
;;;        lit  - a literal (a positive or negative fact)
;;;
;;; output: t   - if lit is an instance of pred
;;;         nil - otherwise
;;;
;;; example: (match-predicate? '(foo x y) '(not (foo BAR EXAM))) ==> t
;;;          (match-predicate? '(foo x y) '(foo BAR)) ==> nil
;;;
;;; warning: no sanity checks on input
(defun match-predicate? (pred lit)
  (and (eq (predicate-name lit) (predicate-name pred))
       (= (length (literal-args lit)) (length (literal-args pred)))))

;;; strip-modals
;;;
;;; input: lit - a literal
;;; 
;;; output: the same literal with any modals stripped from it
(defun strip-modals (lit)
  (if (modal-lit? lit)
      ;; XXX: must be changed once modals can take more than one argument
      (strip-modals (second lit))
      lit))

;;; match-modals?
;;;
;;; input: lit1 - a literal
;;;        lit2 - a literal
;;;
;;; output: t   - if the modal operators in lit1 and lit2 are identical
(defun match-modals? (lit1 lit2)
  ;; this will be more involved once we incorporate arguments for modals.
  (equal (list-modals lit1) (list-modals lit2)))

;;; list-modals
;;;
;;; input: lit - a (modal) literal
;;;
;;; output: a list of modal operators modifying the literal
;;;
;;; example: (list-modals '(belief (goal (belief (intention (foo b c))))))
;;;          => (belief goal belief intention)
(defun list-modals (lit)
  (when (modal-lit? lit)
      (cons (first lit) (list-modals (second lit)))))

;;; prepend-modals
;;;
;;; input: mds - an ordered list of modals
;;;        lit - a literal
;;; 
;;; output: lit with the modals in mds prepended to it.
;;;
;;; example: (prepend-modals '(belief goal belief) '(foo b c))
;;;          => (belief (goal (belief (foo b c))))
(defun prepend-modals (mds lit)
  (cond ((null mds)
	 lit)
	(t 
	 (list (first mds) (prepend-modals (rest mds) lit)))))


;;;; BELIEFS
;;
;; Beliefs are the elements of working memory. They are unique and
;; they have logical content. Beliefs are not to be confused with
;; literals modified by a belief modal. In this version of the code,
;; goals, intentions, observations, and beliefs, are all stored using
;; the belief structure. If I ever refactor the code, I plan to rename
;; the structure something more generic like "element".
;; 
;; historical note: Originally, I had planned to distinguish goals,
;; intentions, beliefs, etc. by having different types of worlds that
;; would house different types of elements. My latest thoughts are
;; that all the elements in all the worlds are identical and the
;; modality of the elements are determined by the relationships that
;; link worlds to one another.

;;; support the creation of unique belief identifiers
(let ((bid 0)) 
  (defun reset-belief-id () (setf bid 0))
  (defun make-belief-id () 
    (symb "B" (incf bid))))

(defun print-belief (b str d)
  (declare (ignore d))
  (format str "(~A ~A)" (belief-id b) (belief-content b) ))

(defstruct (belief (:conc-name belief-)
		   (:print-function print-belief)
		   (:predicate belief?))
  (id (make-belief-id) :type symbol :read-only t)
  (content nil)
  (fact nil)
  (weak-links nil) ;; justifications used by other agents that rely on this belief
                   ;; ((wrld jst) (wrld jst) ...)
  (start nil)
  (end nil))

;; tests if the content of the belief is negated.  this only considers
;; the outermost context of the belief and does not drill down into
;; modals.
(defun belief-negated? (b)
  (negated? (belief-content b)))

;; tests if two beliefs contradict each other.
(defun belief-contradiction? (b1 b2)
  (literal-contradiction? (belief-content b1) (belief-content b2)))


;;;; JUSTIFICATIONS

(let ((jid 0)) 
  (defun reset-justification-id () (setf jid 0))
  (defun make-justification-id () 
    (symb "J" (incf jid))))


(defstruct (justification (:conc-name justification-)
		   (:print-function print-justification)
		   (:predicate justification?))
  (id (make-justification-id) :type symbol :read-only t)
  (content nil))

(defun print-justification  (j str d)
  (declare (ignore d))
  (if (justification-constraints j)
      (format str "(~A: ~A => ~A) {~A}" (justification-id j) 
	      (justification-antecedent j) 
	      (justification-consequent j)
	      (justification-constraints j))
      (format str "(~A: ~A => ~A)" (justification-id j) 
	      (justification-antecedent j) 
	      (justification-consequent j))))

(defun justification-antecedent (j)
  (second (justification-content j)))

(defun justification-consequent (j)
  (third (justification-content j)))

(defun justification-constraints (j)
  (fourth (justification-content j)))


(defun skolemize (rl &optional (ht (make-hash-table :test #'eq)))
  (cond ((variable? rl)
	 (if (gethash rl ht) 
	     (gethash rl ht)
	     (setf (gethash rl ht) (make-skolem))))
	((atom rl)
	 rl)
	(t 
	 (cons (skolemize (car rl) ht)
	       (skolemize (cdr rl) ht)))))

;; bind variables to skolem constants
(defun instantiate-rule (r)
  (make-justification :content (skolemize r (make-hash-table :test #'eq))))

;; returns all the literals in the antecedent or consequent of a rule
(defun collect-literals (&rest args)
  (copy-list 
   (mappend #'(lambda (x) 
		(if (eq 'and (first x))
		    (rest x)
		    (list x)))
	    args)))

;; returns all the literals in an (instantiated) rule
(defun justification-literals (j)
  (collect-literals (justification-consequent j)
		    (justification-antecedent j)))

(defun rule-literals (r)
  (collect-literals (rule-consequent r)
		    (rule-antecedent r)))

(defun contradictory-justification (j)
  ;; when detecting contradictions within a justification, it's important to
  ;; match skolems. otherwise, (p sk1) and (not (p sk2)) would inaccurately
  ;; trigger a contradiction.
  (detect-contradiction (justification-literals j)))

(defun detect-contradiction (lts)
  (unless (null lts)
    (or (member-if #'(lambda (x) (literal-contradiction? (first lts) x :test #'eql))
		   (rest lts))
	(detect-contradiction (rest lts)))))

(defun fully-grounded? (j)
  (null (find-if-not #'literal-grounded? (justification-literals j))))

;;; list-predicates
;;;
;;; input: a rule
;;; output: a list of the predicates in that rule.
;;;  predicates are consed with not if they're negated otherwise, they're
;;;  consed with nil.
(defun list-predicates (r)
  (cond ((or (eq '=> (car r)) (eq 'and (car r)))
	 (mappend #'list-predicates (cdr r)))
	((negated? r)
	 (list (cons (predicate-name r) '(not))))
	(t
	 (list (cons (predicate-name r) '())))))


;;; input: a rule
;;; output: a list of the names of the predicates in that rule
(defun predicate-names (r)
  (if (or (eq '=> (car r)) (eq 'and (car r)))
      (mappend #'list-predicates (cdr r))
      (list (predicate-name r))))

;; takes a predicate and a justification or rule
;; returns each use of the predicate in a rule
(defun find-predicate-in-rule (pred rl)
  (if (justification? rl)
      (find-predicate pred (justification-literals rl))
      (find-predicate pred (rule-literals rl))))
  

;; takes a predicate and a list of literals
;; returns each literal that matches the predicate
;; ignores modals and negations.
(defun find-predicate (pred lit-list)
  (delete-if-not #'(lambda (x) (eql (predicate-name pred) (predicate-name x)))
		 lit-list))

(let ((skhash (make-hash-table :test 'equal)))
  ;; we keep a hash table around for efficiency, although this 
  ;; prohibits parallel execution.
  
  ;; tests whether two rule instantiations are equivalent.
  ;;
  ;; returns t if r1 and r2 are equivalent.
  ;; all domain constants are identical.
  ;; all skolems are equivalent. here that means that they preserve 
  ;; rule structure. the following is an example where skolems are 
  ;; not equivalent:
  ;; (=> (and (p1 "SK1" "SK2") (p2 "SK2" "SK3")) (p3 "SK1" "SK3"))
  ;; (=> (and (p1 "SK1" "SK2") (p2 "SK2" "SK3")) (p3 "SK1" "SK2"))
  (defun justification-equal? (r1 r2)
    (clrhash skhash)
    (or (eq r1 r2)
	(justification-equal-help (justification-content r1) (justification-content r2))))

  ;; this is not intended as a public function.
  (defun justification-equal-help (r1 r2)
    (cond ((skolem? r1) 
	   (and (skolem? r2)
		;; this ensures the skolems are used in a structurally
		;; equivalent way. the initial matching of two skolems
		;; binds them so that if sk1 ~= sk2 in the second rule 
		;; then sk1 ~/= sk3.
		(if (gethash r1 skhash)
		    (skolem-equal? r2 (gethash r1 skhash))
		    (setf (gethash r1 skhash) r2))))
	  ((atom r1) (eql r1 r2))
	  (t 
	   ;; the consp here is for paranoia. we know that r1 is a cons, but
	   ;; we don't know anything about r2.
	   (and (consp r2)
		(justification-equal-help (car r1) (car r2))
		(justification-equal-help (cdr r1) (cdr r2))))))
)

(let ((skhash (make-hash-table :test 'equal)))
  ;; we keep a hash table around for efficiency, although this 
  ;; prohibits parallel execution.
  
  ;; tests whether r1 subsumes r2
  ;; t when the domain constants in r2 either equal those in r1 or
  ;;   match to equivalent skolems in r1.
  ;; very similar to justification-equal?
  ;; true if r2 specializes r1
  (defun justification-subsumes? (r1 r2)
    (clrhash skhash)
    (or (eq r1 r2)
	(justification-subsumes-help (justification-content r1) (justification-content r2))))

  ;; this is not intended as a public function.
  (defun justification-subsumes-help (r1 r2)
    (cond ((skolem? r1) 
	   ;; ensure structural equivalence
	   (if (gethash r1 skhash)
	       ;; here's the subsumption part...
	       (or (skolem-equal? r2 (gethash r1 skhash))
		   (eql r2 (gethash r1 skhash)))
	       (setf (gethash r1 skhash) r2)))
	  ((atom r1) (eql r1 r2))
	  (t 
	   ;; the consp here is for paranoia. we know that r1 is a cons, but
	   ;; we don't know anything about r2.
	   (and (consp r2)
		(justification-subsumes-help (car r1) (car r2))
		(justification-subsumes-help (cdr r1) (cdr r2))))))
)

;;;;;
;; clone-justification
;;
;; creates a copy of the justification. if map is provided, replaces
;; the constants in the key field of the map with those in the value
;; field.
;;
;; r - a justification
;; map - a hash table containing substitution instructions
;; full? - indicates whether a new justification structure should be created
;; 
;; when full? is true:  returns a new justification
;; otherwise:        returns the content for a new justification
(defun clone-justification (r &optional map (full? t) (new? nil))
  (labels ((clone-help (r &optional map aux?)
	     (if (atom r)
		 ;; replace the constant if requested
		 (if (and map (gethash r map))
		     (gethash r map)
		     r)
		 (cons 
		  (if aux? 
		      (clone-help (car r)) 
		      ;; don't replace predicate names on accident
		      ;; specialized test relies on horn rule structure to
		      ;; determine if an upcoming argument is a predicate name.
		      (clone-help (car r) map (or (listp (car r)))))
		  (clone-help (cdr r) map nil)))))
    ;; aux? defaults to true so that we can avoid replacing '=> at the
    ;; beginning of the rule.
    (if full?
	(make-justification :id (if new? 'JNEW (make-justification-id))
			    :content (clone-help (justification-content r) map t))
	(clone-help (justification-content r) map t))))

;;; 
;;; rl - a rule or justification
;;;
;;; return the literals in the consequent of rl
(defun consequent-literals (rl)
  (if (justification? rl)
      (collect-literals (justification-consequent rl))
      (collect-literals (rule-consequent rl))))

(defun antecedent-literals (rl)
  (if (justification? rl) 
      (collect-literals (justification-antecedent rl))
      (collect-literals (rule-antecedent rl))))


;;;; KNOWLEDGE BASE

;;; the knowledge base maps predicates to concepts, separating the 
;;; positive literals from the negative ones.
(defstruct kb 
  (pos-cpt (make-hash-table :test #'eq)) 
  (neg-cpt (make-hash-table :test #'eq))
  (rl-to-jst (make-hash-table :test #'eq)))


;;; map rule to its predicates in the knowledge base.
(defun store-rule (rl kb)
  (mapcar #'(lambda (x) 
	      (if (cdr x)
		  (push rl (gethash (car x) (kb-neg-cpt kb)))
		  (push rl (gethash (car x) (kb-pos-cpt kb)))))
	      (list-predicates rl))
  ;; caches instantiations for later use
  ;; assumes all rules are unique
  (setf (gethash rl (kb-rl-to-jst kb)) (instantiate-rule rl)))


;; return rules in kb that could explain lit.
(defmethod get-rules ((lit cons) (kb kb))
  (gethash (predicate-name lit) 
	   (if (negated? lit)
	       (kb-neg-cpt kb)
	       (kb-pos-cpt kb))))


;; return rules in kb that could explain b.
(defmethod get-rules ((b belief) (kb kb))
  (get-rules (belief-content b) kb))


;; return rules where b could appear in the antecedent.
;; if no rule list is given, defaults to searching the kb.
(defun get-rules-antecedent (b kb &optional rlst)
  (unless rlst (setf rlst (get-rules b kb)))
  (delete-if #'(lambda (r) 
		 (not (member (predicate-name (belief-content b))
			      (mapcar #'predicate-name (antecedent-literals r)))))
	     rlst))

;; return rules where b could appear in the consequent.
;; if no rule list is given, defaults to searching the kb.
(defun get-rules-consequent (b kb &optional rlst)
  (unless rlst (setf rlst (get-rules b kb)))
  (delete-if #'(lambda (r) 
		 (not (member (predicate-name (belief-content b))
			      (mapcar #'predicate-name (consequent-literals r)))))
	     rlst))

;;;; WORKING MEMORY

;;; working memory contains a table of worlds mapped to their
;;; identifiers, a reference to the world that the system believes, a
;;; reference to the world containing observations, a reference to the
;;; world that was home to the last inference,
(defstruct wm 
  (worlds (make-hash-table :test #'eq))
  prime
  current)

;; build, initialize, and return a new working memory.
(defun create-wm ()
    (let ((wm (make-wm)))
      (setf (wm-prime wm) (make-world :id 'self))
      (setf (wm-current wm) (wm-prime wm))
      (setf (gethash (world-id (wm-prime wm)) (wm-worlds wm)) (wm-prime wm))
      wm))

;; get the world with the given name
(defun get-world (wn wm)
  (gethash wn (wm-worlds wm)))

;; get the world where the belief is housed
(defun belief-home (b wm)
  (bid-home (belief-id b) wm))

;; returns the world where the belief appears
(defun bid-home (bid wm)
  (loop for w being the hash-values of (wm-worlds wm)
     when (get-local-belief bid w)
     return w))

;; returns a list of worlds where the belief appears
(defun bid-homes (bid wm)
  (loop for w being the hash-values of (wm-worlds wm)
     when (get-local-belief bid w)
     collect w))

;; get the world that holds the justification
(defun justification-home (j wm)
  (jid-home (justification-id j) wm))

(defun jid-home (jid wm)
  (find-if #'(lambda (w) (get-justification jid w)) (hash-table-values (wm-worlds wm))))

(defun print-bid (id) 
  (when (get-local-belief id (wm-prime wm))
    (format t "~%~A SUPPORT: ~A~%" id (get-support (get-local-belief id (wm-prime wm)) (wm-prime wm)))))

(defun print-jid (id)
  (when (get-justification id (wm-prime wm))
    (format t "~%~A SUPPORTED-BY: ~A~%" id (get-supported-beliefs (get-justification id (wm-prime wm)) (wm-prime wm)))))