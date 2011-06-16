;;;;; inference.lisp
;;;
;;; Inference 
;;;
;;; 1.	New literals arrive.
;;; 2.	Pick a literal based on recency and value of information.
;;; 3.	Find all rules that, once grounded, could contain that literal.
;;; 4.	Rank the rules by coherence.
;;; 5.	Fire the most coherent rule; assume new literals as needed.
;;; 6.	Repeat until time runs out.
;;;
;;; Assumptions about rules/justifications:
;;;
;;; * written as IF-THEN rules.
;;; * the antecedent and consequent are conjunctions of arbitrary length.
;;; * individual predicates may be negated on either side of the implication.
;;;
;;; Assumptions about beliefs:
;;;
;;; * associated with a unique identifier
;;; * may be positive or negative literals
;;;
;;; Temporary assumptions:
;;;
;;; * modals will eventually have arguments indicating the agents as with
;;;   (=> (and (belief ?a (shopping ?s)) (belief ?a (find-step ?s ?f)))
;;;       (goal ?a (finding ?f)))
;;; * for now, we assume a single agent and treat literals without a modal
;;;   as if they are beliefs.
;;;   (=> (and (shopping ?s) (find-step ?s ?f))
;;;       (goal (finding ?f)))

;; add incoming facts to working memory by storing them in the
;; observation world.
;;
;; XXX: not handling contradictions yet. suppose a fact comes in
;; that contradicts something previously observed. in that case, we
;; would need to call a belief revision routine.
#|
(defun add-facts (lst src wm)
  ;; find the source world that will house these beliefs
  (let ((wrld (second (find src (world-obs-worlds (wm-prime wm)) :key #'first))))
    ;; add the facts to that world
    (mapcar #'(lambda (x) 
		;; TODO: replace flakey crap for demo
		(cond ((intention-modal? x)
		       (believe (second x) (world-intentions (wm-prime wm)) t))
		      ((goal-modal? x)
		       (believe (second x) (world-goals (wm-prime wm)) t))
		      (t
		       (let ((b (find-belief x wrld wm)))
			 (unless b (setq b (believe x wrld t)))))))
	    lst)))
|#

(defvar *split-skolem-hook* nil)
(defvar *use-kb-constraints* nil)


(defun add-facts (lst src wm)
  ;; find the source world that will house these beliefs
  (let ((wrld (get-world src wm)))
    ;; add the facts to that world
    (mapcar #'(lambda (x) 
		;; TODO: replace flakey crap for demo
		(cond ((intention-modal? x)
		       (believe (skolemize (second x)) (world-intentions (wm-prime wm)) t))
		      ((goal-modal? x)
		       (believe (skolemize (second x)) (world-goals (wm-prime wm)) t))
		      (t
		       (let ((b (find-belief (skolemize x) wrld wm)))
			 (unless b (setq b (believe (skolemize x) wrld t)))))))
	    lst)))

;;;;;
;; infer
;;
;; runs a single inference step, selecting a belief according to :pick-belief.
(defun abra-infer (wm kb &key (w (wm-prime wm)) (bc-only? nil) (focused? nil) (p -1.0)
	                 (pick-belief #'pick-belief-random) (deduction? nil))
  ;; get a world--belief tuple to explain
  (let* ((wb (funcall pick-belief wm kb :world w)) 
	 (b (second wb)) 
	 (w (or w (first wb)))) 
    ;; explain b in the context of w
    (if deduction?
	(deduce-from-b b wm kb w)
	(explain-b b wm kb w bc-only? focused? p))))

;;;;;
;; explain-b
;;
;; runs a single inference step, given a belief.
(defun explain-b (b wm kb &optional (w (wm-prime wm)) (bc-only? nil) 
		  (focused? nil) (p -1.0))
  ;; select a candidate justification
  (let ((c (select-abd 
	    (test-abd w wm 
		      (filter-abd kb
		       (generate-abd b w wm kb
				     (plan-abd b w wm kb 
					       :bc-only? bc-only? 
					       :focused? focused?
					       :random? nil))))
	    :probability p)))
    ;; add new knowledge and carry out truth maintenance.
    
        
    (when c
      
      (when *split-skolem-hook*
        (setf c (split-skolem c)))
      
      (apply-explanation c w wm))))

;; like explain-b but returns the candidate explanation without applying it.
(defun test-explain-b (b wm kb &optional (w (wm-prime wm)) (bc-only? nil) 
		  (focused? nil) (p -1.0))
  (let ((c (select-abd 
	    (test-abd w wm 
		      (filter-abd kb
		       (generate-abd b w wm kb
				     (plan-abd b w wm kb 
					       :bc-only? bc-only? 
					       :focused? focused?
					       :random? nil))))
	    :probability p)))
    c))

(defun deduce-from-b (b wm kb &optional (w (wm-prime wm)))
  ;; select a candidate justification
  (let ((c (select-abd 
	    (test-abd w wm 
		      (filter-abd kb
		       (generate-abd b w wm kb
				     (plan-ded b w wm kb))))
	    :probability -1.0)))
    ;; add new knowledge and carry out truth maintenance.
    (when c
      (apply-explanation c w wm))))

;;;;;
;; modal-bindable?
;;
;; takes two literals and returns nil if the first literal cannot bind
;; to the second. expects modals to be expanded.
(defun modal-bindable? (lit1 lit2)
  ;; XXX: assumes that modals will not be negated.
  (and
   (eql (predicate-name lit1) (predicate-name lit2))
   (match-modals? lit1 lit2)
   (eql (negated? (strip-modals lit1)) 
	(negated? (strip-modals lit2))) 
   (not (binding-conflict? (literal-args lit1) 
			   (literal-args lit2)))))


;;;;;
;; lit-bindable?
;;
;; takes a belief and a rule or the contents of a justification
;; returns nil if the belief cannot bind to the rule
(defun lit-bindable? (lit rl)
  (find-if #'(lambda (p)
	       (modal-bindable? lit p))
	   (find-predicate-in-rule lit rl)))

;;;;;
;; bindable?
;;
;; takes a belief and a rule or the contents of a justification
;; returns nil if the belief cannot bind to the rule. expands modals.
(defun bindable? (b rl w)
  (lit-bindable? (expand-modals b w) rl))


;;;;;
;; binding-conflict?
;;
;; returns true if the constants in new-args cannot override
;; the constants in old-args.
(defun binding-conflict? (new-args old-args)
  (mismatch new-args old-args :test #'bindable-args?))


;;;;;
;; bindable-args?
;;
;; true when one argument can be bound to another
(defun bindable-args? (arg1 arg2)
  (or (eq arg1 arg2)
      (skolem? arg1)
      (skolem? arg2)))

;;;;;
;; in-consequent?
;;
;; returns true if the predicate in belief b appears in the 
;; consequent of rule rl
(defun in-consequent? (b w rl)
  (loop with bc = (expand-modals b w)
     for lt in (consequent-literals rl)
     when (and (eql (predicate-name bc) (predicate-name lt))
	       (match-modals? bc lt))
     return t))

;;;;;
;; in-antecedent?
;;
;; returns true if the predicate in belief b appears in the 
;; antecedent of rule rl
(defun in-antecedent? (b w rl)
  (loop with bc = (expand-modals b w)
     for lt in (antecedent-literals rl)
     when (and (eql (predicate-name bc) (predicate-name lt))
	       (match-modals? bc lt))
     return t))


;;;;;
;; apply-binding
;;
;; replace skolems in the belief with corresponding domain constants
;; in the literal.
;;
;; returns a literal that merges the arguments in the contents of b
;; and lit. modal operators in lit are preserved.
;; 
;; future: will need to take a world argument to handle modals that 
;; have arguments other than the literal they modify.
(defun apply-binding (b lit)
  (labels ((replace-args (b lit) 
	     (cons (predicate-name (belief-content b))
		   (mapcar #'(lambda (x y) 
			       (if (and (not (skolem? x))
					(skolem? y))
				   x 
				   y))
			   (literal-args lit)
			   (literal-args (belief-content b))))))
    (prepend-modals (list-modals lit) 
		    (if (negated? (belief-content b)) 
			(negate (replace-args b lit))
			(replace-args b lit)))))

;;;;;
;; make-substitution-map
;;
;; creates a map between skolems in lit1 and domain constants in lit2
;;
;; TODO: support modals with arguments other than the literal.
;;       e.g., (goal ?X (foo bar))
(defun make-substitution-map (lit1 lit2)
  (loop with ht = (make-hash-table) 
        for arg1 in (literal-args lit1) 
        and arg2 in (literal-args lit2)
     unless (or (equivalent-args? arg1 arg2)
		(gethash arg1 ht)
		(not (skolem? arg1)))
     do (setf (gethash arg1 ht) arg2)
     finally (return ht)))

;;;;;
;; find-bindings
;; 
;; takes a (modal) literal and a justification
;;
;; returns the literals in the justification that can unify with the
;; given literal.
(defun find-bindings (mlit j)
  ;; only consider literals that have the same modal operators,
  ;; the same predicate, and the same negation status.
  (delete-if-not #'(lambda (x) 
		     (modal-bindable? mlit x))
		 (find-predicate-in-rule mlit j)))

(defun find-bindings-in-list (mlit litlst)
  ;; only consider literals that have the same modal operators,
  ;; the same predicate, and the same negation status.
  (delete-if-not #'(lambda (x) 
		     (and (match-modals? x mlit)
			  ;; XXX: assumes that modals will not be negated.
			  (eq (negated? (strip-modals x)) (negated? (strip-modals mlit)))
			  (not (binding-conflict? (literal-args mlit) (literal-args x)))))
		 (find-predicate mlit litlst)))


;;;;;
;; pick-binding
;; 
;; takes a justification, a belief, the world where the justification
;; lives, and an optional selection function.
;;
;; returns the literal in the justification that best fits the belief
;; according to the selection function.
(defun pick-binding (j b jw &optional (selector #'most-specific-binding))
  ;; * get all possible matchings of this belief to the rule and then
  ;; * find the most specific binding
  ;; * get rid of candidates whose negation status doesn't match the belief
  ;; * return the best candidate literal to assign to the belief
  (let ((mlit (expand-modals b jw)))
    (funcall selector 
	     mlit
	     (find-bindings mlit j))))

;;;;;
;; update-justification
;;
;; takes a justification, a belief, a literal that the belief should match,
;; and a world that encloses the reasoning.
;;
;; returns the justification that would ultimately match the belief
;; along with the literal that matches the belief. note that the
;; skolems in the literal may have different names from the ones in
;; the justification. if the justification is new, then its ID is JNEW.
(defun update-justification (j b lit wrld)
  (let (tmpj)
    (if (find-if-not #'skolem? (literal-args (belief-content b)))
	;; if we can replace skolems in the justification with domain
	;; constants from the belief, do it, otherwise poke around for
	;; an existing version of the justification and return that (or
	;; the original input).
	(if (mismatch (literal-args lit) (literal-args (belief-content b)) :test #'equivalent-args?)
	    (multiple-value-bind (new-j lit) (bind-to-support (belief-content b) j lit)
	      (setf tmpj (find-justification (make-justification :id 'JTMP :content new-j) wrld))
	      (if tmpj
		  (values tmpj lit)
		  ;; until they're official, new justifications get a flag ID
		  (values (make-justification :id 'JNEW :content new-j) lit)))
	    ;; no replaceable skolems.
	    (if (setf tmpj (find-justification j wrld))
		(values tmpj lit)
		(values j lit)))
	;; case: all belief constants are skolems.
	(cond ((get-justification (justification-id j) wrld)
	       ;; if the justification already exists, then just use it.
	       (values j lit))
	      ((setf tmpj (find-subsumed-justification j wrld))
	       ;; if the justification doesn't already exist, look for
	       ;; one that it subsumes and use that one.  make sure to
	       ;; update the literal for the new justification.
	       (values tmpj (pick-binding tmpj b wrld)))
	      (t
	       ;; justification doesn't exist and there's nothing that
	       ;; it subsumes, so just use what you have.
	       (values j lit)))))) 

;; binds the skolems in b to domain constants in lit. if this creates
;; a new belief, then it's added to the world.
;;
;; returns a tuple containing either
;; (a) the world that contains b and b (no change)
;; (b) the world that should contain the new belief and the new belief with ID 'BNEW
(defun update-belief (b lit wrld wm)
  ;; either b and lit will only contain skolems, or they will have
  ;; equivalent arguments, or we'll need to update b. in the first
  ;; two cases, we just need to add new-rule as support for b. in
  ;; the third case, we need to make some changes beforehand.
  (if (mismatch (literal-args lit) (literal-args (belief-content b)) :test #'equivalent-args?)
      ;; is the new belief already in the world? if so, use
      ;; the existing belief, otherwise return the new belief.
      (let* ((nlit (apply-binding b lit))
	     (tmp-b (find-belief nlit wrld wm)))
	(if tmp-b
	    tmp-b ;; existing belief
	    (list (resolve-world nlit wrld)
		  (make-belief :id 'BNEW 
			       :content (strip-modals nlit)))))
      ;; no change required
      (list (belief-home b wm) b)))

;;;;; EXTENDED NOTE ;;;;;
;;
;; abduction, in the sense of creating new beliefs, happens in two
;; ways given a rule and a belief. first, as the belief is matched to
;; the justification some of its skolems may be replaced by domain
;; constants. the new belief supersedes the original one under the
;; assumption that the skolems match constants from the justification.
;; this increases coherence.  second, as the justification is matched
;; to the belief, the justification may be completely new or skolems
;; that occur in multiple literals of an existing justification may be
;; replaced by domain constants from the belief. if these new literals
;; don't exist in working memory, they're assumed, which brings more
;; beliefs into the system without perception.

;;;;;
;; bind-justification
;;
;; given a justification, a belief, and a world, bind the belief to
;; the justification. since skolems may be replaced by domain
;; constants, this could generate a new belief and a new justification
;; or transform the input into existing structures.
;;
;; returns a list of raw candidate justifications. the list contains
;; triples that consist of
;; (1) the justification after being bound to the belief
;; (2) the belief after being bound to the justification
;; (3) the world where the belief should reside
(defun bind-justification (j b wrld wm)
  (let (cands lit new-j new-wb)
    (dolist (binding (find-bindings (expand-modals b wrld) j) cands)
      ;;; XXX
      ;; belief appears as-is in the justification, so do not suggest 
      ;; other bindings that would specialize it.
      ;; ultra-conservative fix to the bug where a general belief would bind
      ;; to a more specific belief already used in the justification and the
      ;; system would lose track of which literal was actually changed.
      ;; (when (equivalent-args? (belief-content b) binding)
      ;; (return-from bind-justification (list (list j b wrld))))

      (multiple-value-setq (new-j lit) (update-justification j b binding wrld))
      ;; now, new-j either contains a completely new justification or an
      ;; existing one. (in the latter case, the belief will also be
      ;; redundant, and already supported by the existing justification.)
      (setf new-wb (update-belief b lit wrld wm))
      (push (list new-j (second new-wb) (first new-wb)) cands))))

;; add the belief to its intended home world if it's new
(defun add-new-belief (c)
  (when (eq (belief-id (candidate-belief c)) 'BNEW)
    (setf (candidate-belief c) 
	  (believe (belief-content (candidate-belief c)) (candidate-world c)))))

;; add the justification to the focal world if it's new
(defun add-new-justification (c wrld wm)
  (when (fresh-candidate? c)
    (setf (candidate-justification c)
	  (fire-justification 
	   (make-justification :content (justification-content (candidate-justification c)))
	   wrld wm))))

;;;;;;
;; remove-justification-if-allowed
;;
;; input:
;; b - belief that is suspected of being specialized
;; j - justification that supported that belief
;; wrld - world that the system is focusing on (perspective)
;; wm - working memory
;; 
;; if b or j are owned by wrld, then j is removed from working memory.
;; returns the beliefs that are no longer supported by j if it was removed.
(defun remove-justification-if-allowed (b j wrld wm)
  ;; (a) if b is in wrld.
  ;;        drop jid.
  ;;        drop any beliefs that relied only on jid.
  ;;
  ;; [ more specific b in this world overshadows the more general 
  ;;   one in the imported world ]
  ;; (b) if b is in some imported world.
  ;;        if jid is in wrld.
  ;;           drop jid.
  ;;           drop any beliefs that relied only on jid.
  ;;; i can only remove the justification if my wrld owns the belief or the justification
  ;;; that's what makes this bit of code a little convoluted.
  (if (get-local-belief (belief-id b) wrld)
      (remove-justification j (justification-home j wm))
      (when (get-justification (justification-id j) wrld)
	(remove-justification j wrld))))

(defun move-isolated-justification (j c sub-map wrld wm)
  (let (jhome new-j exists-j)
    (setf jhome (justification-home j wm))
    ;; if j is duplicated in the list or was already removed and
    ;; this belief hadn't heard about it, we have an error.
    (assert jhome)
    ;; update any justifications in any world that rely on the original belief.
    (setf new-j (clone-justification j sub-map))
    ;; look for duplicate justifications (only) in the world that houses jid.
    (setf exists-j (get-justification (find-justification-id new-j jhome) jhome))
    ;; register either the old rule or the new one as support for the new belief.
    (if exists-j
	(register-support exists-j wrld (candidate-belief c) (candidate-world c))
	(fire-justification new-j wrld wm))))

(defun move-shared-belief-justification (j c wrld wm)
  ;; grab all potential bindings in this justification
  ;; bind justification should grab existing justifications if possible
  (let ((opts (bind-justification j (candidate-belief c) wrld wm))
	(score -1) best tmp)
    ;; find the highest scoring binding
    (dolist (opt opts)
      (setf tmp (score-justification (first opt) wrld wm))
;;      (print tmp)
 ;;     (print (first opt))
      (when (> tmp score)
	(setf best (first opt) score tmp)))
    ;; process the best one.
    ;; if it already exists, then there's nothing to do. (i think.)
    (unless (find-justification-id best (justification-home j wm))
      (fire-justification (clone-justification best) wrld wm))))

;;;;;;
;; one-to-many-belief-update
;;
;; sometimes a single belief appears multiple times in a
;; justification. this can happen when a skolem eventually resolves to
;; two or more separate domain constants. we need to treat this
;; scenario differently by creating a new justification, moving one of
;; its support links to the new belief, and creating links to the old
;; (more general) belief. the original justification disappears, and
;; we don't move any of the other justifications for the original
;; belief to the new one (leaving them maximally general).  this is a
;; cautious strategy because we don't assume that the specialization
;; of the belief should percolate to the other justifications that it
;; originally supported.
(defun one-to-many-belief-update (c wrld wm)
  (let ((ht (make-hash-table))
	(sub-map (make-substitution-map 
		  (belief-content (candidate-original-belief c)) 
		  (belief-content (candidate-belief c)))))
    ;; isolate the duplicated justifications
    (dolist (j (get-support (candidate-original-belief c) wrld))
      (if (gethash j ht)
	  (incf (gethash j ht))
	  (setf (gethash j ht) 1)))
    ;; handle justifications where the belief appears once as normal.
    ;; do one localized search step when the belief appears multiple times in a justification.
    (loop for count being the hash-values of ht using (hash-key j)
       do (if (= count 1) 
	      (move-isolated-justification j c sub-map wrld wm)
	      (move-shared-belief-justification j c wrld wm)))
    ;; remove the original justification, which was just specialized.
    ;; drop any beliefs that are no longer supported.
    (loop for count being the hash-values of ht using (hash-key oldj)
       unless (or (fresh-candidate? c :original? t))
       do (dolist (empty-b (remove-justification-if-allowed (candidate-original-belief c) 
							    oldj
							    wrld wm))
	    (unless (or (fact? empty-b wrld) (null (belief-home empty-b wm)))
	      (unbelieve empty-b (belief-home empty-b wm) wm))))))

;;;;;;
;; one-to-one-belief-update
;;
;; if a belief never supports the same justification twice, then when
;; the belief is specialized, we update all the other justifications
;; that relied on that belief, specializing them all. in this special,
;; common case, the system can be aggressive.
(defun one-to-one-belief-update (c wrld wm)
  ;; note: should only be called when a belief was updated
  (let ((sub-map (make-substitution-map 
		  (belief-content (candidate-original-belief c)) 
		  (belief-content (candidate-belief c)))))
;; FIX: if cand-bel is already in the justification, then we treat this as
;;      a one-to-many case and do not move the support. i need to write an
;;      extended note about this problem because the fix may introduce new
;;      issues that will have to be addressed down the line. the final
;;      solution may involve altering the core mechanism for specializing
;;      the explanation. due to this option, we need to edit the focus selection
;;      routine so that the system doesn't get stuck trying to resolve the
;;      same belief in the same way.

    (dolist (j (get-support (candidate-original-belief c) wrld))
      (unless (member (candidate-belief c) (get-supported-beliefs j wrld) :key #'second) 
       
      (move-isolated-justification j c sub-map wrld wm)
      ;; drop any beliefs that are no longer supported
      (dolist (empty-b (remove-justification-if-allowed (candidate-original-belief c) j wrld wm))
	;; sometimes two beliefs might be linked by multiple justifications
	;; that could lead to two attempts to zap the same belief
	(unless (or (fact? empty-b wrld) (null (belief-home empty-b wm)))
	  (unbelieve empty-b (belief-home empty-b wm) wm)))))))

;;;;;;
;; apply-explanation
;;
;; input:
;; c - a candidate that contains the justification and the world--belief
;;     actually being explained.
;; wrld - the world where the belief is being explained, this may not
;;        be the world where the belief resides.
;; wm - working memory
;;
;; registers the belief in c if it's new, and ensures that all the
;; knowledge structures are updated appropriately.
(defun apply-explanation (c wrld wm)
  ;; test whether the justification will lead to a contradiction.
  ;; if so, then resolve the contradiction before applying the explanation.
  ;; if the justification is rejected, store the contradictory literal and 
  ;; the rejected justification.
  (add-new-belief c)
  (add-new-justification c wrld wm)

  ;; stop, unless c specializes an existing belief.
  (unless (eq (candidate-original-belief c) (candidate-belief c))
    (let ((support (get-support (candidate-original-belief c) wrld)))
      (if (= (length support) (length (remove-duplicates support)))
	  (one-to-one-belief-update c wrld wm)
	  (one-to-many-belief-update c wrld wm))))

  ;;; TODO: needs testing.
  ;;; I think this is all that's needed when removing stale justifications.
  ;;; notes:
  ;;;   * when a world imports beliefs (accepts w/o modal qualifiers), those beliefs are 
  ;;;     considered immutable and justified. the world can only override those beliefs locally.
  ;;;     this is the case when an agent uses an observational world or a counterfactual world
  ;;;     imports beliefs from the agent's "real" world.
  ;;;   * otherwise, the beliefs may be altered if local justifications are retracted. if my 
  ;;;     belief world drops a goal because it's no longer supported, then the goal world should
  ;;;     update accordingly.
  (unless (or
	   (null (find-justification-id (candidate-original-justification c) wrld)) ;; never existed
	   (eq (candidate-original-justification c) (candidate-justification c)) ;; never changed
	   (fresh-candidate? c :original? t)) ;; nothing to remove
    ;; remove the justification to get the emptyb's 
    ;; remove the emptyb's
    (dolist (empty-b (remove-justification (candidate-original-justification c) wrld))
      (unless (fact? empty-b wrld)
	(unbelieve empty-b (belief-home empty-b wm) wm)))))

;;;;
;;
;; bp - a positive or negative literal 
;; rps - literals from an instantiated rule that could bind to bp
;;       without any conflicts (plus same modals, same negation status)
;;
;; return - the literal in rps that shares the most domain constants
;;          with bp and, in case of a tie, would leave the fewest skolems
;;          in both literals after they're bound to each other.
(defun most-specific-binding (bp rps)
  (cond ((null rps) nil)
	((= (length rps) 1)  
	   (car rps))
	(t
	 ;; XXX: inelegant solution...
	 ;;      i don't know if this code has been tested. rules typically only contain 
	 ;;      a single instance of a predicate
	 (cadar (stable-sort
		 ;; count the arguments that won't be bound.
		 (mapcar #'(lambda (x) 
			     (list (loop for rarg in (literal-args x) and barg in (literal-args bp)
				      count (and (skolem? rarg) (skolem? barg)))
				   x))
			 rps)
		 #'(lambda (x y) 
		     ;; prefer bindings that don't involve skolems at all
		     (cond ((= (first x) (first y))
			    (< (count-if #'skolem? (literal-args (second x)))
			       (count-if #'skolem? (literal-args (second y)))))
			   ;; avoid bindings skolems to each other
			   (t 
			    (< (first x) (first y))))))))))

;; bind-to-support
;;
;; blit - literal from a belief
;; j - justification
;; lit - literal from j that can unify with blit 
;;
;; return the (modal) literal with the updated constants and a new
;; justification that contains that literal
;;
;; TODO: needs (A) to navigate any modals in lit so that the arguments
;;      will be substituted correctly and (B) to assign the world
;;      information if any of the arguments to the modal operator are
;;      skolems. We are not handling generic rules like B(?A, ?X) =>
;;      G(?A, ?X). The literal in X must be specified.
(defun bind-to-support (blit j lit)
  (let ((map (make-substitution-map lit blit)) tmp-j tmp-lit)
    (setf tmp-j (clone-justification j map nil))
    ;; update literal too
    (setf tmp-lit (cons (predicate-name lit) 
			(mapcar #'(lambda (x) 
				    (if (gethash x map) (gethash x map) x)) 
				(literal-args lit))))
    (values tmp-j 
	    (prepend-modals (list-modals lit)
			    (if (negated? lit) 
				(negate tmp-lit)
				tmp-lit)))))
