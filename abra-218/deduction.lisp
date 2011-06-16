
#|
Some basic testing code

(test-one-init *shopping-rules* '((robbing R1) (get-weapon-step R1 G1) (getting G1) (get-weapon-step R1 G2) (getting G2)))
(setf wrld (first (first (get-observed-beliefs (wm-prime wm)))))
(setf blf  (second (first (get-observed-beliefs (wm-prime wm)))))
(setf rl (first (last (get-inst-rules (belief-content blf) kb))))
(full-groundings blf rl (wm-prime wm) wm)

(test-one-init *sm-rules* *sm-ex*)
(setf wrld (first (first (get-observed-beliefs (wm-prime wm)))))
(setf blf  (second (first (get-observed-beliefs (wm-prime wm)))))
(plan-ded blf (wm-prime wm) wm kb)

|# ;;|# 

;;; we're going to require that all variables appear in the antecedent
;;; for this to work. we can change that later if necessary.

;;; function that takes the focus and a matching rule and returns all
;;; fully supported unifications. actually, that might be sufficient.

(defun full-groundings (b j wrld wm)
  (let (cands lit new-j)
    (dolist (binding (find-bindings (expand-modals b wrld) j) cands)
      ;; we need a new version of update-justification that forces fully specific bindings
      (multiple-value-setq (new-j lit) (ground-justification j b binding wrld wm))
      ;; now, new-j either contains a completely new justification or an
      ;; existing one. (in the latter case, the belief will also be
      ;; redundant, and already supported by the existing justification.)
      ;;; (setf new-wb (update-belief b lit wrld wm)) not necessary since the belief is already fully grounded
      ;; change this once we fix the structure of the returned information.
      (mapcar #'(lambda (x) (push (list x b wrld) cands)) new-j))))


(defun deductively-supported? (j wrld wm)
  (not (remove-if #'(lambda (lit) (ded-find-belief lit wrld wm)) 
		  (collect-literals (justification-antecedent j)))))


;; returns a (possibly nil) list of fully grounded justifications.
(defun ground-justification (j b lit wrld wm)
  ;; two choices. either j is fully ground or it is at least partly skolemized. 
  ;; if it is fully ground, and b fits, then we're done. that's the candidate.
  ;; if it is partly skolemized, we have to start matching.
  (cond ((or (fully-grounded? j) (null b))
	 ;; step 0: when j is fully ground, stop and return j.
	 ;; this case fires only if j arrives fully bound, which is not currently possible.
	 (if (deductively-supported? j wrld wm)
	     (list j)))
	((not (bindable? b (justification-content j) wrld))
	 nil)
	(t
	 ;; step 1: unify b with j.
	 (let ((bndj (make-justification :id 'JNEW :content (bind-to-support (belief-content b) j lit)))
	       (nxtlt))
	   ;; step 2: find a literal in j that still has skolems and domain constants.
	   ;;         if there aren't any, then find a literal that still has skolems.
	   ;;; this line might not work unless all grounded literals appear in working memory
	   ;;; in that case, we would just be constructing justifications to tie the beliefs together
	   ;;; without any inference taking place (i.e., no new symbols). boring, but possibly useful later.
	   ;;; (setf nxtlt (first (sort (delete-if #'literal-grounded? (justification-literals bndj))
	   ;;;			    #'> :key #'(lambda (x) (count-if-not #'skolem? (literal-args x))))))
	   (setf nxtlt (first (sort (delete-if #'literal-grounded? (collect-literals (justification-antecedent bndj)))
	   			    #'> :key #'(lambda (x) (count-if-not #'skolem? (literal-args x))))))
	   
	   (if (null nxtlt)
	       (if (deductively-supported? bndj wrld wm)
		   (list bndj))
	       ;; step 3: call ground justification, passing j and each belief that could unify 
	       ;;         with the literal from step 2
	       (mappend #'(lambda (x) (ground-justification bndj (second x) nxtlt wrld wm))
			(ded-find-belief nxtlt wrld wm)))))))

;;;;;;;;;;;;;;;;;;;
;; XXX: stop-gap measure to get deduction to work.
;;
;; this code implements a find-belief procedure that returns all
;; beliefs that are identical to the literal or could unify with the
;; literal. it is only guaranteed to work with the single-agent case
;; as i need to redo the find-belief code as a whole (immediately) to
;; handle default beliefs correctly.
;;





;;; ded-fb
;;;
;;; returns (world belief) tuples for all beliefs that match the literal.
(defun ded-find-belief (l wrld wm)
  (cond ((or (null l) (null wrld)) nil)
	((belief-modal? l)
	 (ded-resolve-belief l wrld wm))
	((goal-modal? l)
	 (ded-fb-local (remove-goal-modal l) (world-goals wrld) nil))
	((intention-modal? l)
	 (ded-fb-local (remove-intention-modal l) (world-intentions wrld) nil))
	(t
	 (ded-find-belief-h l wrld))))

;;; resolve-belief
;;;
;;; walks through nested belief modals. supports literals that refer
;;; to an unknown agent. for example, (belief SK3 X) would search for
;;; X in all worlds that are treated as beliefs of other agents by the
;;; base world, wrld.
(defun ded-resolve-belief (l wrld wm)
  (labels ((fb-rb (l wrlds wm)
	     ;; strip a level of belief on each recursive call.
	     ;; collect the resulting beliefs. $$$ differs from
	     ;; resolve-belief by not stopping at the first match.
	     (append (ded-find-belief (third l) (first wrlds) wm)
		     (fb-rb l (rest wrlds) wm))))
    ;; we use a negation as failure model when it comes to resolving
    ;; beliefs.  the system will never explicitly state that agent A1
    ;; does not believe that agent A2 believes B.
    (if (skolem? (second l))
	;; agent is unspecified, so search through every connected agent world
	;; ASSUME: an agent only ever believes one world about another agent.
	(fb-rb l (mapcar #'second (world-blf-worlds wrld)) wm)
	;; find id in the indicated agent world.
	;; literal: (belief agent-id <literal>)
	(ded-find-belief (third l) 
			 (second (find-if #'(lambda (bw) (eq (first bw) (second l)))
					  (world-blf-worlds wrld)))
			 wm))))

;;; find-belief-h
;;; $$$ differs from find-belief-h in that it returns multiple beliefs if available.
(defun ded-find-belief-h (l wrld)
  (let (bs)
    ;; the belief is overshadowed by its contradiction
    (cond ((ded-fb-local l wrld t) (values nil t))
	  ;; the belief is local
	  ((setq bs (ded-fb-local l wrld nil)) (values bs t))
	  ;; find the belief in each imported world. if one of these
	  ;; is active in the current world (not in the history)
	  ;; then return it.
	  ((setq bs
		 (remove-if-not #'(lambda (y) 
				    (and y (not (gethash (belief-id (second y)) (world-history wrld)))))
			  ;; this code assumes that contradictions have been resolved.
			  (mappend #'(lambda (x) (ded-fb-local l (second x) nil))
				  (world-obs-worlds wrld))))
	   (values bs t))
	  ;; the belief isn't in the current world or in any of its observable worlds.
	  (t
	   (values nil nil)))))


;; ded-fb-local
;; 
;; looks for any beliefs in wrld that could unify with l. returns
;; (world belief) tuples for each such belief.
(defun ded-fb-local (l wrld flip-sign?)
  (when flip-sign? (setq l (flip-negation l)))
  ;; $$$ differs from fb-local by grabbing *all* matches
  (mappend #'(lambda (b) (list (list wrld b)))
	  (remove-if #'(lambda (b) (binding-conflict? (literal-args (belief-content b)) (literal-args l)))
		     (ec-get-elements (predicate-name l) (negated? l) (world-elmnts wrld)))))

;; for negation as failure, we need to change deductively-supported?
;; so that it returns true even if a negated antecedent is missing (as
;; long as its opposite isn't found). When the system fires the rule,
;; it'll abduce the negated condition and have it available to use in
;; other rules. That will change code in ground justification, because
;; there will be a special case when a belief does not exist, but we'd
;; like to pretend that it does.
;;
;; suppose we have (not (on sk1 A)) and (not (on B A)). the system
;; should treat these as separate beliefs (generating separate
;; candidates) even if (not (on sk1 A)) does not yet exist. it'll also
;; need to know that under this situation, a skolem is allowed.
