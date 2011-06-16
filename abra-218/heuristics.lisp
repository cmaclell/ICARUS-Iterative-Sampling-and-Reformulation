
;;;;;
;; pick-belief-*
;;
;; beliefs are chosen from the selected world and any worlds that it
;; imports. in general, the system will not choose to explain nested
;; beliefs unless directed to do so as a result of planning or problem
;; solving.

;;; returns the most recently believed belief in the list.
;;; maximizes belief-start.
(defun get-newest (wbs)
  (loop with newest = (first wbs)
     for wb in (rest wbs)
     when (> (belief-start (second wb)) (belief-start (second newest)))
     do (setf newest wb)
     finally (return newest)))

(defun get-agent-beliefs (world)
  (funcall #'append 
	   (get-local-beliefs world) 
	   (get-observed-beliefs world)
	   (get-goals world)
	   (get-intentions world)))

;; mostly for testing. selects a belief given its id.
(defun pick-belief-id (wm kb &key (world (wm-prime wm)) (id nil))
  (when id
    (setf world (bid-home id wm))
    (list world (get-local-belief id world))))

;; selects a random belief that's visible from the given world. if no
;; world is given, assume the primary system world. 
(defun pick-belief-random (wm kb &key (world (wm-prime wm)))
  (let ((blfs (get-agent-beliefs world)))
    (nth (random (length blfs)) blfs)))

;; selects the most recent belief. if world is specified, then limits
;; its search to beliefs viewable from that world.
(defun pick-belief-recency (wm kb &key (world (wm-prime wm)))
  (get-newest  (get-agent-beliefs world)))


;; returns the belief that can be explained by the fewest possible
;; rules.  favors making obvious inferences so that later inferences
;; so that we can use the new information to guide other inferences.
(defun get-fewest-rules (wbs kb)
  (when wbs
    (let* ((fewest (first wbs))
	   (fewest-score (length (get-rules (second fewest) kb)))
	   (tmp-score))
      (dolist (wb (rest wbs) (when (> fewest-score 0) fewest))
	(setf tmp-score (length (get-rules (second wb) kb)))
	(when (and (> tmp-score 0) 
		   (< tmp-score fewest-score))
	  (setf fewest wb)
	  (setf fewest-score tmp-score))))))

;; b - belief
;; kb - knowledge base
;;
;; true if there are rules that could potentially explain b
(defun attachable-belief? (b kb)
  (get-rules b kb))

;; b - belief
;; world - home world of b
;; kb - knowledge base
;;
;; true if the belief is either already explained or if it cannot be
;; explained by the knowledge base.
(defun attached-belief? (b world)
  (or (get-support b world)
      (belief-weak-links b)))

(defun remove-attached-beliefs (blfs world kb)
  (remove-if #'(lambda (x)
		 (when x
		   (or (attached-belief? (second x) world)
		       (not (attachable-belief? (second x) kb)))))
	     blfs))

;;; returns the most recent unexplained belief or the most recent belief if
;;; everything's been explained.
;;; HMM: predicates that cannot be explained given the current rule set
;;;      (e.g., (name bill2 bill)) can block progress. we could "dirty" 
;;;      the beliefs, but the current (easy) fix is to ignore beliefs that
;;;      can't be worked into the explanation.
(defun pick-belief-recency-unattached (wm kb &key (world (wm-prime wm)))
  (or 
   (get-newest (delete-if #'(lambda (x)
			      (when x
				(or (attached-belief? (second x) world)
				    (not (attachable-belief? (second x) kb)))))
			  (get-agent-beliefs world)))
   (pick-belief-random wm kb :world world)))

;; turns out that this approach is related to earlier work on
;; "essential explanations" see -- Olivier Fischer, Ashok Goel, John
;; R. Svirbely, Jack W. Smith, The role of essential explanation in
;; abduction, Artificial Intelligence in Medicine, Volume 3, Issue 4,
;; August 1991, Pages 181-191, ISSN 0933-3657, DOI:
;; 10.1016/0933-3657(91)90010-9.
(defun pick-belief-fewrules-unattached (wm kb &key (world (wm-prime wm)))
  (or 
   (get-fewest-rules (remove-attached-beliefs (get-agent-beliefs world) world kb) kb)
   (pick-belief-random wm kb :world world)))


;;;;;;
;; antecedent-belief?
;;
;; b - a belief
;; wrld - the world containing the belief
;; 
;; returns true if b has appeared in the antecedent of a justification
(defun antecedent-belief? (b wrld)
  ;; find out if the belief shows up in the head of any of those justifications
  (or 
   ;; get justifications within world
   (find-if #'(lambda (x) (member (belief-content b) (antecedent-literals x) :test #'literal-equal?))
	    (get-support b wrld))
   ;; get justifications across worlds
   (find-if #'(lambda (x) (member (expand-modals b (first x))
				  (antecedent-literals (second x))
				  :test #'literal-equal?))
	    (belief-weak-links b))))

;;;;;;
;; consequent-belief?
;;
;; b - a belief
;; wrld - the world containing the belief
;; 
;; returns true if b has appeared in the consequent of a justification
(defun consequent-belief? (b wrld)
  ;; find out if the belief shows up in the tail of any of those justifications
  (or 
   ;; get justifications within world
   (find-if #'(lambda (x) (member (belief-content b) (consequent-literals x) :test #'literal-equal?))
	    (get-support b wrld))
   ;; get justifications across worlds
   (find-if #'(lambda (x) (member (expand-modals b (first x))
				  (consequent-literals (second x))
				  :test #'literal-equal?))
	    (belief-weak-links b))))

;;;;;;
;; count-matching-rules
;;
;; b - a belief
;; w - a world
;; kb - a knowledge base
;; key - a function that extracts literals from a justification
;;
;; returns the number of instantiated rules in kb where b binds to at
;; least one of the literals extracted by key.
(defun count-matching-rules (b w kb &key (key #'justification-literals))
  (count t (mapcar #'(lambda (j) 
		       (consp (find-bindings-in-list (expand-modals b w) (funcall key j))))
		   (get-inst-rules (belief-content b) kb))))

;;find either 
;;1. beliefs with no justification
;;2. beliefs with a justification in one direction only

;;sort by number of rules that could justify the belief.
;;pay attention to directionality.

;; this is a modification of pick-belief-fewrules-unattached to treat
;; forward and backward inferences as two separate choices. the idea
;; is that each belief should be supported if possible and should
;; provide support for other beliefs. if a belief appears in two
;; rules, in the head of one and the tail of another, then both should
;; eventually fire if we're to make the most of the knowledgebase.
;; the "uda" stands for "unnattached, direction aware"

;; NOTE: needs a version of attached-belief? that returns true only if the 
;;       belief is attached in both directions.
;;       then needs a version of get-fewest-rules that returns the number
;;       of rules that could match the belief in that unrepresented direction.

;; XXX: initial fix to avoid endless cycles around the same focus.

;; a list of beliefs to ignore when selecting a focus
;; ((cycles-to-ignore belief)(...)...)
(defvar *bad-beliefs* nil)
;; a list containing the last three beliefs
(defvar *last-3-beliefs* nil)

(defun pick-belief-fewrules-uda (wm kb &key (world (wm-prime wm)))
  (setf *bad-beliefs* (mapcar #'(lambda (x) (list (- (first x) 1) (second x))) *bad-beliefs*))
  (setf *bad-beliefs* (delete-if #'zerop *bad-beliefs* :key #'first))
  (let* ((blfs (get-agent-beliefs world))
	 (uablfs (remove-attached-beliefs blfs world kb))
	 (retblf))
    (if uablfs
	;; first, prefer beliefs that are unexplained.
	(setf retblf (get-fewest-rules (set-difference uablfs (mapcar #'second *bad-beliefs*)) kb))
	(setf retblf
	      (or
	       ;; second, prefer beliefs that are unexplained in one direction.
	       (second (first (sort 
			       (remove-if #'zerop ;; drop cases with no potential rules
					  (loop for wb in (set-difference blfs *bad-beliefs* :key #'second)
					     when (not (consequent-belief? (second wb) world))
					     collect (list (count-matching-rules (second wb) world kb :key #'consequent-literals)
							   wb)
					     else when (not (antecedent-belief? (second wb) world))
					     collect (list (count-matching-rules (second wb) world kb :key #'antecedent-literals) 
							   wb))
					  :key #'first)
			       #'< :key #'first)))
	       ;; (pick-belief-recency wm kb :world world))))
	       ;; third, go wild.
	       (pick-belief-random wm kb :world world))))
    (push retblf *last-3-beliefs*)
    (when (> (length *last-3-beliefs*) 3) 
      (setf *last-3-beliefs* (subseq *last-3-beliefs* 0 2)))
    (when (every #'(lambda (x) (equal x (first *last-3-beliefs*))) (rest *last-3-beliefs*))
      (push (list 5 (second retblf)) *bad-beliefs*))
    retblf))


;; returns the instantiated rules from kb that can bind with lit.
(defun get-inst-rules (lit kb)
  (remove-duplicates 
   (mapcar #'(lambda (x)  
	       ;; (instantiate-rule x)))
	       (gethash x (kb-rl-to-jst kb))) ;; cached
	   (get-rules lit kb))))

;;;;;
;; old-plan-abd
;; 
;; the first stage in the plan-generate-test approach.
;;
;; * plan - applies cheap constraints that don't require binding the
;;   rule to a belief. this could be something like "if there's
;;   already an explanation for the belief, use that. the heuristics
;;   here can be just as flexible as anywhere else. this gets and
;;   analyzes the belief, the current applicable justifications, and
;;   the applicable rules and prunes a bit.
;;
;; options: 
;;   bc-only? - when true, will enforce inferences from the consequent
;;              only. if the consequent is a conjunction, only requires
;;              one member to fire the rule. we can later add a means
;;              to require multiple members.
;;
;;   focused? - when true, automatically prefers existing
;;              justifications over new ones. importantly, if a belief
;;              is already explained by a rule, then that
;;              justification will be selected, and no alternatives
;;              will be explored (unless randomization is used).
;;
;;   random? - when true, a single rule/justification will be selected
;;             uniformly at random. if focused? is true, then there is
;;             a fifty-percent chance that the result will be a new
;;             rule even when there is an existing justification.
(defun old-plan-abd (b wrld wm kb 
		     &key (bc-only? nil) (focused? nil) (random? nil))
  (let ((joptions (get-justifications wrld)) ;; existing justifications in wrld
	(roptions (get-inst-rules (belief-content b) kb)))

    ;; a justification may have variable bindings that conflict with
    ;; the belief. if so, then we need to toss it out.
    ;; also, rules and justifications may not match based on the
    ;; modality of the predicate, so we need to check for that.
    (setq joptions (delete-if #'(lambda (j) (not (bindable? b j wrld)))
			      joptions))

    (setq roptions (delete-if #'(lambda (r) (not (bindable? b r wrld)))
			      roptions))

    ;; combine the two lists depending on how conservative we want the
    ;; system to be.
    (unless (and focused? joptions)
      (setq joptions (append roptions joptions)))

    ;; we could add any number of optional constraints to apply at
    ;; this point. each one would serve to reduce the work required
    ;; during the candidate generation phase.

    ;; drop rules unless the belief predicate appears in the
    ;; head. (back chain abduction only)
    (when bc-only?
      (setq joptions (delete-if-not #'(lambda (rl) (in-consequent? b wrld rl))
				    joptions)))

    ;; inject a bit of randomness if you're brave.
    (mapcar #'(lambda (j) (make-candidate :original-justification j 
					  :original-belief b
					  :world wrld))
	    (if random?
		(if (and focused? roptions (= (random 2) 0))
		    (list (nth (random (length roptions)) roptions))
		    (when joptions
		      (list (nth (random (length joptions)) joptions))))
		joptions))))


;;;;;
;; plan-abd
;; 
;; the first stage in the plan-generate-test approach.
;;
;; * plan - applies cheap constraints that don't require binding the
;;   rule to a belief. this could be something like "if there's
;;   already an explanation for the belief, use that. the heuristics
;;   here can be just as flexible as anywhere else. this gets and
;;   analyzes the belief, the current applicable justifications, and
;;   the applicable rules and prunes a bit.
;;
;; options: 
;;   lookahead? - when true, uses heuristic search to find justifications
;;                that are on the path of connecting the focus to the
;;                larger collection of beliefs in working memory.
;;
;;                when false, see old-plan-abd.
;;
;; returns a list of candidates
(defun plan-abd (b wrld wm kb 
		 ;; none of these keywords are relevant in the new version
		 ;; we'll probably want different keywords
		 &key (lookahead? t) (bc-only? nil) (focused? nil) (random? nil))
  (if lookahead?
      (mapc #'(lambda (cj) 
		(setf (candidate-original-belief cj) b)
		(setf (candidate-world cj) wrld))
	    (pick-candidates (belief-content b) wrld wm kb))
      (old-plan-abd b wrld wm kb :bc-only? bc-only? :focused? focused? :random? random?)))


(defun plan-ded (b wrld wm kb)
  ;; rules may not match based on the modality of the
  ;; predicate, so we need to check for that.
  ;;
  ;; drop rules unless the belief predicate appears in the
  ;; tail. (forward chaining only)
  (mapcar #'(lambda (jblob) (make-candidate :original-justification (first jblob) 
					:original-belief (second jblob)
					:world (third jblob)))
	  (mappend #'(lambda (rl) (full-groundings b rl wrld wm))
		   (delete-if-not #'(lambda (rl) (in-antecedent? b wrld rl))
				  (delete-if #'(lambda (rl) (not (bindable? b rl wrld))) 
					     (get-inst-rules (belief-content b) kb))))))
  
;;;;;
;; generate-abduction
;; 
;; the second stage in the plan-generate-test approach.
;;
;;   * generate - carries out the bindings and can produce rules
;;     either by binding the individual belief to each candidate
;;     or by attempting to further specialize those candidates
;;     with the most specific beliefs.  (or possibly both)
;; 
;; takes a list of candidate rules/justifications as input and
;; specializes these to create a larger set of candidates.
(defun generate-abd (b wrld wm kb cands)
  ;; there are two options that we initially want to support 
  ;;
  ;;  * return the candidate justifications bound to the belief
  ;;  * return the candidates bound to the belief, and if more
  ;;    specific beliefs could appear in the rules, try binding to
  ;;    those as well.
  ;; 
  ;; for now, well just do the former because the latter option
  ;; considerably extends the search space.
  
  ;; in addition to each justification, if binding the belief to a
  ;; justification altered the belief's content (replaced skolems),
  ;; then we want to return the original belief so that the system
  ;; knows that it has been superceded (and should probably be
  ;; unbelieved, although that's not entirely necessary).
  (mappend #'(lambda (x) 
	       (mapcar #'(lambda (j) 
			   (make-candidate 
			    :original-belief (candidate-original-belief x)
			    :original-justification (candidate-original-justification x)
			    :la-score (candidate-la-score x)
			    :score (candidate-score x)
			    :world (third j)
			    :belief (second j)
			    :justification (first j)))
		       (bind-justification (candidate-original-justification x) b wrld wm)))
	   cands))


;;;;;
;; filter-abduction
;;
;; This is a a new stage in the plan-generate-test approach where we
;; remove candidates after the generation stage. Unification could
;; have led to justifications that violate temporal constraints,
;; pruning heuristics, or other potential requirements.
(defun filter-abd (kb cands)
  (hook-filter-inconsistent-jcandidates 
       ; filters inconsistent justifications when feature is turned on
  
  (when cands
    (let (fcands)
      ;; Filter: Time Constraints
      (setf fcands (if *temporal-filter* (temporal-filter cands) cands))

      ;; Filter: Directionality Preferences
      (when *divide-lookahead?* 
	;; all candidates will have the same original belief...
	(let ((c (first cands)) cb ab cob cw)
	  (setf cob (candidate-original-belief c) 
		cow (candidate-world c))
	  (setf cb (consequent-belief? cob cow))
	  (setf ab (antecedent-belief? cob cow))
	  (setf fcands 
		(cond ((or (and (not cb) (not ab)) (and cb ab))
		       cands)
		      ((and (not cb) ab 
			    (> (count-matching-rules cob cow kb :key #'consequent-literals) 0))
		       ;; keep only the candidates with belief in consequent
		       (loop for x in cands
			     when (in-consequent? (candidate-belief x) (candidate-world x) (candidate-justification x))
			     collect x))
		      ((and (not ab) cb
			    (> (count-matching-rules cob cow kb :key #'antecedent-literals) 0))
		       ;; keep only the candidates with belief in antecedent
		       (loop for x in cands
			     when (in-antecedent? (candidate-belief x) (candidate-world x) (candidate-justification x))
			     collect x))))))

      ;; Filter: Internal Contradictions
      ;; get rid of justifications that contain X and (not X). we'll
      ;; need to make contradictory-justification time aware when we
      ;; reason about actions.
      (delete-if  #'contradictory-justification fcands :key #'candidate-justification)))))

(defvar *filter-inconsistent-justifications* nil)
   ; default of hook is turned off

(defun hook-filter-inconsistent-jcandidates (candidates)

  "hook to filter out justifications that don't satisfy consistency constraints"
  
  (if *filter-inconsistent-justifications*
      (filter-inconsistent-jcandidates candidates) 
      
      candidates))


;;;;;
;; test-abduction
;;
;; the third stage in the plan-generate-test approach.
;;
;;   * test - evalutes each candidate and selects the one that is
;;     the most promising.
(defun test-abd (wrld wm cands)
  (dolist (c cands cands)
    (setf (candidate-score c) 
	  (score-justification (candidate-justification c) wrld wm :cost 1))))

(defun display-justifications (wrld)
  (dolist (j 
	    (sort (mapcar #'(lambda (j) (cons (score-justification j wrld wm) j)) 
			  (get-justifications wrld))
		  #'< :key #'car))
    (format t "~A: ~A~%" (car j) (cdr j))))

(defun display-beliefs (wrld)
    (dolist (b (sort (mapcar #'(lambda (wb)
				 (cons (length (get-support (second wb) wrld)) (second wb)))
			     (get-local-beliefs wrld))
		     #'< :key #'car))
      (format t "~A: ~A~%" (car b) (cdr b))))

;; the justification gains points for using beliefs that are supported
;; by other existing justifications. when cost is greater than zero,
;; justifications are penalized by the number of new assumptions they
;; make (cost is uniformly applied to all assumptions).
#|
(defun score-justification (j wrld wm &key (cost 0))
  (let ((score 0) b (nass 0) old-skolems new-skolems nsk)
    (dolist (lit (justification-literals j))
      (setf b (find-belief lit wrld wm))
      (if b
	  (progn 
	    (incf score (score-belief (second b) (first b) wrld))
	    (append (remove-if-not #'skolem? lit) old-skolems))
	  (progn 
	    (incf nass)
	    (append (remove-if-not #'skolem? lit) new-skolems))))
    ;; number of skolems introduced by the justification
    (setf nsk (length (set-difference new-skolems old-skolems)))
    ;; for now, subtract the number of assumptions weighted by some cost measure.
    (- score (* nass cost))))
    ;; (- score (* nsk cost))))
|#

;;; TEST: using an average instead of a sum to reduce the influence of rule length.
;;; don't count found beliefs in the ignore list
(defun score-justification (j wrld wm &key (cost 0) (ignore nil))
  (declare (ignore cost))
  (let ((score 0) b)
    (dolist (lit (justification-literals j) 
	     (/ score (length (justification-literals j))))
      (setf b (find-belief lit wrld wm))
      (when (and b (not (find (belief-content (second b)) ignore :test #'equal))) 
	(incf score (score-belief (second b) (first b) wrld))))))

(defun score-belief (b bwrld jwrld)
  (if (eql bwrld jwrld)
      ;; belief and justification are in the same world, so return the local score
      ;; XXX: this line doesn't lead to the intended results.
      ;; (if (find-if #'skolem? (belief-content b)) 1 (length (get-support b bwrld)))
      (length (get-support b bwrld))
      ;; justification is in a different world, so report the normal
      ;; score for the belief in its own world plus any extra support
      ;; it has in jwrld
      (+ 
       (if (member bwrld (world-obs-worlds jwrld) :key #'second) 1 0) ; a point for being an observation
       (length (get-support b bwrld)) ; points for local support in bwrld
       ;; points for other connections to jworld
       (count-if #'(lambda (x) (eql (first x) jwrld)) (belief-weak-links b))))) 

;;; XXX: Working Here 
;;; 
(defun score-belief-testing (b bwrld jwrld)
  (if (eql bwrld jwrld)
      ;; belief and justification are in the same world, so return the local score
      (* (count-if-not #'skolem? (literal-args (belief-content b))) (length (get-support b bwrld)))
      ;; justification is in a different world, so report the normal
      ;; score for the belief in its own world plus any extra support
      ;; it has in jwrld
      (+ 
       (if (member bwrld (world-obs-worlds jwrld) :key #'second) 1 0) ; a point for being an observation
       (length (get-support b bwrld)) ; points for local support in bwrld
       ;; points for other connections to jworld
       (count-if #'(lambda (x) (eql (first x) jwrld)) (belief-weak-links b))))) 

;;;;;
;; select-abduction
;;
;; input: cands - a list of scored candidates
;;        selector - a function to select among the candidates
;;        probability - a probability
;;
;; output: a candidate explanation
;; 
;; the explanation is chosen based on the selector function. if
;; probability is greater than zero, then there is a chance
;; (corresponding to the probability) that a random candidate will be
;; selected instead.
(defun select-abd (cands 
		   &key (selector #'high-score-elt) (probability -1.0))
  ;; (print cands)
  ;; (print "-------------------------")
  (if (and (> probability 0.0) (< (random 1.0) probability))
      (random-elt cands)
      (funcall selector cands)))

;;;;;
;; random-elt
;;
;; input: a list
;; output: a random element from that list
(defun random-elt (lst)
  (when lst (nth (random (length lst)) lst)))

;;;;;
;; trim-candidates
;;
;; input: a list of candidates sorted by their scores.
;;        the threshold for keeping a candidate.
;; output: a list of candidates whose scores are higher than
;;         the threshold.
(defun trim-candidates (lst scr scrf)
  (cond ((null lst) nil)
	((< (funcall scrf (car lst)) scr) nil)
	(t (cons (car lst) (trim-candidates (cdr lst) scr scrf)))))

;;;;;
;; high-score-elt (selector)
;; 
;; input: a list of candidates
;; output: the candidate with the highest score. picks randomly
;;         from the list of candidates with the highest score.
(defun high-score-elt (lst)
  (when lst
    (let (slst)
      (setf slst (sort (copy-list lst) #'> :key #'candidate-score))
      (setf slst (trim-candidates slst (candidate-score (car slst)) #'candidate-score))
      ;; resort to lookahead score when there's a tie.
      (when (second slst) 
	(setf slst (sort (copy-list slst) #'> :key #'candidate-la-score))
	(setf slst (trim-candidates slst (candidate-la-score (car slst)) #'candidate-la-score)))
      ;; if all else fails, go random.
      (random-elt slst))))

(defun candidate-split-by-chaining (cands)
  (let (tail head)
    (dolist (c cands (list tail head))
      ;; who knows? the literal might match in both locations.
      (when  (member (belief-content (candidate-belief c))
		     (consequent-literals (candidate-justification c))
		     :test #'literal-equal?)
	(push c head))
      (when  (member (belief-content (candidate-belief c))
		     (antecedent-literals (candidate-justification c))
		     :test #'literal-equal?)
	(push c tail)))))

;;;;;;
;; fresh-candidate
;;
;; c - a candidate structure
;; original? - if true, then evaluate the original-justification 
;;             as opposed to justification
;;
;; returns true if j is flagged as a new justification (i.e., not
;; already in the system).
(defun fresh-candidate? (c &key original?)
  (if original?
      (eq 'JNEW (justification-id (candidate-original-justification c)))
      (eq 'JNEW (justification-id (candidate-justification c)))))


(defun all-new-justifications? (clst &key original?)
  (not (find-if-not #'(lambda (c) (fresh-candidate? c :original? original?)) clst)))

;;;;;
;; high-score-connect (selector)
;; 
;; input: a list of candidates
;; output: the candidate with the highest score.
;;
;; XXX: close, but no cigar.
(defun high-score-connect (lst)
  (when lst
    (let* ((slst (candidate-split-by-chaining lst))
	   (hlst (sort (copy-list (second slst)) #'> :key #'candidate-score))
	   (tlst (sort (copy-list (first slst)) #'> :key #'candidate-score)))
      (when tlst
	(setf tlst (trim-candidates tlst (candidate-score (car tlst)) #'candidate-score)))
      (when hlst 
	(setf hlst (trim-candidates hlst (candidate-score (car hlst)) #'candidate-score)))
      
     
      (setf slst '())
      (when (all-new-justifications tlst)
	(setf slst (append tlst slst)))
      (when (all-new-justifications hlst)
	(setf slst (append hlst slst)))
      (unless slst
	(setf slst (append hlst tlst)))

      ;; resort to lookahead score when there's a tie.
      (when (second slst) 
	(setf slst (sort (copy-list slst) #'> :key #'candidate-la-score))
	(setf slst (trim-candidates slst (candidate-la-score (car slst)) #'candidate-la-score)))
      ;; if all else fails, go random.
      (random-elt slst))))

;;;;; 
;; high-score-new (selector)
;;
;; input: a list of candidates
;; output: the candidate with the highest score that (a) is not already 
;;         explaining the belief and (b) does not generalize an existing
;;         explanation of the belief.
(defun high-score-new (lst)
  (when lst
    (let ((slst (sort (copy-list lst) #'> :key #'candidate-score)))
      (loop for c in slst 
	 when (or (fresh-candidate? c)
		  (find-if-not #'literal-grounded? (justification-literals (candidate-justification c))))
	 return c))))


;;;;;
;; print-candidate
;;
;; print a candidate in a way that's suitable for debugging.
(defun print-candidate (c str d)
  (declare (ignore d))
  (format str "~%Candidate ::~%~TScore ~A~%~TLookahead ~A~%~TW: ~A~%~TB: ~A to ~%~T~T~T~A~%~TJ: ~A to ~%~T~T~T~A"
	  (candidate-score c) 
	  (candidate-la-score c)
	  (candidate-world c)
	  (candidate-original-belief c)
	  (candidate-belief c)
	  (candidate-original-justification c)
	  (candidate-justification c)))

;;;;;
;; candidate
;;
;; a structure that contains all the relevant information for
;; candidate explanations of a belief.
(defstruct (candidate (:print-function print-candidate)
		      (:predicate candidate?))
  original-belief        ;; belief before unification
  original-justification ;; justification before unification
  world                  ;; the home world of belief if it exists
                         ;; or the world that should eventually contain a new belief
                         ;; not necessarily the intended home of the justification
  belief
  justification
  (la-score 0) ;; score based on a lookahead heuristic
  (score 0))

(defun candidate-constraints (candidate)
  (justification-constraints
    (candidate-justification candidate)))
