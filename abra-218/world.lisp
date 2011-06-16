;;;; WORLDS

;;;; Worlds are complicated creatures. Their primary purpose is to
;;;; store beliefs, but they can also store justifications. Worlds can
;;;; connect with each other in numerous ways.
;;;;
;;;; Importing: Importing world A into world B makes A's beliefs
;;;; visible to B. The relationship is designed to be one level
;;;; deep. An agent world may import a sense world, or a goal world
;;;; may import an agent world. The justifications, if any, are not
;;;; carried over. If the imported world imports other worlds, those
;;;; beliefs are not carried over. As a result, a goal world may need
;;;; to import its agent world and the sense worlds imported by the
;;;; agent. The more effective approach is for the goal world to ask
;;;; the agent world for a snapshot of all its beliefs. (The final
;;;; behavior in this case is to be determined, but one layered
;;;; imports with sensor worlds are well defined.)
;;;; 
;;;; Believing: World A may believe that world B reflects a particular
;;;; agent's beliefs. 
;;;;
;;;; History: When a world "unbelieves" something, it places the
;;;; belief in its history table. If the world owns the belief, then
;;;; the belief is given an end time and stored away. If the belief is
;;;; imported then a copy of the belief (with the same ID) is stored
;;;; away with a time stamp. Whenever the system uses a belief from an
;;;; imported world, it first checks in the history to ensure that the
;;;; belief has not been overridden. (It's too early in the project to
;;;; tell, but this may only occur when there's an explicit negation
;;;; that drives the belief to obsolescence. However, we could build
;;;; support for forgetfulness whereby old, weak beliefs are scrubbed
;;;; away.)

;; generate unique ids for worlds
(let ((wid 0)) 
  (defun reset-world-id () (setf wid 0))
  (defun make-world-id () 
    (symb "W" (incf wid))))

(defun print-world (w str d)
  (declare (ignore d))
  (format str "#W:~A" (world-id w)))

;; a world contains beliefs, the justifications for those beliefs, and
;; an identifier.
(defstruct (world (:predicate world?)
		   (:print-function print-world)) 
  (elmnts (make-ec)) ;; beliefs
  (goals) ;; world containing goals (if any)
  (intentions) ;; world containing intentions (if any)
  (blf-worlds '()) ;; worlds believed <(agent-name world)>
  (obs-worlds '()) ;; worlds containing observations <(src-name world)>
  (bs (make-bs)) ;; justifications
  (history (make-hash-table)) ;; belief id ==> belief
  (id (make-world-id)))

;;; observe-world
;;;
;;; name - an mnemonic for the world being observed
;;; new - a world to be treated as a source of observations
;;; base - the world that will act as the observer
;;;
;;; registers 'new' as an observed world of 'base'
(defun observe-world (name new base)
  (push (list name new) (world-obs-worlds base)))

;;; believe-world
;;;
;;; name - the agent who owns the beliefs
;;; new - the world assigned to the agent
;;; base - the world that holds beliefs about the agent
;;;
;;; the base world is said to believe that the agent specified by
;;; 'name' believes the contents of the world specified by 'new.'
(defun believe-world (name new base)
  (push (list name new) (world-blf-worlds base)))

;;; fact?
;;;
;;; b - a belief
;;; wrld - a world
;;;
;;; b is a fact with respect to wrld if wrld observes the world that
;;; contains b and b is not locally overridden.
(defun fact? (b wrld)
  (dolist (ow (world-obs-worlds wrld))
    (when (and (get-local-belief (belief-id b) (second ow))
	       (not (overridden-belief b wrld)))
      (return t))))

;;; expand-modals
;;;
;;; takes a belief and a world
;;; traces the path from world to belief
;;; returns the content of the belief with the modal information tacked on
;;;
;;; Temporary assumptions:
;;;
;;; * for now, we assume a single agent and treat literals without a modal
;;;   as if they are beliefs.
;;;   (=> (and (shopping ?s) (find-step ?s ?f))
;;;       (goal (finding ?f)))
;;;
;;; * we only handle goal and intent modals and don't support
;;;   nesting. this is very basic to get the single-agent version
;;;   working with these new world types.
;;;
;;; XXX: extend to handle other modalities.
(defun expand-modals (b w)
  (cond ((or (get-local-belief (belief-id b) w)
	     (fact? b w))
	 (belief-content b))
	((get-local-belief (belief-id b) (world-goals w))
	 (list 'goal (belief-content b)))
	((get-local-belief (belief-id b) (world-intentions w))
	 (list 'intention (belief-content b)))))
	
;;; get-support
;;;
;;; b - a belief
;;; world - a world
;;;
;;; get the justifications that support b in world. this does not
;;; return justifications from any other worlds.
(defun get-support (b world)
  (bs-supporting-justifications b (world-bs world)))

;;; get-supported-beliefs
;;;
;;; j - a justification
;;; world - a world
;;;
;;; get the world--belief tuples for the beliefs supported by j.
(defun get-supported-beliefs (j world)
  (bs-supported-beliefs j (world-bs world)))

;;; believed-world?
;;;
;;; w - the world assigned to the agent
;;; base-world - the world that holds beliefs about the agent
;;;
;;; returns a list containing the agent name and w when w records the
;;; beliefs of the agent according to the base-world.
(defun believed-world? (w base-world)
  (find w (world-blf-worlds base-world) :key #'second))

;;; observed-world?
;;;
;;; w - the world observed
;;; base-world - the potential observer world
;;;
;;; returns a list containing the observed world's mnemonic and w when
;;; base-world treats w as a source of observations.
(defun observed-world? (w base-world)
  (find w (world-obs-worlds base-world) :key #'second))

;;; add-if-no-contradiction
;;;
;;; b - a belief
;;; ht - a hash table that maps predicates to beliefs
;;;
;;; adds a belief to the hash table if (a) it isn't already in the
;;; hash table and (b) it's contradiction isn't already in the hash
;;; table.
(defun add-if-no-contradiction (b ht)
  (unless (find-if #'(lambda (y) (or (eq b y) (belief-contradiction? b y)))
		   (gethash (predicate-name (belief-content b)) ht))
    (push b (gethash (predicate-name (belief-content b)) ht))))

;;; get-observed-beliefs
;;;
;;; w - a world
;;;
;;; returns a list world--belief tuples containing all the beliefs in
;;; worlds observed by w that do not
;;;   (a) appear in the history of w as items no longer believed
;;;   (b) appear explicitly negated in w as items where the contradiction is believed
(defun get-observed-beliefs (w)
  (remove-if #'(lambda (ob) (overridden-belief (second ob) w))
	     (mappend #'(lambda (o) 
			  (mapcar #'(lambda (x) (list (second o) x))
				  (ec-get-all (world-elmnts (second o)))))
		      (world-obs-worlds w))))

(defun overridden-belief (b w)
  (or (gethash (belief-id b) (world-history w))
      (fb-local (belief-content b) w t)))

(defun get-goals (w)
  (when w (get-local-beliefs (world-goals w))))

(defun get-intentions (w) 
  (when w (get-local-beliefs (world-intentions w))))

;;; get-local-belief
;;; 
;;; id - a belief identifier
;;; wrld - the world to search
;;;
;;; returns the belief with specified by id if it is housed within
;;; wrld. does not follow links to other worlds.
(defun get-local-belief (id wrld)
  (ec-get-element id (world-elmnts wrld)))

;;; get-local-beliefs
;;;
;;; wrld - a world
;;; 
;;; returns a list of world--belief tuples containing the beliefs that
;;; exist within wrld. does not follow links to other worlds.
(defun get-local-beliefs (wrld)
  (mapcar #'(lambda (x) (list wrld x)) (ec-get-all (world-elmnts wrld))))

;;; get-justification
;;;
;;; jid - the identifier for a justification
;;; wrld - a world
;;;
;;; returns the justification specified jid if it appears in
;;; wrld. this function searches locally and will not track down
;;; justifications in other worlds.
(defun get-justification (jid wrld)
  (bs-get-justification jid (world-bs wrld)))

;;; get-justifications
;;;
;;; wrld - a world
;;;
;;; returns all justifications in wrld.
(defun get-justifications (wrld)
  (bs-get-all-justifications (world-bs wrld)))

;;; get-justifications
;;;
;;; wrld - a world
;;;
;;; returns all justifications in wrld with the same predicate and
;;; negation status as the literal.
(defun get-lit-justifications (lit wrld)
  (bs-get-justifications lit (world-bs wrld)))

;;; uses-predicate?
;;;
;;; p - predicate name
;;; neg? - negation state
;;; wrld - a world
;;;
;;; returns true if the predicate appears as some belief within the world.
(defun uses-predicate? (p neg? wrld)
  (not (null (get-beliefs-by-predicate p neg? wrld))))


;;; get-beliefs-by-predicate
;;;
;;; p - predicate name
;;; neg? - negation state
;;; wrld - a world
;;;
;;; returns the beliefs that use the given predicate.
(defun get-beliefs-by-predicate (p neg? wrld)
  (if neg? 
      (gethash p (ec-neg-pred (world-elmnts wrld)))
      (gethash p (ec-pos-pred (world-elmnts wrld)))))


;;; believe
;;;
;;; lit - a literal
;;; wrld - a world 
;;; 
;;; creates a belief that contains lit and adds it to wrld. the belief
;;; is given an id and a start time in the process.  
;;;
;;; returns the new belief.
(defun believe (lit wrld &optional (fact nil))
  ;not sure why this causes a break here -CM
;  (when (= (length lit) 1) (break))
  (let ((b (make-belief :content lit :fact fact :start (get-universal-time))))
    (ec-add-element b #'belief-content #'belief-id (world-elmnts wrld))))

;;; get-belief-content
;;;
;;; bid - a belief identifier
;;; wrld - a world
;;;
;;; returns the belief associated with bid if it appears in wrld.
(defun get-belief-content (bid wrld)
  (when bid
    (belief-content (ec-get-element bid (world-elmnts wrld)))))

;;; find-justification
;;;
;;; j - a justification
;;; wrld - a world
;;;
;;; searches for a justification in wrld that is equivalent to j and
;;; returns the result.
(defun find-justification (j wrld)
  (bs-get-justification (find-justification-id j wrld) (world-bs wrld)))

;;; find-subsumed-justification
;;;
;;; j - a justification
;;; wrld - a world
;;;
;;; searches for a justification in wrld that is directly more
;;; specific than j and returns the result.
(defun find-subsumed-justification (j wrld)
  (bs-get-justification (find-subsumed-justification-id j wrld) (world-bs wrld)))

;;; find-justification-id
;;;
;;; j - a justification
;;; wrld - a world
;;;
;;; searches for the identifier of a justification in wrld that is
;;; equivalent to j and returns the result.
(defun find-justification-id (j wrld)
  (bs-find-justification-id j (world-bs wrld)))

;;; find-subsumed-justification-id
;;;
;;; j - a justification
;;; wrld - a world
;;;
;;; searches for the identifier of a justification in wrld that is
;;; directly more specific than j and returns the result.
(defun find-subsumed-justification-id (j wrld)
  (bs-find-subsumed-justification-id j (world-bs wrld)))

;;; register-support
;;;
;;; j - a justification
;;; jw - the intended justification world
;;; b - a belief
;;; bw - the world that contains b
;;;
;;; register j as support for b in jw. guards against duplicating support.
;;; NOTE: turns out that we want to allow duplicate support.
;;(defun register-support (j jw b bw)
  ;; update the justification
;;  (setf (gethash (justification-id j) (bs-supports (world-bs jw)))
;;	(nunion (gethash (justification-id j) (bs-supports (world-bs jw))) (list (list bw b)) :test #'equal))
  ;; update the belief
;;  (if (eql bw jw)
      ;; within-world belief
;;      (setf (gethash (belief-id b) (bs-supported-by (world-bs jw)))
;;	    (nunion (gethash (belief-id b) (bs-supported-by (world-bs jw))) (list j) :test #'eql))
      ;; outside-world belief
;;      (setf (belief-weak-links b)
;;	    (nunion (belief-weak-links b) (list (list jw j)) :test #'equal))))
      
;;; register-support
;;;
;;; j - a justification
;;; jw - the intended justification world
;;; b - a belief
;;; bw - the world that contains b
;;;
;;; register j as support for b in jw.
(defun register-support (j jw b bw)
  ;; update the justification
  (setf (gethash (justification-id j) (bs-supports (world-bs jw)))
	(append (gethash (justification-id j) (bs-supports (world-bs jw))) (list (list bw b))))
  ;; update the belief
  (if (eql bw jw)
      ;; within-world belief
      (setf (gethash (belief-id b) (bs-supported-by (world-bs jw)))
	    (append (gethash (belief-id b) (bs-supported-by (world-bs jw))) (list j)))
      ;; outside-world belief
      (setf (belief-weak-links b)
	    (append (belief-weak-links b) (list (list jw j))))))


;;; unbelieve
;;;
;;; b - a belief
;;; wrld - a world
;;; wm - working memory
;;;
;;; stores that wrld no longer believes b. if wrld is the home to b,
;;; then b is stashed away. otherwise, b is overridden by a local
;;; historical belief.
;;;
;;; does not check for justifications that refer to b either within
;;; wrld or in worlds connected to wrld.
(defun unbelieve (b wrld wm)
  ;; trash all the weak links
  ;; this kills the associated justifications, which may remove other beliefs
  ;; (caveat: i think this is too harsh and that it will lead to forests of 
  ;; assumptions that don't relate to any facts.)
  (dolist (lnk (belief-weak-links b))
    (dolist (b (remove-justification (second lnk) (first lnk)))
      (unbelieve b (second lnk) wm)))
  ;; if wrld isn't b's home, then copy b.
  (unless (eql wrld (belief-home b wm))
    (setq b (make-belief :id (belief-id b) :content (belief-content b)
			 :start (belief-start b))))
  ;; give b (or its copy) an end time and store it in the history.
  (setf (belief-end b) (get-universal-time))
  (setf (gethash (belief-id b) (world-history wrld)) b)
  ;; if wrld is b's home, then remove b from the list of elements.
  (when (eql (belief-home b wm) wrld)
    (ec-remove-element b #'belief-content #'belief-id (world-elmnts wrld))))

;;; fire-justification
;;;
;;; j - a justification that's not yet in this wrld
;;; wrld - a world
;;; wm - working memory
;;;
;;; links beliefs in wrld to the literals in j, creating new beliefs
;;; if necessary. returns j.
;;
;; NOTE: Sometimes b can match multiple positions
;;       in a single justification. Superficially, 
;;       this seems bad, especially if the skolems in
;;       b enable the matches. in the rule
;;          (a sk1 sk2) => (a sk2 sk1)
;;       we should really note that these are different groundings
;;       for now, these might both bind to an existing belief
;;          (a sk3 sk4)
;;       which would lead to uninteresting consequences once
;;       domain constants enter the picture.
(defun fire-justification (j wrld wm)
  (bs-store-justification j (world-bs wrld))
  (dolist (lit (justification-literals j) j)
    (let ((wb (or (find-belief lit wrld wm) 
		  (list (resolve-world lit wrld) 
			(believe (strip-modals lit) (resolve-world lit wrld))))))
      (register-support j wrld (second wb) (first wb))))
  j)

;;; remove-justification
;;;
;;; j - justification
;;; w - the world containing the justification
;;;  
;;; returns ids of beliefs that are no longer supported by any rules
;;; due to the removal of jid.
(defun remove-justification (j w)
  ;; drop the justification from its world
;; (print j)
  ;; XXX: the belief support doesn't know anything about worlds, so we pass it a 
  ;; predicate to let it figure out if a belief is local or not. This should 
  ;; be a temporary patch!!
  ;; beliefs may appear multiple times, but we should only return them once.
  (remove-duplicates (bs-remove-justification (justification-id j) #'(lambda (x) (eq (first x) w)) (world-bs w))))

;;;
;;; takes a literal that's possibly modified by modals and a starting world
;;; returns the world indicated by the modal trail.
;;;
;;; XXX WARNING: only works for the limited goal/intention single-agent case.
(defun resolve-world (lit wrld)
  (cond ((eq 'goal (car lit))
	 (world-goals wrld))
	((eq 'intention (car lit))
	 (world-intentions wrld))
	(t
	 wrld)))
	 

;;; find-belief-id
;;;
;;; finds the identifier for a belief containing the given literal
;;; that is accessible from the current world. follows import and
;;; belief relationships between worlds.
(defun find-belief-id (l wrld wm)
  (let ((b (find-belief l wrld wm)))
    (when b (belief-id b))))

;;; find-belief 
;;;
;;; l - a literal
;;; wrld - a world
;;; wm - working memory
;;;
;;; finds a belief that matches l that is accessible from wrld. checks
;;; within the wrld and within any worlds that wrld observes. assumes
;;; that observed worlds are leaf nodes.  
;;; 
;;; resolves belief modals if necessary.
;;;
;;; this function searches allows beliefs to override each other based
;;; on the import relationships, so believing (not X) in the local
;;; world will overshadow a belief of X in an observed world. however,
;;; more specific beliefs do not overshadow more general ones or vice
;;; versa.
(defun find-belief (l wrld wm)
  (cond ((or (null l) (null wrld)) nil)
	((belief-modal? l)
	 (resolve-belief l wrld wm))
	((goal-modal? l)
	 (fb-local (remove-goal-modal l) (world-goals wrld) nil))
	((intention-modal? l)
	 (fb-local (remove-intention-modal l) (world-intentions wrld) nil))
	(t
	 (find-belief-h l wrld))))

;;; resolve-belief
;;;
;;; walks through nested belief modals. supports literals that refer
;;; to an unknown agent. for example, (belief SK3 X) would search for
;;; X in all worlds that are treated as beliefs of other agents by the
;;; base world, wrld.
(defun resolve-belief (l wrld wm)
  (labels ((fb-rb (l wrlds wm)
	     ;; strip a level of belief on each recursive call.
	     (or (find-belief (third l) (first wrlds) wm)
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
	(find-belief (third l) 
		     (second (find-if #'(lambda (bw) (eq (first bw) (second l)))
				      (world-blf-worlds wrld)))
		     wm))))

;;; I don't know how the system should handle default beliefs (those
;;; inherited from a parent world) at this point. Here's how I would
;;; implement it in resolve-belief.
;;;
;;; check for nil;nil returns. in that case look in the parent world
;;; for the belief (inheritance) otherwise keep returning up the
;;; stack. ignore the case of overridden beliefs for now, because that
;;; requires us to look down from the parent. this approach would work
;;; when the agents are specified, but if they're just skolems, we have
;;; a problem because we need to bind those skolems to actual agents
;;; all the way up the chain.
;;; 
;;; getting default beliefs to work for the case where icarus has
;;; beliefs about other agents but no further recursion occurs, might
;;; be easy, but i'm leaving even that unimplemented at the moment so
;;; that we can move on to the goals/intentions worlds.

;;; find-belief-h
;;; 
;;; helps find-belief by searching through observation worlds.
;;; returns a single belief that matches l if one exists. although
;;; it's possible that matching beliefs will appear in multiple
;;; worlds, only one would be returned.
;;;
;;; returns two values
;;; (1) the belief if found or nil
;;; (2) t if the belief or its negation was found
;;;     nil if the belief does not exist
(defun find-belief-h (l wrld)
  (let (b)
    ;; the belief is overshadowed by its contradiction (woops, breaks things)
    ;; overshadowing only makes sense when we're in a nested agent situation.
    (cond ;; ((fb-local l wrld t) (values nil t))
	  ;; the belief is local
	  ((setq b (fb-local l wrld nil)) (values b t))
	  ;; find the belief in each imported world. if one of these
	  ;; is active in the current world (not in the history)
	  ;; then return it.
	  ((setq b
		 ;; y will be a list: (world belief)
		 (find-if #'(lambda (y) 
			      (and y (not (gethash (belief-id (second y)) (world-history wrld)))))
			  ;; this code assumes that contradictions have been resolved.
			  (mapcar #'(lambda (x) (fb-local l (second x) nil))
				  (world-obs-worlds wrld))))
	   (values b t))
	  ;; the belief isn't in the current world or in any of its observable worlds.
	  (t
	   (values nil nil)))))

;;; finds a belief in wrld that contains the given literal. if
;;; flip-sign? is true, this function looks for the negation of the
;;; literal. returns a list of the form (world belief).
(defun fb-local (l wrld flip-sign?)
  (when flip-sign? (setq l (flip-negation l)))
  ;;(print (ec-get-elements (predicate-name l) (negated? l) (world-elmnts wrld)))
  (let ((b (find-if #'(lambda (b) (literal-equal? (belief-content b) l))
		    (ec-get-elements (predicate-name l) (negated? l) (world-elmnts wrld)))))
    (when b (list wrld b))))


;;;;; FIND BELIEF
;;;
;;; Notes: If a world is monolithic (i.e., it has no links to other
;;; worlds) then finding a belief within it is simply a matter of
;;; searching its local structures. Once you add the ability to import
;;; worlds, the relationships among beliefs gets a little
;;; trickier. 
;;;
;;; There are two relationships we need to handle. The first occurs
;;; when one world imports another as a sensor. The interpretation is
;;; that all the beliefs in the sensor world are available for
;;; explanation in the importing world. The relationship is binary and
;;; does not support nesting. When evaluating candidate
;;; justifications, beliefs in a sensor world get a +1 to their
;;; coherence score because they synchronize with the external world
;;; (i.e., they are treated as facts).
;;;
;;; A local belief can override a sensor belief in one of two
;;; ways. First, the local belief may directly contradict the sensor
;;; belief such as if the sensor reports X and the local world
;;; believes ~X. Second, the local world can report that it stopped
;;; believing in X at a particular time. This act stores a copy of the
;;; sensor belief in the history structure of the local world and
;;; gives it an end time. Subsequent belief in X would require a new
;;; ID and would be treated as a new belief.
;;;
;;; The second relationship is when one world serves as a model of
;;; another agent's beliefs. Within a world, only beliefs local to
;;; that world or imported from a sensor are available for explanation
;;; without using modal operators in the rules. However, suppose that
;;; world A2 represents the beliefs of an agent Jack from the
;;; perspective of another agent Jill whose beliefs exist within world
;;; A1 (i.e., Jill believes that Jack holds the beliefs in A2). When
;;; selecting a justification, Jack will use both the beliefs in A2
;;; and the beliefs in A1, which serve as defaults, during the scoring
;;; process.
;;;
;;; For now, we will not worry about the case where some belief in A1
;;; contradicts a belief in one of A2's sensor worlds. This
;;; circumstance raises a problem if one justification in A2 uses the
;;; belief from A1 and another justification uses the belief from the
;;; sensor world.
;;;
;;; XXX: find-belief needs to return a nested belief so that the
;;; system knows how to bind the variables associated with agents in
;;; the belief modal. This can be tricky. Suppose we have agents A1,
;;; A2, and A3 and that A1 believes that A2 believes that A3 believes
;;; X. Let X exist in A2's world. Although X doesn't exist in A3's
;;; world, A3 inherits the default from A2, so find-belief would
;;; return something like (A1 (A2 (A3 X-ID))) even though X-ID only
;;; exists in A2. What do we do in this case? Are we storing
;;; justifications for beliefs that appear due to modals?
;;;
;;; *** The key question is: When justifications cross worlds, where
;;; do they reside? ***
;;;
;;; Laying aside the scenario where the rules contain nested belief
;;; modals, we're left with the case that a justification that exists
;;; within one world links to a belief in another. We treat this as
;;; normal except that the belief in the other world gets a weak link
;;; to the justification instead of a strong one. That means that the
;;; justification from the perspective of A2 doesn't influence
;;; inference from the perspective of A1.
