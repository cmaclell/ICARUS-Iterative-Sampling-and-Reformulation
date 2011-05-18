;******************************************************************************
;******************************************************************************
;
; MATCHER.LISP - rewritten by Dongkyu Choi, starting on 25NOV2007
;
; ICARUS Matcher: Matches concept definitions to percepts on each cycle
;                 to get satisfied concept instances for the cycle.
;                 Includes a bottom-up fast matcher and a top-down matcher.
;
;******************************************************************************
;******************************************************************************

;(in-package icarus)

;******************************************************************************
; Global Variable Definitions, their Initializations, and Support Functions
;******************************************************************************
(defvar concept-hash-table*)
(defvar concept-name-hash-table*) ; for top-down matching
(defvar matched-concepts*)
(setf matched-concepts* nil)
(defparameter *arithmetic-tests* '(= <= >= < >))

;******************************************************************************
; Structure Definitions and Support Functions
;******************************************************************************

;******************************************************************************
; Main Macros/Functions
;******************************************************************************
; FAST-MATCHER is the top-level function for the bottom-up fast matcher.
; Inputs: concepts ---- concept definitions (long-term structures)
;         perceptions - perceived objects (current perceptual buffer)
;         beliefs ----- matched CINSTANCE's so far (short-term structures)
; Output: matched CINSTANCE's in a list
(defun fast-matcher (concepts perceptions beliefs)
  (initialize-concept-network)
  (let ((ccopy concepts)
	beliefs)
    (do ((concept (car ccopy) (car ccopy)))
        ((null ccopy) beliefs)
	(setq beliefs
	      (union (match-a-concept concept concepts perceptions) beliefs))
	(pop ccopy))))

; January 22, 2005 For Seth's application
; Clear instances field of all concepts, store beliefs in the proper
; place. Even there are concept extensions, it does not matter where
; exactlythese instances in beliefs whould be stored because extensions
; of the same concept are connected by the :siblings field and they
; are one entity to the outside world.
;
; There will be problems if there are instances of dummy concepts in
; beliefs. Dummy concept: concepts without definition in concepts.

(defun fast-matcher1 (concepts perceptions beliefs)
 (let ((ccopy cltm*)
       (ecopy beliefs))
   (do ((concept (car ccopy) (car ccopy)))
       ((null ccopy) (fast-matcher concepts perceptions ecopy))
     (let ((cinstances nil)
           (bcopy beliefs))
       (do ()
           ((null bcopy) (setf (concept-instances concept) cinstances))
         (cond ((equal (caar bcopy) (car (concept-head concept)))
                (push (car bcopy) cinstances)
                (setf beliefs (remove (car bcopy) beliefs :test 'equal))))
         (pop bcopy)))
     (pop ccopy))))

; Top-down matcher
;
; Designate a concept to match, the matcher will recursively match
; supporting lower-level concepts and concept expansions. If the concept
; has been matched already in this cycle, return stored result directly.
;
; Use a global variable to keep track of all concepts that have been
; matched. We need to reset this global variable every cycle.
;
; Notice: Recursive concept definition will impose a problem to top-down
;         matcher.


; cname should be the name of the concept that you want to match,
; such as GOOD-EDGE-TILE.
; Top-down matcher will match all expansions of this concept.

(defun top-down-matcher (cname perceptions)
  (let ((cids (gethash cname concept-name-hash-table*))
        (flag (member cname matched-concepts* :test 'equal))
        result)
    (do ((concept (lookup (car cids)) (lookup (car cids))))
        ((null cids) result)
	(cond (flag (push (concept-instances concept) result))
	      (t (let ((sub-ids (union (concept-pschildren concept)
				       (concept-ngchildren concept))))
		   (do ()
		       ((null sub-ids)
			(push (match-a-concept concept nil perceptions) result)
			(push (car (concept-head concept)) matched-concepts*))
		       (let ((h (concept-head (lookup (car sub-ids)))))
			 (top-down-matcher (car h) perceptions)
			 (push (car h) matched-concepts*)
			 (pop sub-ids))))))
	(pop cids))))

;******************************************************************************
; Functions
;******************************************************************************

(defun initialize-concept-network ()
  (sort-concepts)
  (create-hash-table))

; Sort concepts from simple ones to complex ones
(defun sort-concepts ()
  (let* ((temp (get-mapping-id-unprocessed-heads))
         (mapping-id (car temp))
         (unprocessed-heads (cdr temp))
         (concepts cltm*)
         (ccopy cltm*)
         (result nil))
    (do ((concept (car ccopy) (car ccopy)))
        ((null ccopy)
         (cond ((not (null concepts))
                (print-looped-concepts concepts))) ; Report definition loops
         (setf cltm* (reverse result))
         (create-name-hash-table mapping-id) ; Because we need mapping-id
         ;(set-skill-id mapping-id)          ; Because we need mapping-id     [REWRITE -- this breaks on new skills - is it somehow needed?]
	 )
	(cond ((check-concept concept unprocessed-heads)
	       (setf concepts (remove concept concepts :test 'equal))
	       ; Use :count 1 since there might be expansions of this concept
	       ; Should evaluate all expansions before using this concept
	       (setf unprocessed-heads
		     (remove (car (concept-head concept))
			     unprocessed-heads :count 1 :test 'equal))
	       (push (set-ids concept mapping-id) result)
	       (setf ccopy concepts))
	      (t (pop ccopy))))))

; Get mapping-id and unprocessed-heads
(defun get-mapping-id-unprocessed-heads ()
  (let ((concepts cltm*)
        (mapping-id nil)
        (unprocessed-heads nil))
    (do ((concept (car concepts) (car concepts)))
        ((null concepts) (cons mapping-id unprocessed-heads))
      (let* ((id   (concept-id concept))
             (head (car (concept-head concept)))
             (map  (assoc head mapping-id :test 'equal)))
        (cond ((null map) (push (list head id) mapping-id))
              (t (setf mapping-id
                      (subst (append map (list id)) map
			     mapping-id :test 'equal))))
        (push head unprocessed-heads)
      (pop concepts)))))

(defun print-looped-concepts (concepts)
  (terpri)(princ "ERROR: CONCEPT DEFINITION LOOP")
  (do ()
      ((null concepts) nil)
    (pprint (car concepts))
    (pop concepts)))

(defun create-name-hash-table (mapping-id)
  (setf concept-name-hash-table* nil)
  (setf concept-name-hash-table* (make-hash-table))
  (do ()
      ((null mapping-id) nil)
    (setf (gethash (caar mapping-id) concept-name-hash-table*)
	  (cdar mapping-id))
    (pop mapping-id)))

(defun set-skill-id (mapping-id)
  (let ((temp-sltm sltm*))
    (do ((skill (car temp-sltm) (car temp-sltm)))
        ((null temp-sltm) nil)
      (setf (sclause-startid skill)
	    (set-children (sclause-start skill) mapping-id))
      (setf (sclause-requiresid skill)
	    (set-children (sclause-requires skill) mapping-id))
      (pop temp-sltm))))

; Return t   when the current concept can be added to the sorted pool, which
;            means all concepts in its :positives and :negatives fields are
;            in the sorted pool.
; Return nil if not.
; Do not report dummy concepts here because we might go through the same
; dummy concepts several times.
(defun check-concept (concept unprocessed-heads)
  (let ((sub-heads (union (subconcept-heads (concept-positives concept))
                          (subconcept-heads (concept-negatives concept)))))
    (null (intersection sub-heads unprocessed-heads))))

(defun subconcept-heads (conditions)
  (let (result)
    (do ()
        ((null conditions) result)
      (push (caar conditions) result)
      (pop conditions))))

; Set pschildren, ngchildren, siblings, and rmdup field
(defun set-ids (concept mapping-id)
  (let* ((head      (concept-head      concept))
         (id        (concept-id        concept))
         (positives (concept-positives concept))
         (negatives (concept-negatives concept))
         (temp      (assoc (car head) mapping-id :test 'equal)))
    (setf (concept-siblings concept)
          (cond ((eq id (cadr temp)) (cddr temp))))
    (setf psset (set-children positives mapping-id))
    (setf (concept-pschildren concept) psset)
    (setf (concept-ngchildren concept) (set-children negatives mapping-id))
    (setf (concept-rmdup      concept)
          (let (flag)
            (do ()
                ((or flag (null psset)) flag)
		(setf flag
		      (not
		       (null (member (car psset) (cdr psset) :test 'equal))))
		(pop psset))))
    concept)) ; return concept

; Set pschildren/ngchildren field
(defun set-children (conditions mapping-id)
  (let (result)
    (do ()
        ((null conditions) (reverse result))
	(setf temp (assoc (caar conditions) mapping-id :test 'equal))
	(cond ((null temp)
	       ;(terpri)
	       ;(princ "WARNING: DUMMY CONDITION ")(princ (car conditions))
	       )
	      (t (push (cadr temp) result)))
	(pop conditions))))

(defun create-hash-table ()
  (setf concept-hash-table* nil)
  (setf concept-hash-table* (make-hash-table))
  (let ((ccopy cltm*))
    (do ((concept (car ccopy) (car ccopy)))
        ((null ccopy) nil)
      (setf (gethash (concept-id concept) concept-hash-table*) concept)
      (pop ccopy))))

; Match one concept. Corresponds to get-matches in the original matcher
; concepts is not used in this function, but it is still an input since
; we want to keep the interface the same
(defun match-a-concept (concept concepts perceptions)
  (let* ((percepts    (concept-percepts   concept))
         (positives   (concept-positives  concept))
         (negatives   (concept-negatives  concept))
         (tests       (concept-tests      concept))
         (head        (concept-head       concept))
         (pschildren  (concept-pschildren concept))
         (attributes  (concept-attributes concept))
         (bmatches    (match-positives pschildren positives
				       percepts perceptions nil))
         (psinstances (get-sub-instances pschildren))
         (nginstances (get-sub-instances (concept-ngchildren concept)))
         inferences)
    (do ((bindings (car bmatches) (car bmatches)))
        ((null bmatches) (setf (concept-instances concept) inferences))
	(let* ((r0 (cons (car head) (instantiate-args (cdr head) bindings)))
	       (r1 (cond ((null attributes) r0)
			 (t (let ((acopy attributes))
			      (do ()                                        
				  ((null acopy) r0)                         
				  (setf r0
					(append r0 (compute-pvalues (car acopy)
								    bindings)))
				  (pop acopy)))))))
	  (cond ((and (null (member r1 inferences :test 'equal))
		      (match-tests tests bindings)
		      (cond ((null negatives) t)
			    (t (none-match negatives nginstances bindings))))
		 (push r1 inferences))))
	(pop bmatches))))

; First match percepts with sub-concept (in positive field only) instances,
; then match perceptions. Delete the matched percept instances and perception
; instances. Then match the rest percepts and perceptions with match-pconds.
; Now concept-rmdup is useless, delete it later
(defun match-positives (pschildren positives percepts perceptions binding)
  (cond ((null pschildren)
         (cond ((null percepts) (list binding))
               (t (let (r)
		    (pmatches (match-pconds percepts perceptions nil binding))
		    (do ()
			((null pmatches) r)
			(push (caar pmatches) r)
			(pop pmatches))))))
        (t (let ((sub-instances (get-sub-instances (list (car pschildren))))
                 (result nil))
             (do ((sub-instance (cdar sub-instances) (cdar sub-instances)))
                 ((null sub-instances) result)
		 (setf result
		       (append result
			       (match-sub-instance sub-instance pschildren
						   positives percepts
						   perceptions binding)))
		 (pop sub-instances))))))

; cid-list: list of sub-concept ids
(defun get-sub-instances (cid-list)
  (let (result)
    (do ((cid (car cid-list) (car cid-list)))
        ((null cid-list) result)
	(setf concept (lookup cid))
	(setf result (union result (concept-instances concept)))
	(let ((siblings (concept-siblings concept)))
	  (do ()
	      ((null siblings) nil)
	      (pop siblings)))
	(pop cid-list))))

; match-sub-instance
; flag:  percept name is consistant with the variable in condition, such as
;        :percept (STREET ?STREET) is consistant with (ON-STREET ?SELF ?STREET)
; flag1: perception is consistant with binding
; flag2: sub-instance is consistant with binding
;        for example we have :positives ((ABOVE ?ELM1 ?ELM2) (ABOVE ?ELM2 ?ELM3))
;        then ((ABOVE NUM1 NUM2) (ABOVE NUM3 NUM4)) does not match because of different
;        values for ?ELM2
(defun match-sub-instance (sub-instance pschildren positives
			   percepts perceptions binding)
  (let ((flag2 t)
        (condition (cdar positives)))
    (do ((name  (car condition)    (car condition))
         (value (car sub-instance) (car sub-instance)))
        ((or (null flag2) (null condition))
         (cond (flag2 (match-positives (cdr pschildren) (cdr positives)
				       percepts perceptions binding))))
	(setf b (assoc name binding :test 'equal))
	(cond ((and (null (variablep name)) (null (equal name value)))
	       (setf flag2 nil))
	      ((or (null b) (eq value (cdr b)))
	       (let (p
		     (temp percepts)
		     flag)
		 (cond ((null b) (push (cons name value) binding)))
		 (do ((percept (car temp) (car temp)))
		     ((or flag (null temp)) nil)
		     (setf flag (eq name (cadr percept)))
		     (cond (flag
			    (let (flag1
				  (pe perceptions))
			      (setf percepts (append p (cdr temp)))
			      (do ()
				  ((or flag1 (null pe)) (setf flag2 flag1))
				  (let ((re (pmatches1
					     percept (car pe) binding)))
				    (cond ((not (null (car re)))
					   (setf flag1 t)
					   (setf perceptions
						 (remove (car pe) perceptions
							 :test 'equal))
					   (setf binding
						 (union binding (cdr re))))))
				  (pop pe))))
			   (t (push percept p)))
		     (pop temp))))
	      (t (setf flag2 nil)))
	(pop condition)
	(pop sub-instance))))

; Support arithmetic operations in percept pattern, such as using
;     :percepts ((TILE ?tile x (+ ?rx 1) y (+ ?ry 1)))
; as an alternative for
;     :percepts ((TILE ?tile x ?x        y ?y))
;     :tests    ((= ?x (+ ?rx 1))
;                (= ?y (+ ?ry 1)))
; Percepts should be ordered such that all variables are determined already.
; Need to write a compiler to do the sorting.
;
; pattern <=> one percept
; element <=> one perception
(defun pmatches1 (pattern element bindings)
  (cond ((eq (car pattern) (car element))
         (let* ((pname (cadr pattern))
                (ename (cadr element))
                (nbind (assoc pname bindings))
                (flag t))
           (cond ((null nbind) (push (cons pname ename) bindings))
                 ((not (eq (cdr nbind) ename)) (setq flag nil)))
           (setq pattern (cddr pattern))
           (do ((variable (cadr pattern) (cadr pattern)))
               ((or (null flag) (null pattern)) (cons flag bindings))
	       (let ((value (cadr (member (car pattern) element)))
		     (vbind (assoc variable bindings)))
		 (cond ((atom variable) ; add here, Jiang
			(cond ((null vbind)
			       (push (cons variable value) bindings))
			      ((not (equal (cdr vbind) value))
			       (setq flag nil))))
		       (t (setq flag ; add here, Jiang
				(equal value
				       (eval
					(qsubst-bindings bindings
							 variable)))))))
	       (setq pattern (cddr pattern)))))))

;************************
; standard matcher functions

; *************************************************************************
; Changes to matching functions for use with new code
; *************************************************************************

; Need improved version of INFER that makes multiple passes.

(defun infer (concepts perceptions beliefs)
 (let ((new-inference nil)
       (ccopy concepts))
   (do ((concept (car concepts) (car concepts)))
       ((null concepts)
        (cond ((null new-inference) beliefs)
              (t (infer ccopy perceptions beliefs))))
       (let ((inferences (get-matches concept perceptions beliefs)))
         (do ((inference (car inferences) (car inferences)))
             ((null inferences) nil)
;TLP_EXP
             (let ((analog (member inference beliefs :test 'same-cinstance)))
;             (let ((analog (member inference beliefs :test 'equal)))
               (cond ((null analog)
                      (setq new-inference t)
                      (push inference beliefs))
                     (t nil)))
             (pop inferences)))
       (pop concepts))))

;(defun fast-matcher (x y z)
; (infer x y z))

(defun match-pcond (condition clist elist ematched bindings)
 (let ((ecopy elist)
       (results nil))
;We shouldn't remove numeric bindings here. But...
;       (bindings (remove-numeric-bindings bindings)))
   (do ((enext (car elist) (car elist)))
       ((null elist) results)
       (cond ((not (member enext ematched :test 'equal))
              (let ((pnext (pmatches condition enext bindings)))
                (cond ((not (null (car pnext)))
                       (setq results
                             (append (match-pconds clist ecopy
                                           (cons enext ematched) (cdr pnext))
                                     results)))))))
       (pop elist))))

(defun remove-numeric-bindings (id bindings)
  (let ((sclause (car (member id sltm* :key #'sclause-id :test 'equal))))
    (if (not (null sclause))
	(let ((percepts (mapcar #'cddr
				(copy-tree (sclause-percepts sclause))))
	      (degmatch (sclause-degmatch sclause)))
	  (setq bindings
		(remove-if #'(lambda (binding)
			       (member (car binding) degmatch :test 'equal))
			   bindings))
	  (dotimes (i (length percepts) bindings)
	    (setq bindings
		  (remove-if #'(lambda (binding)
				 (member binding (nth i percepts)
					 :test 'equal))
			     bindings)))))))

;TLP_EXP
; GET-MATCHES that uses CINSTANCE structures with DEGMATCH attributes
(defun get-matches (concept perceptions beliefs)
  (let* ((head (concept-head concept))
; removing the changes for revision 170 - see the svn log for revision 213
;	 (head-bindings (remove-if
;			 #'null
;			 (mapcar
;			  #'(lambda (belief)
;			      (let ((head-matches (bmatches head belief nil)))
;				(if (car head-matches) (cdr head-matches))))
;			  beliefs)))
	 (percepts (concept-percepts concept))
	 (positives (concept-positives concept))
	 (negatives (concept-negatives concept))
	 (tests (concept-tests concept))
	 (id (concept-id concept))
	 (pivot (concept-pivot concept))
	 (threshold (concept-threshold concept))
	 (pmatches
;	  (if head-bindings
;	      (mapcan #'(lambda (head-binding)
;			  (match-pconds percepts perceptions
;					nil head-binding))
;		      head-bindings)
;	      (match-pconds percepts perceptions nil nil)))
	  (match-pconds percepts perceptions nil nil))
	 (inferences (if (equal inference* 'fast)
			 (concept-instances concept))))
    ;(show "PMATCHES: " pmatches debug*)
    (do ((bmatches (match-bconds positives beliefs nil (caar pmatches))
		   (match-bconds positives beliefs nil (caar pmatches))))
	((null pmatches)
	 (if (equal inference* 'fast)
	     (setf (concept-instances concept) inferences)
	     inferences))
      (do ((bindings (caar bmatches) (caar bmatches)))
          ((null bmatches) nil)
        (let ((belief-head (subst-bindings bindings head)))
          (cond
	    ((and (equal inference* 'fast)
		  (member belief-head inferences
			  :key #'cinstance-head :test #'equal))
	     t)
	    ((and (null pivot)
		  (match-tests tests bindings)
		  (cond ((null negatives) t)
			(t (none-match negatives beliefs bindings))))
	     (cond ((equal inference* 'fast)
		    (push (make-cinstance
			   :head belief-head
			   :id id
			   :bindings bindings
			   :pos-dependencies
			   (get-pos-sub-heads concept bindings)
			   :neg-dependencies
			   (get-neg-sub-heads concept bindings)
			   :percepts (get-percepts concept bindings))
			  inferences)
		    (get-all-percepts (first inferences)))
		   (t
		    (push (make-cinstance
			   :head belief-head
			   :id id
			   :bindings bindings)
;			   :time-stamps (cons cycle* 'NOW))
			  inferences))))
	    ((and pivot
		  (cond ((null negatives) t)
			(t (none-match negatives beliefs bindings))))
	     (let ((degmatch (match-partial-tests tests bindings pivot))
		   (ihead (subst-bindings bindings head))
		   (args (instantiate-args (cdr head) bindings)))
	       (unless (or (not (numberp degmatch))
			   (and (numberp (car threshold))
			   ; for now assume degmatch is not a list
			   ; and threshold is a list. This will change soon.
				(< (abs degmatch) (car threshold))))
;		       (terpri)(princ "DEGMATCH: ")(princ degmatch)
;		       (princ "THRESHOLD: ")(princ (car threshold))
		 (push (make-cinstance
			:head ihead
			:id id
			:degmatch degmatch
			:bindings bindings)
		       inferences))))))
        (pop bmatches))
      (pop pmatches))))

; NONE-MATCH inputs a set of (negated) conditions, a set of elements 
; from conceptual short-term memory, and a set of bindings. It returns
; NIL if any ONE of the conditions match successfully and T otherwise. 
; See NMATCHES below for details about how it treats variables. 

(defun none-match (clist elist bindings)
  (cond ((null clist) t)
	((match-ncond (car clist) elist bindings) nil)
	(t (none-match (cdr clist) elist bindings))))

; MATCH-NCOND inputs a condition, a a set of elements from conceptual
; short-term memory, and a set of bindings. It returns NIL if the 
; condition matches any one of the elements given the bindings. 
; See NMATCHES below for details about how it treats variables. 

(defun match-ncond (condition elist bindings)
  (do ((enext (car elist) (car elist)))
      ((null elist) nil)
      (cond ((car (nmatches condition enext bindings))
	     (return t)))
      (pop elist)))

;TLP_EXP
; NMATCHES inputs a condition (CLIST), an element (ELIST), and a set of 
; bindings. It returns a list of the form (T <bindings>) if the condition 
; matches the element and a list of the form (NIL <bindings>) otherwise. 
; NOTE: An unbound variable may NOT bind to any symbol that is already
; bound to some other variable. 

(defun nmatches (clist elist bindings)
  (cond
   ;If concept names don't match, return NIL.
   ((not (equal (car clist) (cinstance-name elist)))
    (cons nil bindings))
   ;If concept names match, proceed with arguments.
   (t
    (let ((flag t)
	  (cargs (cdr clist))
	  (eargs (cinstance-args elist)))
      (do ((c (car cargs) (car cargs))
	   (e (car eargs) (car eargs))
	   (b nil nil))
	  ((or (null flag) (null cargs))
	   (cons flag bindings))
	  (cond ((not (variablep c))
		 (cond ((not (equal c e))(setq flag nil))))
		((setq b (assoc c bindings))
		 (cond ((not (equal (cdr b) e))
			(setq flag nil))))
		((rassoc e bindings)
		 (setq flag nil))
		(t (push (cons c e) bindings)))
	  (pop cargs)
	  (pop eargs))))))

#| ; Original NMATCHES
; NMATCHES inputs a condition (CLIST), an element (ELIST), and a set of 
; bindings. It returns a list of the form (T <bindings>) if the condition 
; matches the element and a list of the form (NIL <bindings>) otherwise. 
; NOTE: An unbound variable may NOT bind to any symbol that is already
; bound to some other variable. 

(defun nmatches (clist elist bindings)
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
		       ; Now it allows the same number
		       ; to bind to other variables.
		       ; It will be nice if we could check if the number
		       ; is actually an object or not, but this will be
		       ; fine for now.
		       ((and (rassoc e bindings)
			     (not (numberp e)))
			(setq flag nil))
		       (t (push (cons c e) bindings)))
		 (pop clist)
		 (pop elist))))))
|#

; MATCH-PCONDS finds all matches between the perceptual conditions in CLIST
; and the Boolean perceptual in ELIST, with EMATCHED and BINDINGS holding 
; the elements and bindings collected so far. It returns a list of lists,
; with the car of each component being a list of variable bindings and 
; the cdr being the matched elements. 

(defun match-pconds (clist elist ematched bindings)
  (cond ((null clist)(list (cons bindings ematched)))
        ((not (null elist))
         (match-pcond (car clist) (cdr clist) elist ematched bindings))))

; PMATCHES inputs a perceptual pattern, a specific perceptual object, and
; a set of bindings (cast as a list of dotted pairs). It returns a list of
; the form (T ((?X . A)(?Y . B))) if the pattern matches and is consistent 
; with the initial bindings and returns a list of the form (NIL ((?X . A)))
; if the match fails or the bindings are inconsistent. 

(defun pmatches (pattern element bindings)
  (cond ((eq (car pattern) (car element))
	 (let* ((pname (cadr pattern))
		(ename (cadr element))
		(nbind (assoc pname bindings))
		(flag t))
	   (cond ((null nbind)
		  ;if the pattern is a variable
		  (if (variablep pname)
		      (push (cons pname ename) bindings)
		    ;if the pattern is a constant
		    ;and the perceptual name is the same as that
		    (if (equal pname ename)
			(push (cons pname ename) bindings)
		      ;if the pattern is a constant
		      ;and the perceptual name does not match
		      (setq flag nil))))
		 ((not (eq (cdr nbind) ename))
		  (setq flag nil)))
	   (setq pattern (cddr pattern))
	   (do ((variable (cadr pattern) (cadr pattern)))
	       ((or (null flag) (null pattern))
		(cons flag bindings))
	       (let ((value (cadr (member (car pattern) element)))
		     (vbind (assoc variable bindings)))
		 (cond ((null vbind)
			;if the pattern is a variable
			(if (variablep variable)
			    (push (cons variable value) bindings)
			  ;if the pattern is a constant
			  ;and the perceptual name is the same as that
			  (if (equal variable value)
			      (push (cons variable value) bindings)
			    ;if the pattern is a constant
			    ;and the perceptual name does not match
			    (setq flag nil))))
		       ((not (equal (cdr vbind) value))
			(setq flag nil))))
	       (setq pattern (cddr pattern)))))))

;TLP_EXP
; MATCH-DEGMATCH finds all matches between CLIST and ELIST, and add a special
; degree of match binding to BINDINGS, using the variable specified in DEGMATCH.
(defun match-degmatch (clist elist degmatch bindings)
  (cond ((null clist) (list bindings))
	((not (null elist))
	 (mapcar
	  #'(lambda (onematch)
	      (let ((onebindingset (car onematch))
		    (conditions (reverse (cdr onematch))))
		(append
		 onebindingset
		 (remove-if
		  #'null
		  (mapcar #'(lambda (onecond degvar)
			      (unless (null degvar)
				(cons degvar (cinstance-degmatch onecond))))
			  conditions degmatch)))))
	  (match-bconds clist elist nil bindings nil)))))

; MATCH-BCONDS finds all matches between the Boolean conditions in CLIST
; and the Boolean elements in ELIST, with EMATCHED and BINDINGS holding 
; the elements and bindings collected so far. It returns a list of lists,
; with the car of each component being a list of variable bindings and 
; the cdr being the matched elements. 
;  clist:     conditions (concepts) to match
;  elist:     concept instances that are true in the current state
;  ematched:  conditions that are matched so far
;  bindings:  bindings used in the conditions matched so far
;  novel:     switch for novel instance check
(defun match-bconds (clist elist ematched bindings &optional (novel nil))
  (cond ((null clist) (list (cons bindings ematched)))
; ((testp (caar clist))
; (match-test (car clist) (cdr clist) elist ematched bindings))
	((not (null elist))
	 (let (positives negatives results)
	   (mapcar #'(lambda (condition)
		       (cond ((equal (car condition) 'NOT)
			      (push condition negatives))
			     (t
			      (push condition positives))))
		   clist)
	   (setq results
		 (match-bconds-aux positives elist ematched bindings novel))
	   (setq results
		 (remove-if-not #'(lambda (result)
				    (none-match negatives elist (car result)))
				results))
	   results))))

(defun match-bconds-aux (clist elist ematched bindings novel)
  (cond ((null clist) (list (cons bindings ematched)))
	((not (null elist))
	 (match-bcond (car clist) (cdr clist) elist ematched bindings novel))))

; MATCH-BCOND is called by MATCH-BCONDS (which it calls recursively) to
; handle matching for a single condition. 
;  condition: a condition (a concept) to match
;  clist:     rest of conditions (concepts) to match
;  elist:     concept instances that are true in the current state
;  ematched:  conditions that are matched so far
;  bindings:  bindings used in the conditions matched so far
;  novel:     switch for novel instance check
(defun match-bcond (condition clist elist ematched bindings
			      &optional (novel nil))
  (let ((ecopy elist)
	(results nil))
    (do ((enext (car elist) (car elist)))
	((null elist) results)
      (cond ((cinstance-p enext))
	    (t
	     (setq enext (make-cinstance :head enext))))
      (let ((bnext (bmatches condition enext bindings novel)))
	(cond ((not (null (car bnext)))
	       (setq results
		     (append (match-bconds-aux clist ecopy 
					       (cons enext ematched) (cdr bnext)
					       novel)
			     results)))))
      (pop elist))))

; TLP_EXP
; BMATCHES checks if clist matches elist and returns (FLAG . BINDINGS).
;  clist:    a concept head to match
;  elist:    a concept instance that is true in the current state
;  bindings: bindings collected so far
;  novel:    novel instance checker
(defun bmatches (clist elist bindings &optional (novel nil) (identity nil))
  (let ((timecheck (time-stamped clist))
	(orig-bindings (copy-list bindings))
	head start end)
    (cond
      ; if clist is not time-stamped
      ((null timecheck)
       (setq head clist)
       (setq end 'NOW)) ; needs a fix pending Gary's response -DKC
      ; if clist is time-stamped
      (t
       (setq head (car clist))
       (setq start (second clist))
       (setq end (third clist))))
    (cond
      ; if concept names don't match
      ((not (equal (car head) (cinstance-name elist)))
       (cons nil bindings))
      ; if concept names match
      (t
       (let* ((degmatch (cinstance-degmatch elist))
	      (flag (if (and (numberp degmatch)
			     (< degmatch 1.0))
			degmatch t))
	      (cargs (append (cdr head) (list start end)))
	      (eargs (append (cinstance-args elist)
			     (list (start-time elist) (end-time elist)))))
	 (do ((c (car cargs) (car cargs))
	      (e (car eargs) (car eargs))
	      (b nil nil))
	     ((or (null flag) (null cargs))
	      (if (null flag)
		  (cons flag orig-bindings)
		  (cons flag bindings)))
	   (cond
	     ; if c is a space-filler (null)
	     ((null c))
	     ; if c is a constant
	     ((not (variablep c))
	      (cond ((not (equal c e)) (setq flag nil))
		    (t
		     (if novel
			 (push (cons c e) bindings)))))
	     ; if c is a variable and it exists in bindings
	     ((setq b (assoc c bindings))
	      (cond ((not (equal (cdr b) e))
		     (setq flag nil))))
	     ; When IDENTITY is nil, do not add the binding
	     ; if c and e are the same variables.
	     ; If they are all variables but different, the binding is
	     ; added for unification later.
	     ((and (variablep e)
		   (equal c e))
	      (if identity (push (cons c e) bindings)))
	     (t (push (cons c e) bindings)))
	   (pop cargs)
	   (pop eargs)))))))

#| ; Original BMATCHES
(defun bmatches (clist elist bindings &optional (novel nil))
  (cond ((not (eq (length clist) (length elist)))
	 (cons nil bindings))
	(t (let ((flag t))
	     (do ((c (car clist) (car clist))
		  (e (car elist) (car elist))
		  (b nil nil))
		 ((or (null flag) (null clist))
		  (cons flag bindings))
		 (cond ((not (variablep c))
			(cond ((not (equal c e)) (setq flag nil))
			      (t
			       (if (not (null novel))
				   (push (cons c e) bindings)))))
		       ((setq b (assoc c bindings))
			(cond ((not (equal (cdr b) e))
			       (setq flag nil))))
		       (t (push (cons c e) bindings)))
		 (pop clist)
		 (pop elist))))))
|#

(defun match-tests (clist bindings)
  (cond ((null clist) t)
	((eval (qsubst-bindings bindings (car clist)))
	 (match-tests (cdr clist) bindings))))

;may want to put a MATCH-TESTS before starting the partial matching process
;to avoid complicated partial matching if the concept is absolutely true.
(defun match-partial-tests (clist bindings pivot)
  (cond ((null clist) t)
	(t
	 (let (symbolics arithmetics)
	   (mapcar #'(lambda (condition)
		       (cond ((and (member (car condition)
					   *arithmetic-tests* :test 'equal)
				   (intersection pivot condition))
			      (push condition arithmetics))
			     (t
			      (push condition symbolics))))
		   clist)
	   (cond ((match-tests (reverse symbolics) bindings)
		  (get-degree-of-match (reverse arithmetics)
				       bindings pivot)))))))

;dummy Gaussian
(defun gaussian (var center &optional (type 'ALL))
  (cond ((equal type 'ALL)
	 (cond ((< var (- center 1)) 0.0)
	       ((< var center) (- var (- center 1)))
	       ((< var (+ center 1)) (- (+ center 1) var))
	       (t 0.0)))
	((equal type 'LEFT)
	 (cond ((< var (- center 1)) 0.0)
	       ((< var center) (- var (- center 1)))
	       (t 1.0)))
	((equal type 'RIGHT)
	 (cond ((< var center) 1.0)
	       ((< var (+ center 1)) (- (+ center 1) var))
	       (t 0.0)))))

;assumes there is only one pivot in each condition
(defun get-degree-of-match (arithmetics bindings pivot)
;  (terpri)(princ "bindings: ")(princ bindings)
  (let (degmatch)
    (mapcar #'(lambda (condition)
		(let* ((var   (car (intersection pivot condition)))
		       (value (cdr (assoc var bindings)))
		       (bound (subst-bindings
			       (remove-if #'(lambda (binding)
					      (equal (car binding) var))
					  bindings)
			       condition))
		       center flag)
		  (cond ((equal var (second bound))
			 (setq center (third bound))
			 (setq flag 'SECOND))
			((equal var (third bound))
			 (setq center (second bound))
			 (setq flag 'THIRD)))
;		  (terpri)(princ "condition: ")(princ condition)
;		  (terpri)(princ "var: ")(princ var)
;		  (terpri)(princ "value: ")(princ value)
;		  (terpri)(princ "bound: ")(princ bound)
;		  (terpri)(princ "center: ")(princ center)
;		  (terpri)(princ "flag: ")(princ flag)
		  (cond ((null center) (push 0.0 degmatch))
			((equal (car condition) '=)
			 (push (gaussian value center) degmatch))
			((or (equal (car condition) '<=)
			     (equal (car condition) '<))
			 (push
			  (gaussian value center
				    (if (equal flag 'SECOND) 'RIGHT 'LEFT))
			  degmatch))
			((or (equal (car condition) '>=)
			     (equal (car condition) '>))
			 (push
			  (gaussian value center
				    (if (equal flag 'SECOND) 'LEFT 'RIGHT))
			  degmatch)))))
	    arithmetics)
    (/ (apply #'+ degmatch) (length degmatch))))

(defun instantiate-args (args bindings)
  (cond ((null args) nil)
	((or (numberp (car args)) (not (variablep (car args))))
	 (cons (car args) (instantiate-args (cdr args) bindings)))
	(t (cons (cdr (assoc (car args) bindings))
		 (instantiate-args (cdr args) bindings)))))

(defun neq (x y) (not (eq x y)))

; SUBST-BINDING inputs a list of variable bindings, like ((?X . A)(?Y . B)),
; and a generic skill or action, like (STACK ?X ?Y), and substitutes the 
; bindings for variables to produce a specific result, like (STACK A B).  

(defun subst-bindings (bindings generic)
  (cond ((null bindings) generic)
	(t (subst-bindings (cdr bindings)
			   (subst (cdar bindings) (caar bindings) generic)))))

; QSUBST-BINDINGS is just like SUBST-BINDINGS except that it embeds the
; substituted value in quotes for later evaluation. 

(defun qsubst-bindings (bindings generic)
  (cond ((null bindings) generic)
	(t (qsubst-bindings (cdr bindings)
			    (subst (list 'quote (cdar bindings))
				   (caar bindings) generic)))))

; RSUBST-BINDINGS is just like SUBST-BINDINGS except that it embeds both
; the substituted value and the constants in quotes for later evaluation.
(defun rsubst-bindings (bindings generic)
  (cond ((null bindings) generic)
	(t
	 (let ((source (caar bindings))
	       target)
	   (if (or (numberp (cdar bindings))
		   (not (variablep (caar bindings))))
	       (setq target (cdar bindings))
	       (setq target (list 'quote (cdar bindings))))
	   (rsubst-bindings (cdr bindings)
			    (subst target source generic))))))

; UNSUBST-BINDINGS is just like SUBST-BINDINGS except that it replaces 
; variables with constants, rather than the reverse. 

(defun unsubst-bindings (bindings generic)
  (cond ((null bindings) generic)
        (t (unsubst-bindings (cdr bindings)
	            (subst (caar bindings) (cdar bindings) generic)))))

(defun combine-bindings (bindings1 bindings2)
  (cond ((null bindings1) bindings2)
	((assoc (caar bindings1) bindings2)
	 (combine-bindings (cdr bindings1) bindings2))
	(t (combine-bindings (cdr bindings1)
			     (cons (car bindings1) bindings2)))))

; CASSOC returns the first expansion in CONCEPTS that has NAME as 
; the first symbol in its :HEAD field. 
(defun cassoc (name concepts)
  (let ((result nil))
    (do ((next (car concepts) (car concepts)))
        ((or (not (null result)) (null concepts)) result)
        (cond ((eq (car (concept-head next)) name)
               (setq result next)))
        (pop concepts))))

; GENERATE-BINDINGS inputs a list of concept and skill instances, along
; with a list of variable bindings, and returns an extended list that
; includes a unique variable for each distinct argument. The second 
; argument (BINDINGS) is initially NIL. 

(defun generate-bindings (elements bindings)
  (cond ((null elements) nil)
        (t (generate-bindings-one
	    (car elements)
	    (generate-bindings (cdr elements) bindings)))))


; New version of GENERATE-BINDINGS-ONE that handles negated literals. 

(defun generate-bindings-one (element bindings)
  (cond ((eq (car element) 'not)
	 (setq element (cdadr element)))
	(t (setq element (cdr element))))
  (do ((next (car element) (car element)))
      ((null element) bindings)
      (cond ((variablep next) nil)
	    ((not (assoc next bindings))
	     (push (cons next (gen-svar next)) bindings)))
      (pop element)))

; GEN-SVAR generates a variable by putting ? in front of its argument X, 
; whether X is a symbol or a number. 

(defun gen-svar (x)
  (cond ((numberp x)
         (intern (concatenate 'string "?" (princ-to-string x))))
        (t (intern (concatenate 'string "?" (string x))))))

(defun match-heads (clist elist bindings)
  (cond ((null clist) (if (null bindings) (list t) (list t bindings)))
        ((not (null elist))
	 (cond ((eq (caar clist) '*neg*)
		(match-neg-cond (cadar clist) (cdr clist) elist bindings))
	       (t (match-pos-cond (car clist) (cdr clist) elist bindings))))))

(defun match-pos-cond (condition clist elist bindings)
  (unless (cinstance-p condition)
    (setq condition (make-cinstance :head condition)))
  (let ((ecopy elist)
        results
	flag)
    (do ((enext (car elist) (car elist)))
        ((null elist) (if (null flag) flag (cons flag results)))
      (let ((bnext (bmatches enext condition bindings)))
	(cond ((not (null (car bnext)))
	       (let ((others
		      (match-heads clist ecopy (cdr bnext))))
		 (setq flag (car others))
		 (setq results
		       (append results (cdr others)))))))
      (pop elist))))

(defun match-neg-cond (condition clist elist bindings)
  (unless (cinstance-p condition)
    (setq condition (make-cinstance :head condition)))
  (let ((ecopy elist)
        results
	flag
	bnext)
    (do ((enext (car elist) (car elist)))
        ((null elist) (if (null flag) flag (cons flag results)))
      (cond ((eq (car enext) '*neg*)
	     (setq bnext (bmatches (cadr enext) condition bindings))
	     (cond ((not (null (car bnext)))
		    (let ((others
			   (match-heads clist ecopy (cdr bnext))))
		      (setq flag (car others))
		      (setq results
			    (append results (cdr others))))))))
      (pop elist))))