;; when true, candidates will be filtered so that they respect temporal constraints.
;; the filter is turned on when the temporal-filter code is loaded.
(defvar *temporal-filter* nil)

(defconstant +system-name+ 'icarus)

(defvar wm)
(defvar kb)

(defun initialize ()
  (reset-skolem-id)
  (reset-belief-id)
  (reset-justification-id)
  (reset-world-id)

  (setf kb (make-kb))
  (setf wm (create-wm))
  ;; set up self belief
  (believe-world 'self (wm-prime wm) (wm-prime wm))
   (make-goal-worlds 'self  wm))

;; the name is a mnemonic for the sensor. the created world still gets
;; a unique id.
(defun add-sensor (name wm)
  ;; register the world with working memory and have the primary
  ;; system world import it.
  (let ((w (make-world :id name)))
    (observe-world name 
		  (setf (gethash (world-id w) (wm-worlds wm)) w)
		  (wm-prime wm))))

;; creates an observation world and an agent belief world. links the
;; worlds to the primary system world.
(defun add-agent (name wm)
  (let ((sw (make-world))
	(aw (make-world)))
    ;; register the world with working memory and have the primary
    ;; system world believe what the agent reports.
    ;;(import-world name 
    ;;		  (setf (gethash (world-id sw) (wm-worlds wm)) sw)
    ;;		  (wm-prime wm))
    ;; register the agent world with working memory and have the
    ;; primary system world believe that agent believes whatever is in
    ;; its world.
    (believe-world name 
		   (setf (gethash (world-id aw) (wm-worlds wm)) aw)
		   (wm-prime wm))
    ;; the agent believes what it reports
    (observe-world name sw aw)
    ;; the agent believes what it believes
    (believe-world 'self aw aw)
    ;; naively, the system believes that the agent believes what the
    ;; system believes. *this is untrue, because the system may have
    ;; unique access to sensors, so the agent better not import the
    ;; system's beliefs.*
    ;;
    ;; (import-world +system-name+ (wm-prime wm) aw)
    ;;
    ;; the agent believes that the system believes whatever is in its
    ;; world.  this is fair because we assume that the agent tells the
    ;; system about all its important sensory information.
    (believe-world +system-name+ (wm-prime wm) aw)))


(defun make-goal-worlds (name wm)
  (let ((g (make-world :id (symb name ".GOAL")))
	(i (make-world :id (symb name ".INTENTIONS"))))

    (setf (gethash (world-id i) (wm-worlds wm)) i)
    (setf (gethash (world-id g) (wm-worlds wm)) g)

    (setf (world-goals (gethash name (wm-worlds wm))) g)
    (setf (world-intentions (gethash name (wm-worlds wm))) i)))


(defun test-one-init (rules example)
  (initialize)
  (add-sensor 'story wm)
  (dolist (r rules) (store-rule r kb))
  (add-facts example 'story wm)
  (output-dot wm))

(defun test-one-print ()
 (format t "~%Observations:~%")
  (dolist (i (get-observed-beliefs (wm-prime wm)))
    (format t "~T~A~%" (second i)))

  (format t "Assumptions:~%")
  (dolist (i (get-local-beliefs (wm-prime wm)))
    (format t "~T~A~%" (second i)))

  (format t "Goals:~%")
  (dolist (i (get-goals (wm-prime wm)))
    (format t "~T~A~%" (second i)))

  (format t "Intentions:~%")
  (dolist (i (get-intentions (wm-prime wm)))
    (format t "~T~A~%" (second i)))
  
  (dolist (i (get-justifications (wm-prime wm)))
    (format t "~%~%~A supports: ~A" (justification-id i) 
	    (sort (mapcar #'(lambda (x) (belief-id (second x))) (get-supported-beliefs i (wm-prime wm)))
		  #'string< :key #'symbol-name))
        (pprint  i))
  (output-dot wm :show-worlds? nil))


(defparameter *a1* '((A) (B)))
(defparameter *simple-rules* '(
(=> (and (B) (C))
    (A))
	))


(defun return-assumptions ()
  (let ((assumptions nil))
    (dolist (i (get-local-beliefs (wm-prime wm)))
      (push (belief-content (second i)) assumptions))
    assumptions))

(defun test-fixed-point (example rules &optional (pb #'pick-belief-fewrules-unattached) (d? nil) (print? t))
  (test-one-init rules example)
  (let ((assumptions nil))
    (loop
       (dotimes (i 10) (abra-infer wm kb
	      :bc-only? nil
	      :focused? nil :p -1
	      :pick-belief pb
	      :deduction? d?))
       (when (equal assumptions (get-local-beliefs (wm-prime wm))) (return))
       (setf assumptions (copy-list (get-local-beliefs (wm-prime wm))))))
  (when print? (return-assumptions)))

(defun test-one (steps example rules &optional (pb #'pick-belief-fewrules-unattached) (d? nil) (print? t))
  (test-one-init rules example)
  (dotimes (i steps) (abra-infer wm kb
			    :bc-only? nil
			    :focused? nil :p -1
			    :pick-belief pb
			    :deduction? d?))
  (when print? (test-one-print)))

(defun inc-test-one (steps example rules &optional (plot? nil) (pb #'pick-belief-fewrules-unattached) (d? nil))
  (test-one-init rules (list (first example)))
  (dotimes (i steps) (abra-infer wm kb :bc-only? nil
			    :focused? nil :p -1
			    :pick-belief pb
			    :deduction? d?))
  
  (dolist (f (rest example))
    (add-facts (list f) 'story wm)
    (dotimes (i steps) (abra-infer wm kb :bc-only? nil
			    :focused? nil :p -1
			    :pick-belief pb
			    :deduction? d?))
    (when plot?
      (output-dot wm)
      (sleep 1))))

(defun continue-one (steps facts &optional (pb #'pick-belief-fewrules-unattached) (d? nil))
  (add-facts facts 'story wm)
  (dotimes (i steps) (abra-infer wm kb 
			    :bc-only? nil
			    :focused? nil
			    :p -1 
			    :pick-belief pb
			    :deduction? d?)))

(defun next-cycle-one (blf-id &optional (d? nil))
  (labels ((pb (a b &key world)
	     (declare (ignore a b world))
	     (find blf-id (get-agent-beliefs (wm-prime wm)) 
		   :key #'(lambda (x) (belief-id (second x))))))
    (abra-infer wm kb 
	   :bc-only? nil
	   :focused? nil
	   :p -1 
	   :pick-belief #'pb
	   :deduction? d?))
  (test-one-print))


(defun test-ng (steps example sorts? &optional (pb #'pick-belief-fewrules-unattached))
  ;; not a good idea to split in this rule set.
  (setf *divide-lookahead?* nil)
  (if sorts?
      (test-one steps example (append *shopping-rules* *sort-hierarchy*) pb)
      (test-one steps example *shopping-rules* pb)))

(defun inc-test-ng (steps example sorts? &optional (plot? nil) (pb #'pick-belief-fewrules-unattached))
  ;; not a good idea to split in this rule set.
  (setf *divide-lookahead?* nil)
  (if sorts?
      (inc-test-one steps example (append *shopping-rules* *sort-hierarchy*) plot? pb)
      (inc-test-one steps example *shopping-rules* plot? pb)))

;;;;
;; (test-one 0 *e6* *shopping-rules*)
;; (next-cycle-one 'B2)
;; (next-cycle-one 'B12)
