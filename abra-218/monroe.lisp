(defvar *monroe-kb*)
(setf *monroe-kb* '(
(=> (AND (SHELTER-LEADER ?LEADER)
         (NOT (ASSIGNED-TO-SHELTER ?LEADER ?OTHER-SHELTER)) (FOOD ?FOOD)
         (GET-ELECTRICITY ?LOC) (GET-TO ?LEADER ?LOC) (GET-TO ?FOOD ?LOC)) (SET-UP-SHELTER
                                                                            ?LOC))
(=> (AND (SHUT-OFF-WATER ?FROM ?TO) (REPAIR-PIPE ?FROM ?TO)
         (TURN-ON-WATER ?FROM ?TO)) (FIX-WATER-MAIN ?FROM ?TO))
(=> (AND (BLOCK-ROAD ?FROM ?TO) (CLEAN-UP-HAZARD ?FROM ?TO)
         (UNBLOCK-ROAD ?FROM ?TO)) (CLEAR-ROAD-HAZARD ?FROM ?TO))
(=> (AND (SET-UP-CONES ?FROM ?TO) (CLEAR-WRECK ?FROM ?TO)
         (TAKE-DOWN-CONES ?FROM ?TO)) (CLEAR-ROAD-WRECK ?FROM ?TO))
(=> (AND (TREE-BLOCKING-ROAD ?FROM ?TO ?TREE) (SET-UP-CONES ?FROM ?TO)
         (CLEAR-TREE ?TREE) (TAKE-DOWN-CONES ?FROM ?TO)) (CLEAR-ROAD-TREE
                                                          ?FROM ?TO))
(=> (AND (ROAD-SNOWY ?FROM ?TO) (SNOWPLOW ?PLOW) (ATLOC ?PLOW ?PLOWLOC)
         (PLOWDRIVER ?DRIVER) (GET-TO ?DRIVER ?PLOWLOC)
         (!NAVEGATE-SNOWPLOW ?DRIVER ?PLOW ?FROM)
         (!ENGAGE-PLOW ?DRIVER ?PLOW)
         (!NAVEGATE-SNOWPLOW ?DRIVER ?PLOW ?TO)
         (!DISENGAGE-PLOW ?DRIVER ?PLOW)) (PLOW-ROAD ?FROM ?TO))
(=> (AND (IN-TOWN ?LOC ?TOWN) (POLICE-UNIT ?P1) (POLICE-UNIT ?P2)
         (NOT (EQUAL ?P1 ?P2)) (DECLARE-CURFEW ?TOWN) (GET-TO ?P1 ?LOC)
         (GET-TO ?P2 ?LOC) (!SET-UP-BARRICADES ?P1)
         (!SET-UP-BARRICADES ?P2)) (QUELL-RIOT ?LOC))
(=> (AND (PERSON ?PERSON) (SHELTER ?SHELTER) (GET-TO ?PERSON ?SHELTER)) (PROVIDE-TEMP-HEAT
                                                                         ?PERSON))
(=> (AND (PERSON ?PERSON) (ATLOC ?PERSON ?PLOC)
         (GENERATE-TEMP-ELECTRICITY ?PLOC) (!TURN-ON-HEAT ?PLOC)) (PROVIDE-TEMP-HEAT
                                                                   ?PERSON))
(=> (AND (POWER-CREW ?CREW) (POWER-VAN ?VAN) (GET-TO ?CREW ?LINELOC)
         (GET-TO ?VAN ?LINELOC) (REPAIR-LINE ?CREW ?LINELOC)) (FIX-POWER-LINE
                                                               ?LINELOC))
(=> (AND (HOSPITAL ?HOSP) (HAS-CONDITION ?PERSON ?COND)
         (NOT (HOSPITAL-DOESNT-TREAT ?HOSP ?COND)) (GET-TO ?PERSON ?HOSP)
         (!TREAT-IN-HOSPITAL ?PERSON ?HOSP)) (PROVIDE-MEDICAL-ATTENTION
                                              ?PERSON))
(=> (AND (HAS-CONDITION ?PERSON ?COND) (NOT (SERIOUS-CONDITION ?COND))
         (EMT-TREAT ?PERSON)) (PROVIDE-MEDICAL-ATTENTION ?PERSON))
(=> (AND (HAZARD-SERIOUSNESS ?FROM ?TO VERY-HAZARDOUS) (!CALL FEMA)) (CLEAN-UP-HAZARD
                                                                      ?FROM
                                                                      ?TO))
(=> (AND (HAZARD-TEAM ?HT) (GET-TO ?HT ?FROM)
         (!CLEAN-HAZARD ?HT ?FROM ?TO)) (CLEAN-UP-HAZARD ?FROM ?TO))
(=> (AND (POLICE-UNIT ?POLICE) (SET-UP-CONES ?FROM ?TO)
         (GET-TO ?POLICE ?FROM)) (BLOCK-ROAD ?FROM ?TO))
(=> (AND (TAKE-DOWN-CONES ?FROM ?TO)) (UNBLOCK-ROAD ?FROM ?TO))
(=> (AND (NOT (NO-ELECTRICITY ?LOC))) (GET-ELECTRICITY ?LOC))
(=> (AND (GENERATE-TEMP-ELECTRICITY ?LOC)) (GET-ELECTRICITY ?LOC))
(=> (AND (WATER-CREW ?CREW) (GET-TO ?CREW ?FROM) (SET-UP-CONES ?FROM ?TO)
         (OPEN-HOLE ?FROM ?TO) (!REPLACE-PIPE ?CREW ?FROM ?TO)
         (CLOSE-HOLE ?FROM ?TO) (TAKE-DOWN-CONES ?FROM ?TO)) (REPAIR-PIPE
                                                              ?FROM ?TO))
(=> (AND (BACKHOE ?BACKHOE) (GET-TO ?BACKHOE ?FROM) (!DIG ?BACKHOE ?FROM)) (OPEN-HOLE
                                                                            ?FROM
                                                                            ?TO))
(=> (AND (BACKHOE ?BACKHOE) (GET-TO ?BACKHOE ?FROM)
         (!FILL-IN ?BACKHOE ?FROM)) (CLOSE-HOLE ?FROM ?TO))
(=> (AND (WORK-CREW ?CREW) (GET-TO ?CREW ?FROM) (!PLACE-CONES ?CREW)) (SET-UP-CONES
                                                                       ?FROM
                                                                       ?TO))
(=> (AND (WORK-CREW ?CREW) (GET-TO ?CREW ?FROM) (!PICKUP-CONES ?CREW)) (TAKE-DOWN-CONES
                                                                        ?FROM
                                                                        ?TO))
(=> (AND (WRECKED-VEHICLE ?FROM ?TO ?VEH) (GARBAGE-DUMP ?DUMP)
         (TOW-TO ?VEH ?DUMP)) (CLEAR-WRECK ?FROM ?TO))
(=> (AND (TOW-TRUCK ?TTRUCK) (VEHICLE ?VEH) (ATLOC ?VEH ?VEHLOC)
         (GET-TO ?TTRUCK ?VEHLOC) (!HOOK-TO-TOW-TRUCK ?TTRUCK ?VEH)
         (GET-TO ?TTRUCK ?TO) (!UNHOOK-FROM-TOW-TRUCK ?TTRUCK ?VEH)) (TOW-TO
                                                                      ?VEH
                                                                      ?TO))
(=> (AND (TREE-CREW ?TCREW) (TREE ?TREE) (ATLOC ?TREE ?TREELOC)
         (GET-TO ?TCREW ?TREELOC) (!CUT-TREE ?TCREW ?TREE)
         (REMOVE-BLOCKAGE ?TREE)) (CLEAR-TREE ?TREE))
(=> (AND (WORK-CREW ?CREW) (ATLOC ?STUFF ?LOC) (GET-TO ?CREW ?LOC)
         (!CARRY-BLOCKAGE-OUT-OF-WAY ?CREW ?STUFF)) (REMOVE-BLOCKAGE
                                                     ?STUFF))
(=> (AND (GARBAGE-DUMP ?DUMP) (GET-TO ?STUFF ?DUMP)) (REMOVE-BLOCKAGE
                                                      ?STUFF))
(=> (AND (!CALL EBS) (!CALL POLICE-CHIEF)) (DECLARE-CURFEW ?TOWN))
(=> (AND (GENERATOR ?GEN) (MAKE-FULL-FUEL ?GEN) (GET-TO ?GEN ?LOC)
         (!HOOK-UP ?GEN ?LOC) (!TURN-ON ?GEN)) (GENERATE-TEMP-ELECTRICITY
                                                ?LOC))
(=> (AND (GAS-CAN ?GC) (ATLOC ?GEN ?GENLOC) (SERVICE-STATION ?SS)
         (GET-TO ?GC ?SS) (ADD-FUEL ?SS ?GC) (GET-TO ?GC ?GENLOC)
         (!POUR-INTO ?GC ?GEN)) (MAKE-FULL-FUEL ?GEN))
(=> (AND (SERVICE-STATION ?SS) (GET-TO ?GEN ?SS) (ADD-FUEL ?SS ?GEN)) (MAKE-FULL-FUEL
                                                                       ?GEN))
(=> (AND (!PAY ?SS) (!PUMP-GAS-INTO ?SS ?OBJ)) (ADD-FUEL ?SS ?OBJ))
(=> (AND (TREE ?TREE) (ATLOC ?TREE ?LINELOC) (ATLOC ?CREW ?LINELOC)
         (SHUT-OFF-POWER ?CREW ?LINELOC)
         (UNORDERED (CLEAR-TREE ?TREE) (!REMOVE-WIRE ?CREW ?LINELOC))
         (!STRING-WIRE ?CREW ?LINELOC) (TURN-ON-POWER ?CREW ?LINELOC)) (REPAIR-LINE
                                                                        ?CREW
                                                                        ?LINELOC))
(=> (AND (ATLOC ?CREW ?LINELOC) (SHUT-OFF-POWER ?CREW ?LINELOC)
         (!REMOVE-WIRE ?CREW ?LINELOC) (!STRING-WIRE ?CREW ?LINELOC)
         (TURN-ON-POWER ?CREW ?LINELOC)) (REPAIR-LINE ?CREW ?LINELOC))
(=> (AND (IN-TOWN ?LOC ?TOWN) (POWERCO-OF ?TOWN ?POWERCO) (!CALL ?POWERCO)) (SHUT-OFF-POWER
                                                                             ?CREW
                                                                             ?LOC))
(=> (AND (IN-TOWN ?LOC ?TOWN) (POWERCO-OF ?TOWN ?POWERCO) (!CALL ?POWERCO)) (TURN-ON-POWER
                                                                             ?CREW
                                                                             ?LOC))
(=> (AND (IN-TOWN ?FROM ?TOWN) (WATERCO-OF ?TOWN ?WATERCO)
         (!CALL ?WATERCO)) (SHUT-OFF-WATER ?FROM ?TO))
(=> (AND (IN-TOWN ?FROM ?TOWN) (WATERCO-OF ?TOWN ?WATERCO)
         (!CALL ?WATERCO)) (TURN-ON-WATER ?FROM ?TO))
(=> (AND (EMT-CREW ?EMT) (ATLOC ?PERSON ?PERSONLOC)
         (GET-TO ?EMT ?PERSONLOC) (!TREAT ?EMT ?PERSON)) (EMT-TREAT
                                                          ?PERSON))
(=> (AND (EMT-TREAT ?PERSON)) (STABILIZE ?PERSON))



(=> (AND (ATLOC ?OBJ ?PLACE)) (GET-TO ?OBJ ?PLACE))

;; The operators for get-to for vehicle and person have the same
;; conditions with the same (temporal) semantics and when one was
;; selected the other would never appear. To counter this behavior, I
;; combined the operators into a single rule. This improved
;; recognition from 33% to 16%.

;(=> (AND (NOT (ATLOC ?PERSON ?PLACE)) (PERSON ?PERSON) (VEHICLE ?VEH)
;         (ATLOC ?VEH ?VEHLOC) (ATLOC ?PERSON ?VEHLOC)
;         (DRIVE-TO ?PERSON ?VEH ?PLACE)) (GET-TO ?PERSON ?PLACE))

;(=> (AND (NOT (ATLOC ?VEH ?PLACE)) (PERSON ?PERSON) (VEHICLE ?VEH)
;         (ATLOC ?VEH ?VEHLOC) (ATLOC ?PERSON ?VEHLOC)
;         (DRIVE-TO ?PERSON ?VEH ?PLACE)) (GET-TO ?VEH ?PLACE))

(=> (AND (NOT (ATLOC ?VEH ?PLACE)) (PERSON ?PERSON) (VEHICLE ?VEH)
         (ATLOC ?VEH ?VEHLOC) (ATLOC ?PERSON ?VEHLOC)
         (DRIVE-TO ?PERSON ?VEH ?PLACE)) (AND (GET-TO ?VEH ?PLACE) (GET-TO ?PERSON ?PLACE)))


(=> (AND (NOT (ATLOC ?OBJ ?PLACE)) (VEHICLE ?VEH) (ATLOC ?OBJ ?OBJLOC)
         (FIT-IN ?OBJ ?VEH) (NOT (NON-AMBULATORY ?OBJ))
         (GET-TO ?VEH ?OBJLOC) (GET-IN ?OBJ ?VEH) (GET-TO ?VEH ?PLACE)
         (GET-OUT ?OBJ ?VEH)) (GET-TO ?OBJ ?PLACE))

(=> (AND (NOT (ATLOC ?OBJ ?PLACE)) (ATLOC ?OBJ ?OBJLOC) (AMBULANCE ?VEH)
         (FIT-IN ?OBJ ?VEH) (GET-TO ?VEH ?OBJLOC) (STABILIZE ?OBJ)
         (GET-IN ?OBJ ?VEH) (GET-TO ?VEH ?PLACE) (GET-OUT ?OBJ ?VEH)) (GET-TO
                                                                       ?OBJ
                                                                       ?PLACE))


(=> (AND (PERSON ?PERSON) (VEHICLE ?VEH) (ATLOC ?VEH ?VEHLOC)
         (ATLOC ?PERSON ?VEHLOC) (CAN-DRIVE ?PERSON ?VEH)
         (!NAVEGATE-VEHICLE ?PERSON ?VEH ?LOC)) (DRIVE-TO ?PERSON ?VEH
                                                 ?LOC))
(=> (AND (ATLOC ?OBJ ?OBJLOC) (ATLOC ?VEH ?OBJLOC) (PERSON ?OBJ)
         (NOT (NON-AMBULATORY ?OBJ)) (!CLIMB-IN ?OBJ ?VEH)) (GET-IN ?OBJ
                                                             ?VEH))
(=> (AND (ATLOC ?OBJ ?OBJLOC) (ATLOC ?VEH ?OBJLOC) (PERSON ?PERSON)
         (CAN-LIFT ?PERSON ?OBJ) (GET-TO ?PERSON ?OBJLOC)
         (!LOAD ?PERSON ?OBJ ?VEH)) (GET-IN ?OBJ ?VEH))
(=> (AND (PERSON ?OBJ) (NOT (NON-AMBULATORY ?OBJ)) (!CLIMB-OUT ?OBJ ?VEH)) (GET-OUT
                                                                            ?OBJ
                                                                            ?VEH))
(=> (AND (ATLOC ?VEH ?VEHLOC) (PERSON ?PERSON) (CAN-LIFT ?PERSON ?OBJ)
         (GET-TO ?PERSON ?VEHLOC) (!UNLOAD ?PERSON ?OBJ ?VEH)) (GET-OUT
                                                                ?OBJ ?VEH))))

(defvar *monroe-state* 
'(
   ;;;;;;; locations

   ;; service stations
   (service-station texaco1)
   (in-town texaco1 brighton)

   ;; hospitals
   (hospital strong)
   (in-town strong rochester)

   (hospital park-ridge)
   (in-town park-ridge greece)

   (hospital rochester-general)
   (in-town rochester-general rochester)
   (hospital-doesnt-treat rochester-general broken-leg)

   ;; dumps
   (garbage-dump brighton-dump)
   (in-town brighton-dump brighton)

   (garbage-dump henrietta-dump)
   (in-town henrietta-dump henrietta)

   ;; malls
   (mall marketplace)
   (in-town marketplace henrietta)

   ;; transport-hubs
   (transport-hub airport)
   (in-town airport gates)

   ;; schools
   (school brighton-high)
   (in-town brighton-high brighton)

   ;; parks
   (park mendon-pond)
   (in-town mendon-pond mendon)

   ;; general points
   (point 12-corners)
   (in-town 12-corners brighton)

   (point pittsford-plaza)
   (in-town pittsford-plaza pittsford)

   ;; just assume everywhere is a town (this combines town/villages web/pitts
   (town rochester)
   (town brighton)
   (town mendon)
   (town hamlin)
   (town webster)
   (town irondequoit)
   (town henrietta)
   (town greece)
   (town parma)
   (town clarkson)
   (town sweeden)
   (town ogden)
   (town gates)
   (town riga)
   (town chili)
   (town wheatland)
   (town pittsford)
   (town scottsville)
   (town rush)
   (town perinton)
   (town fairport)
   (town penfield)
   (town east-rochester)
   (town churchville)
   (town brockport)
   (town spencerport)
   (town hilton)
   (town honeoye-falls)

   ;; power
   (powerco-of rochester rge)
   (powerco-of gates rge)
   (powerco-of brighton rge)
   (powerco-of henrietta rge)
   (powerco-of greece rge)
   (powerco-of chili rge)
   (powerco-of mendon mendon-ge)
   (powerco-of hamlin monroe-ge)
   (powerco-of webster monroe-ge)
   (powerco-of irondequoit monroe-ge)
   (powerco-of parma monroe-ge)
   (powerco-of clarkson monroe-ge)
   (powerco-of sweeden monroe-ge)
   (powerco-of ogden monroe-ge)
   (powerco-of riga monroe-ge)
   (powerco-of wheatland monroe-ge)
   (powerco-of pittsford monroe-ge)
   (powerco-of scottsville monroe-ge)
   (powerco-of rush monroe-ge)
   (powerco-of perinton monroe-ge)
   (powerco-of fairport monroe-ge)
   (powerco-of penfield monroe-ge)
   (powerco-of east-rochester monroe-ge)
   (powerco-of churchville monroe-ge)
   (powerco-of brockport monroe-ge)
   (powerco-of spencerport monroe-ge)
   (powerco-of hilton monroe-ge)
   (powerco-of honeoye-falls monroe-ge)

   ;; water
   (waterco-of rochester roch-water)
   (waterco-of gates roch-water)
   (waterco-of brighton roch-water)
   (waterco-of henrietta roch-water)
   (waterco-of greece monroe-water)
   (waterco-of chili roch-water)
   (waterco-of mendon mendon-water)
   (waterco-of hamlin roch-water)
   (waterco-of webster monroe-water)
   (waterco-of irondequoit monroe-water)
   (waterco-of parma monroe-water)
   (waterco-of clarkson monroe-water)
   (waterco-of sweeden monroe-water)
   (waterco-of ogden monroe-water)
   (waterco-of riga monroe-water)
   (waterco-of wheatland monroe-water)
   (waterco-of pittsford monroe-water)
   (waterco-of scottsville monroe-water)
   (waterco-of rush monroe-water)
   (waterco-of perinton monroe-water)
   (waterco-of fairport monroe-water)
   (waterco-of penfield monroe-water)
   (waterco-of east-rochester monroe-water)
   (waterco-of churchville monroe-water)
   (waterco-of brockport monroe-water)
   (waterco-of spencerport monroe-water)
   (waterco-of hilton monroe-water)
   (waterco-of honeoye-falls monroe-water)

  ;; serious conditions
   (serious-condition heart-attack)
   (serious-condition head-injury)
   (serious-condition broken-leg)

   ;; people and vehicles
   ((emt-crew emt1) (ambulance amb1))
   ((bus-driver bdriver1) (bus bus1))
   ((tow-truck-driver ttdriver1)(tow-truck ttruck1))
   ((power-crew pcrew1)(power-van van1))
   ((tree-crew tcrew1)(tree-truck ttruck1))
   ((water-crew wcrew1)(water-truck wtruck1))
   ((truck-driver tdriver1)(dump-truck dtruck1))
   ((police-unit pu1)(police-van pvan1))
   ((police-unit pu2)(police-van pvan2))
   ((construction-crew ccrew1)(backhoe backhoe1))
   ((plowdriver pdriver1)) ;; plowdrivers don't need to be with their plows
   ((plowdriver pdriver2))
   ((snowplow plow1))
   ((snowplow plow2))
   ((hazard-team ht1))
   ((hazard-team ht2))
   ((shelter-leader sleader1))
   ((shelter-leader sleader2))
   ((shelter-leader sleader3))

   ;;;;;;;;;;; other stuff
   ;; generators
   ((generator gen1))
   ((generator gen2))
   ;; food - for shelters
   ((food food1))
   ((food food2))
   ((food food3))
  ))

(defvar *monroe-top-goals*)
(setf *monroe-top-goals* 
'(set-up-shelter fix-water-main clear-road-hazard
  clear-road-wreck clear-road-tree plow-road 
  quell-riot provide-temp-heat fix-power-line
  provide-medical-attention))


(defvar *monroe-operators*)
(setf *monroe-operators* 
'(SET-UP-SHELTER FIX-WATER-MAIN CLEAR-ROAD-HAZARD CLEAR-ROAD-WRECK
 CLEAR-ROAD-TREE PLOW-ROAD QUELL-RIOT PROVIDE-TEMP-HEAT 
 FIX-POWER-LINE PROVIDE-MEDICAL-ATTENTION 
 CLEAN-UP-HAZARD BLOCK-ROAD UNBLOCK-ROAD GET-ELECTRICITY
 REPAIR-PIPE OPEN-HOLE CLOSE-HOLE SET-UP-CONES TAKE-DOWN-CONES
 CLEAR-WRECK TOW-TO CLEAR-TREE REMOVE-BLOCKAGE DECLARE-CURFEW
 GENERATE-TEMP-ELECTRICITY MAKE-FULL-FUEL ADD-FUEL
 REPAIR-LINE SHUT-OFF-POWER TURN-ON-POWER SHUT-OFF-WATER TURN-ON-WATER
 EMT-TREAT STABILIZE GET-TO DRIVE-TO GET-IN GET-OUT))


(defun count-top-level (fp)
  (apply #'+ (mapcar #'(lambda (x) (if (member (first x) *monroe-top-goals*) 1 0))
		     fp)))

(defun plan-action-p (p)
  (eq (char (symbol-name (first p)) 0) #\!))
  

;; given a hierarchical plan, returns a list of plan steps
(defun flatten-plan (x)
  (cond ((every #'atom x) ; plan step
	 (list x))
	((rest x)
	 (append (flatten-plan (first x)) (flatten-plan (rest x))))
	(t
	 (flatten-plan (first x)))))

;; given working memory, returns the beliefs that correspond to plan steps
(defun inferred-flat-plan (wm)
  (mapcar #'(lambda (x) (belief-content (second x)))
	  (append (get-observed-beliefs (wm-prime wm)) (get-local-beliefs (wm-prime wm)))))

(defun count-matches (x y)
  (if (and x y)
      (loop for a in x and b in y count (eql a b))
      0))

;; not sure what Sindhu and Ray are doing, but here's my best guess.
(defun planrec-accuracy (inf-plan top-operator src-plan)
  ;; find the best match of the top level operator and arguments
  ;; one point for the operator, one point for each argument
  (let ((matches (delete (first top-operator) inf-plan :test-not #'eql :key #'first))) 
    ;(princ top-operator)
    ;(princ matches)
    (if matches 
	(apply #'max (mapcar #'(lambda (x) (count-matches x top-operator)) matches))
	0)))

(defun test-monroe (steps case &optional (pb #'pick-belief-fewrules-unattached) (deduction? nil) (print? t))
  (setf *bad-beliefs* nil)
  (setf *last-3-beliefs* nil)

  (test-one steps case *monroe-kb* pb deduction? print?))

;; n -- test on the first n cases
;; bi? -- enforce bidirectional explanations (good idea!)
;; percent -- percentage of actions to use for each case
;;            50% would run the system on the first half 
;;            of the actions associated with a plan.
(defun test-monroe-100 (n bi? &optional (percent 100))
  (if (> n 100) (setf n 100))
  (if (> percent 100) (setf percent 100))
  (if (< percent 0) (setf percent 0))
  (let ((counter 0) (total 0) (leftover 0)
	(acc-total 0) (acc-numerator 0) 
	(prec-rec-numerator 0)
	(precision-denominator 0)
	top-level ctl (tgoal 0)
	fp ttmp lotmp rntmp pb actions prpln acct-tmp accn-tmp)
    (if bi? 
	(setf *divide-lookahead?* t pb #'pick-belief-fewrules-uda)
	(setf *divide-lookahead?* nil pb #'pick-belief-fewrules-unattached))
   ;; (setf pb #'pick-belief-random)
    (dolist (p (subseq *monroe-100* 0 n))
      (setf top-level (first p))
      (setf fp (delete-if #'plan-action-p (flatten-plan p)))
      (setf actions (delete-if-not #'plan-action-p (flatten-plan p)))
      (when (< percent 100) (setf actions (subseq actions 0 (round (* (length actions) (/ percent 100))))))
      (test-monroe 200 actions pb nil nil)
      (setf ttmp (length fp))
      ; (setf acct-tmp (apply #'+ (mapcar #'length fp)))
      (setf acct-tmp (length (first fp)))
      (if (member (car top-level) (mapcar #'first (inferred-flat-plan wm)) :test #'equal)
	  (incf tgoal))
      (setf lotmp (length (set-difference fp (inferred-flat-plan wm) :test #'equal)))
      (setf rntmp (length (intersection fp (inferred-flat-plan wm) :test #'equal)))
      (setf prpln 
	    (delete-if-not #'(lambda (x) (find (predicate-name x) *monroe-operators*))
			   (delete-if-not #'literal-grounded? (inferred-flat-plan wm))))
      (setf accn-tmp (planrec-accuracy (inferred-flat-plan wm) (first fp) fp))
      (setf ctl (count-top-level (delete-if-not #'literal-grounded? (inferred-flat-plan wm))))
      (format t "~A: ~A => ~A :: # top-level methods = ~A~%" counter ttmp lotmp ctl)
      (incf acc-numerator accn-tmp)
      (incf acc-total acct-tmp)
      (incf prec-rec-numerator rntmp)
      (incf total ttmp)
      (incf precision-denominator (length prpln))
      (incf leftover lotmp)
      (incf counter))
    (format t "Total: ~A~%Left: ~A~%Accuracy: ~,2F%~%Recall: ~,2F%~%Precision: ~,2F%~%F-measure: ~,2F%~%Goal: ~,2F%~%"
	    total leftover 
	    (* 100 (float (/ acc-numerator acc-total)))
	    ;; (* 100 (float (/ leftover total))) ;; missing
	    (* 100 (float (/ prec-rec-numerator total))) ;; recall
	    (* 100 (float (/ prec-rec-numerator precision-denominator))) ;; precision
	    (* 100 (float (* (/ (* (/ prec-rec-numerator precision-denominator)(/ prec-rec-numerator total))
				(+ (/ prec-rec-numerator precision-denominator)(/ prec-rec-numerator total)))
			     2))) ;; f-measure
	    (* 100 (float (/ tgoal n)))
	    )
    ))

#|

(0) don't let forward and backward inferences compete with eachother. we want one of each for each belief.
(1) limit justifications so that a belief can only appear in a justification once. see monroe-case2.
|#

(defun tm (&optional (n 50) (bi? nil))
  (let (p fp ttmp lotmp pb)
    (if bi? 
	(setf *divide-lookahead?* t pb #'pick-belief-fewrules-uda)
	(setf *divide-lookahead?* nil pb #'pick-belief-fewrules-unattached))
    (when (> n 99) (setf n 99))
    (setf p (nth n *monroe-100*))
    (format t "~%Plan:~%~A~%~%" p)
    (setf fp (delete-if #'plan-action-p (flatten-plan p)))
    (test-monroe 500 (delete-if-not #'plan-action-p (flatten-plan p)) pb nil t)
    (setf ttmp (length fp))
    (setf lotmp (length (set-difference fp (inferred-flat-plan wm) :test #'equal)))
    (format t "~%Total: ~A~%Left: ~A~%Missing: ~,2F%~%"
	    ttmp lotmp (* 100 (float (/ lotmp ttmp))))
    (format t "~%Missing: ~A~%" (set-difference fp (inferred-flat-plan wm) :test #'equal))))
