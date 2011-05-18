

; Icarus concepts for the Tower of Hanoi puzzle. 

(create-concepts

;; why is this so difficult?
 ((not-same-peg ?peg1 ?peg2)
  :percepts ((peg ?peg1) (peg ?peg2)))


;;; Typing concepts

 ((peg ?peg)
  :percepts ((peg ?peg)))

 ((disk ?disk)
  :percepts ((disk ?disk)))


 ;; ON-PEG matches when ?DISK has the same YPOS as ?PEG and when it
 ;; is no higher than the pegheight of ?PEG.

 ((on-peg ?disk ?peg)
  :percepts  ((disk ?disk xpos ?dxpos ypos ?dypos)
	      (peg ?peg xpos ?pxpos ypos ?pypos pegheight ?pheight))
  :tests     ((equal ?dxpos ?pxpos)
	      (>= ?dypos ?pypos)
	      (< ?dypos (+ ?pypos ?pheight))))

 ;; SMALLER-THAN matches when ?DISK1 has less width than ?DISK2.

 ((smaller-than ?disk1 ?disk2)
  :percepts  ((disk ?disk1 width ?width1)
	      (disk ?disk2 width ?width2))
  :tests     ((< ?width1 ?width2)))

;;;;; Non-primitive concepts

 ;; three distinct pegs
 ((three-different-pegs ?peg1 ?peg2 ?peg3)
  :relations ((not-same-peg ?peg1 ?peg2)
	      (not-same-peg ?peg1 ?peg3)
	      (not-same-peg ?peg2 ?peg3)))
  

 ;; between in size
 ((ordered-by-size ?disk1 ?disk2 ?disk3)
  :relations ((disk ?disk1) (disk ?disk2) (disk ?disk3)
	      (smaller-than ?disk1 ?disk2)
	      (smaller-than ?disk2 ?disk3)))

 ;; ?disk1 is immediately smaller than ?disk 2 (ie no other disk is between them in size)
 ((next-smaller ?disk1 ?disk)
  :relations ((disk ?disk1) (disk ?disk)
	      (smaller-than ?disk1 ?disk)
	      (not (ordered-by-size ?disk1 ?any ?disk))))

 ;; SMALLER-ON matches when a disk ?SMALLER smaller than ?DISK is on ?PEG
 ;;   thus blocking ?DISK from moving on or off ?PEG.

 ((smaller-on  ?disk ?smaller ?peg)
  :relations ((disk ?smaller) (disk ?disk) (peg ?peg)
	      (smaller-than ?smaller ?disk)
	      (on-peg ?smaller ?peg)))

 ;; NOT-ON-PEG matches when ?DISK is not on ?PEG.

 ((not-on-peg ?disk ?peg)
  :relations ((disk ?disk) (peg ?peg)(not (on-peg ?disk ?peg))))

 ;; ALL-DISKS-ON matches when all of the disks are on ?PEG.

 ((all-disks-on ?peg)
  :relations ((peg ?peg) (not (not-on-peg ?disk ?peg))))

 ;; base case (smallest ?disk)
 ((tower ?disk ?peg)
  :relations ((on-peg ?disk ?peg)
	      (not (smaller-than ?any ?disk))))

 ;; recursive case (?disk is on ?peg, and next-smaller disk is a tower on ?peg)
 ((tower ?disk ?peg)
  :relations ((disk ?disk) (peg ?peg)
	      (on-peg ?disk ?peg)
	      (disk ?disk1)
	      (next-smaller ?disk1 ?disk)
	      (tower ?disk1 ?peg)))

 )

;;;   Minimal concepts:
;;;       (ON ?disk ?peg)
;;;       (SMALLER ?disk1 ?disk2)
;;;       (SMALLER-ON ?disk ?smaller ?peg)


(create-skills

;;;   MOVE DISK
 ((move ?disk ?from ?to)
  :conditions ((on-peg ?disk ?from)
	       (not (smaller-on ?disk ?any ?from))
	       (not (smaller-on ?disk ?any ?to)))
  :effects ((on-peg ?disk ?to)
	    (not (on-peg ?disk ?from)))
  :action (*move ?disk ?from ?to))




;;; recursive macro skill:

;; Do we need a base case for smallest disk if already on target peg?
((build-tower ?disk ?peg)
 :conditions ((disk ?disk) (peg ?peg)
	      (not (smaller-than ?any ?disk)) ; ?disk is smallest disk
	      (on-peg ?disk ?peg))
 :effects ((tower ?disk ?peg))
 :subskills ())				; nothing to do, already done

;; Base case: smallest disk, but not on target peg
((build-tower ?disk ?peg)
 :conditions ((disk ?disk) (peg ?peg)
	      (not (smaller-than ?any ?disk)) ; ?disk is smallest disk
	      (on-peg ?disk ?from)
	      (not (on-peg ?disk ?peg)))
 :effects ((on-peg ?disk ?peg) (tower ?disk ?peg))
 :subskills ((move ?disk ?from ?peg)))

;; Recursive case, not smallest, but disk already on target peg
((build-tower ?disk ?peg)
 :conditions ((disk ?disk) (peg ?peg)
	      (on-peg ?disk ?peg)
	      (next-smaller ?disk1 ?disk))
 :effects ((tower ?disk ?peg))
 :subskills ((build-tower ?disk1 ?peg)))

;; Recursive case, not smallest, and disk not on target peg
((build-tower ?disk ?peg)
 :conditions ((disk ?disk) (peg ?peg)
	      (next-smaller ?disk1 ?disk)
	      (on-peg ?disk ?from)
	      (three-different-pegs ?peg ?from ?other))
 :effects ((tower ?disk ?peg))
 :subskills ((build-tower ?disk1 ?other)
	     (move ?disk ?from ?peg)
	     (build-tower ?disk1 ?peg)))

)



#|
;; why does this fail
 ((different-pegs ?peg1 ?peg2 ?peg3)
 :percepts ((peg ?peg1) (peg ?peg2) (peg ?peg3))
 :tests ()
;	 ((not (equal ?peg1 ?peg2))	; ;
;	 (not (equal ?peg1 ?peg3))	; ;
;	 (not (equal ?peg2 ?peg3))	; ;
 )
|#
