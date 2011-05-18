

; Icarus concepts for the Tower of Hanoi puzzle. 

(create-concepts

; ON-PEG matches when ?DISK has the same YPOS as ?PEG and when it
; is no higher than the pegheight of ?PEG.

((on-peg ?disk ?peg)
 :percepts  ((disk ?disk xpos ?dxpos ypos ?dypos)
             (peg ?peg xpos ?pxpos ypos ?pypos pegheight ?pheight))
 :tests     ((equal ?dxpos ?pxpos)
             (>= ?dypos ?pypos)
             (< ?dypos (+ ?pypos ?pheight))))

; SMALLER-THAN matches when ?DISK1 has less width than ?DISK2.

((smaller-than ?disk1 ?disk2)
 :percepts  ((disk ?disk1 width ?width1)
             (disk ?disk2 width ?width2))
 :tests     ((< ?width1 ?width2)))

; SMALLER-ON matches when a disk ?SMALLER smaller than ?DISK is on ?PEG
;   thus blocking ?DISK from moving on or off ?PEG.

((smaller-on  ?disk ?smaller ?peg)
 :percepts  ((disk ?smaller) (disk ?disk) (peg ?peg))
 :relations ((smaller-than ?smaller ?disk)
             (on-peg ?smaller ?peg)))

; NOT-ON-PEG matches when ?DISK is not on ?PEG.

((not-on-peg ?disk ?peg)
 :percepts  ((disk ?disk) (peg ?peg))
 :relations ((not (on-peg ?disk ?peg))))

; ALL-DISKS-ON matches when all of the disks are on ?PEG.

((all-disks-on ?peg)
 :percepts  ((peg ?peg))
 :relations ((not (not-on-peg ?disk ?peg))))

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

)

