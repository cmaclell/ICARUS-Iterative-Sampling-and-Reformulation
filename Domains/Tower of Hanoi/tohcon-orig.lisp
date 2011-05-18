

; Icarus concepts for the Tower of Hanoi puzzle. 

(create-concepts

; ON-PEG matches when ?DISK has the same YPOS as ?PEG and when it
; is no higher than the height of ?PEG.

((on-peg ?disk ?peg)
 :percepts  ((disk ?disk xpos ?dxpos ypos ?dypos)
             (peg ?peg xpos ?pxpos ypos ?pypos height ?pheight))
 :tests     ((equal ?dxpos ?pxpos)
             (>= ?dypos ?pypos)
             (< ?dypos (+ ?pypos ?pheight))))

; NOT-ON-PEG matches when ?DISK is not on ?PEG.

((not-on-peg ?disk ?peg)
 :percepts  ((disk ?disk) (peg ?peg))
 :relations ((not (on-peg ?disk ?peg))))

; ON-DISK matches when ?DISK1 is sitting directly on ?DISK2.

((on-disk ?disk1 ?disk2)
 :percepts  ((disk ?disk1 xpos ?xpos ypos ?ypos1)
             (disk ?disk2 xpos ?xpos ypos ?ypos2 height ?height2))
 :tests     ((>= ?ypos1 ?ypos2)
             (<= ?ypos1 (+ ?ypos2 ?height2))))

; SMALLER-THAN matches when ?DISK1 has less width than ?DISK2.

((smaller-than ?disk1 ?disk2)
 :percepts  ((disk ?disk1 width ?width1)
             (disk ?disk2 width ?width2))
 :tests     ((< ?width1 ?width2)))

; BETWEEN-ON matches when disks ?SMALLER and ?MIDDLE are on ?PEG, with
; ?MIDDLE being larger than ?SMALLER and smaller than disk ?LARGER.

((between-on ?smaller ?middle ?larger ?peg)
 :percepts  ((disk ?smaller) (disk ?middle) (disk ?larger) (peg ?peg))
 :relations ((on-peg ?smaller ?peg)
             (on-peg ?middle ?peg)
             (smaller-than ?smaller ?middle)
             (smaller-than ?middle ?larger)))

; LARGEST-SMALLER-ON matches when ?SMALLER is the largest disk on ?PEG
; that is smaller than ?DISK.

((largest-smaller-on ?smaller ?disk ?peg)
 :percepts  ((disk ?smaller) (disk ?disk) (peg ?peg))
 :relations ((on-peg ?smaller ?peg)
             (smaller-than ?smaller ?disk)
             (not (between-on ?smaller ?any ?disk ?peg))))

; BLOCKING-FROM matches when a disk ?SMALLER is directly on ?DISK,
; thus keeping ?DISK from moving off ?FROM.

((blocking-from ?smaller ?disk ?from ?to)
 :percepts  ((disk ?smaller) (disk ?disk) (peg ?from) (peg ?to))
 :relations ((on-peg ?disk ?from)
             (on-disk ?smaller ?disk)))

; BLOCKING-TO matches when a disk ?SMALLER than ?DISK is on a
; different peg ?TO, and thus keeps ?DISK from moving there, with
; ?SMALLER being the largest disk on ?TO.

((blocking-to ?smaller ?disk ?from ?to)
 :percepts  ((disk ?smaller) (disk ?disk) (peg ?from) (peg ?to))
 :relations ((on-peg ?disk ?from)
             (largest-smaller-on ?smaller ?disk ?to)))

; MOVEABLE matches when ?DISK is on peg ?FROM, when no other peg
; is blocking it from moving off ?FROM, and when there no other
; peg is blocking it from moving to peg ?TO.

((moveable ?disk ?from ?to)
 :percepts  ((disk ?disk) (peg ?from) (peg ?to))
 :relations ((on-peg ?disk ?from)
             (not (blocking-from ?smaller ?disk ?from ?to))
             (not (blocking-to ?smaller ?disk ?from ?to))))

; ON-AND-OFF matches when ?DISK is on peg ?TO but not on peg ?FROM.

((on-and-off ?disk ?from ?to)
 :percepts  ((disk ?disk) (peg ?from) (peg ?to))
 :relations ((on-peg ?disk ?to)
             (not (on-peg ?disk ?from))))

; LARGER-AND-NOT-ON matches when ?DISK1 is larger than ?DISK2 but
; but is not on ?peg.

((larger-and-not-on ?disk1 ?disk2 ?peg)
 :percepts  ((disk ?disk1) (disk ?disk2) (peg ?peg))
 :relations ((smaller-than ?disk2 ?disk1)
             (not (on-peg ?disk1 ?peg))))

; LARGEST-NOT-ON MATCHES when ?disk is the largest disk that is not
; on ?PEG.

((largest-not-on ?disk ?peg)
 :percepts  ((disk ?disk) (peg ?peg))
 :relations ((not (on-peg ?disk ?peg))
             (not (larger-and-not-on ?other ?disk ?peg))))

; ALL-DISKS-ON matches when all of the disks are on ?PEG.

((all-disks-on ?peg)
 :percepts  ((peg ?peg))
 :relations ((not (not-on-peg ?disk ?peg))))

)
