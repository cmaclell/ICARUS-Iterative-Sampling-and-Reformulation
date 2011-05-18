(create-concepts
 
 ((peg ?peg)
  :percepts ((peg ?peg)))

 ((disk ?disk)
  :percepts ((disk ?disk)))

 ((diferent-peg ?peg1 ?peg2)
  :percepts ((peg ?peg1)
  	     (peg ?peg2))
  :tests ((not (eq ?peg1 ?peg2))))
 
 ((on-peg ?disk ?peg)
  :percepts ((disk ?disk xpos ?xpos)
	     (peg ?peg xpos ?xpos)))

 ((smaller-than ?disk1 ?disk2)
  :percepts	((disk ?disk1 width ?width1)
		 (disk ?disk2 width ?width2))
  :tests	((< ?width1 ?width2)))

 ((blocking-from ?smaller ?disk)
  :percepts	((disk ?smaller) 
		 (disk ?disk) 
		 (peg ?peg)) 
  :relations	((on-peg ?smaller ?peg) 
		 (on-peg ?disk ?peg)
		 (smaller-than ?smaller ?disk)))

 ((blocking-to ?smaller ?disk ?to)
  :percepts	((disk ?smaller) 
		 (disk ?disk) 
		 (peg ?to))
  :relations	((on-peg ?smaller ?to) 
  		 (not (on-peg ?disk ?to))
		 (smaller-than ?smaller ?disk)))

 ;;Testing purpose only.
 ((on-disk ?disk1 ?disk2)
  :percepts	((disk ?disk1 ypos ?ypos1)
		 (disk ?disk2 ypos ?ypos2 height ?height))
  :tests	((equal ?ypos1 (+ ?ypos2 ?height)))
  :relations	((on-peg ?disk1 ?peg)
		 (on-peg ?disk2 ?peg)))
 )

(create-skills

 ((move ?disk ?from ?to)
  :conditions ((peg ?from)
	       (peg ?to)
	       (disk ?disk)
	       (diferent-peg ?to ?from)
	       (on-peg ?disk ?from)
	       (not (blocking-from ?smaller-from ?disk))
	       (not (blocking-to ?smaller-to ?disk ?to)))
  :effects ((on-peg ?disk ?to)
	    (not (on-peg ?disk ?from)))
  :action (*move ?disk ?from ?to))
)