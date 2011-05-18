; BLOCK.LISP
; Icarus concepts, primitive skills, and goals for the Blocks World.

; Create a set of conceptual clauses for the Blocks World.
(create-concepts

;;;; Perceptual Typing Concepts

 ((block ?block)
  :percepts ((block ?block)))

 ((table ?table)
  :percepts ((table ?table)))

;;;; Other Concepts

; ON describes a situation in which one block is on top of another.
 ((on ?block1 ?block2)
  :percepts ((block ?block1 xpos ?xpos1 ypos ?ypos1)
	     (block ?block2 xpos ?xpos2 ypos ?ypos2 height ?height2))
  :tests ((equal ?xpos1 ?xpos2) (= ?ypos1 (+ ?ypos2 ?height2))))

; ONTABLE describes a situation in which a block in sitting on the table.
 ((ontable ?block1 ?table)
  :percepts ((block ?block1 xpos ?xpos1 ypos ?ypos1)
	     (table ?table xpos ?xpos2 ypos ?ypos2 width ?width2
		    height ?height2))
  :tests ((<= ?xpos2 ?xpos1)
	  (>= (+ ?xpos2 ?width2) ?xpos1)
	  (>= ?ypos1 ?ypos2)
	  (<= ?ypos1 (+ ?ypos2 ?height2))))

; HOLDING is satisfied when the hand is holding a block.
 ((holding ?block)
  :percepts ((hand ?hand status ?block) (block ?block)))
)

; Create a set of primitive skill clauses for the Blocks World.
(create-skills

; UNSTACK block from another block by grasping and moving block.
 ((unstack ?block ?from)
  :percepts ((block ?block) (block ?from))
  :conditions ((on ?block ?from) (not (on ?any-block ?block)) (not (holding ?some-block)))
  :effects ((not (on ?block ?from)) (holding ?block))
  :action  (*grasp-and-lift ?block))

;  PICK-UP block from table by grasping and moving block.
 ((pick-up ?block ?from)
  :percepts ((block ?block) (table ?from))
  :conditions ((not (on ?any-block ?block)) (not (holding ?some-block)) (ontable ?block ?from))
  :effects ((not (ontable ?block ?from))
	    (holding ?block))
  :action   (*grasp-and-lift ?block))

; STACKED block on another block by moving and ungrasping the block.
 ((stack ?block ?to)
  :percepts ((block ?block) (block ?to xpos ?x ypos ?y  height ?h))
  :conditions ((holding ?block) (not (on ?any-block ?to)))
  :effects ((on ?block ?to) (not (holding ?block)))
  :action (*put-down-at-x-y ?block ?x (+ ?y ?h)))

; Achieve PUT-DOWN by moving and ungrasping a putdownable block.
 ((put-down ?block ?to)
  :percepts ((block ?block width ?w) (table ?to xpos ?tx ypos ?ty height ?th)
	     (marker FREE xpos ?x))
  :conditions ((holding ?block))
  :effects ((ontable ?block ?to) (not (holding ?block)))
  :action (*put-down-at-x-y ?block ?x (+ ?ty ?th) ?w)) 

)
