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

; CLEAR matches when a block has no other block on top of it.
 ((clear ?block)
  :relations ((block ?block) (not (on ?other-block ?block))))

; HOLDING is satisfied when the hand is holding a block.
 ((holding ?block)
  :percepts ((hand ?hand status ?block) (block ?block)))

; HAND-EMPTY describes a situation in which the hand is not holding a block.
 ((hand-empty)
  :percepts ((hand ?hand status ?status))
  :relations ((not (holding ?any)))
  :tests ((eq ?status 'empty)))

)

; Create a set of primitive skill clauses for the Blocks World.
(create-skills

; UNSTACK block from another block by grasping and moving block.
 ((unstack ?block ?from)
  :percepts ((block ?block) (block ?from))
  :conditions ((on ?block ?from) (clear ?block) (hand-empty))
  :effects ((not (on ?block ?from)) (clear ?from) (holding ?block)
	    (not (hand-empty)))
  :action  (*grasp-and-lift ?block))

;  PICK-UP block from table by grasping and moving block.
 ((pick-up ?block ?from)
  :percepts ((block ?block) (table ?from))
  :conditions ((clear ?block) (hand-empty) (ontable ?block ?from))
  :effects ((not (clear ?block)) (not (ontable ?block ?from))
	    (holding ?block) (not (hand-empty)))
  :action   (*grasp-and-lift ?block))

; STACKED block on another block by moving and ungrasping the block.
 ((stack ?block ?to)
  :percepts ((block ?block) (block ?to xpos ?x ypos ?y  height ?h))
  :conditions ((holding ?block) (clear ?to))
  :effects ((on ?block ?to) (hand-empty) (not (holding ?block))
	    (not (clear ?to)))
  :action (*put-down-at-x-y ?block ?x (+ ?y ?h)))

; Achieve PUT-DOWN by moving and ungrasping a putdownable block.
 ((put-down ?block ?to)
  :percepts ((block ?block width ?w) (table ?to xpos ?tx ypos ?ty height ?th)
	     (marker FREE xpos ?x))
  :conditions ((holding ?block))
  :effects ((ontable ?block ?to) (hand-empty) (not (holding ?block)))
  :action (*put-down-at-x-y ?block ?x (+ ?ty ?th) ?w)) 

;;; Macro-skills

((unstack-to-table ?block ?from ?table)
 :conditions ((block ?block) (block ?from) (table ?table)
	      (clear ?block) (on ?block ?from) (hand-empty))
 :effects ((ontable ?block ?table) (clear ?from) (not (on ?block ?from)))
 :subskills ((unstack ?block ?from) (put-down ?block ?table)))

;; recursive clear
((make-clear ?block)
 :conditions ((block ?block) (block ?on) (on ?on ?block)
	      (clear ?on)(hand-empty))
 :effects ((clear ?block) (not (on ?on ?block)) )
 :subskills ((unstack ?on ?block) (put-down ?on ?table))
 )

((make-clear ?block)
 :conditions ((block ?block) (block ?on) (on ?on ?block) (not (clear ?on)))
 :effects ((clear ?block) (not (on ?on ?block)))
 :subskills ((make-clear ?on) (unstack ?on ?block) (put-down ?on ?table)))

)
