; BLOCK.LISP
; Icarus concepts, primitive skills, and goals for the Blocks World.

; Create a set of conceptual clauses for the Blocks World.
(create-concepts

;;;; Perceptual Typing Concepts

 ((dot ?dot ?x ?y)
  :percepts ((dot ?dot xpos ?x ypos ?y)))

 ((pen ?x ?y)
  :percepts ((pen ?pen xpos ?x ypos ?y)))

 ((pen-up)
  :percepts ((pen ?pen status ?status))
  :tests    ((equal ?status 'up)))

 ((pen-down)
  :percepts ((pen ?pen status ?status))
  :tests    ((equal ?status 'down)))



; Recognizing a vertical line
 ((vertical-line-segment ?x0 ?y0 ?x1 ?y1)
  :percepts ((ink ?ink0 xpos ?x0 ypos ?y0)
	     (ink ?ink1 xpos ?x1 ypos ?y1))
  :tests    ((equal ?x0 ?x1)
	     (equal (+ ?y0 1) ?y1)))

 ((vertical-line-segment ?x0 ?y0 ?x1 ?y1)
  :percepts ((ink ?ink0 xpos ?x0 ypos ?y0)
	     (ink ?ink1 xpos ?x1 ypos ?y1)
	     (ink ?ink2 xpos ?xmid ypos ?ymid))
  :relations ((vertical-line-segment ?x0 ?y0 ?xmid ?ymid)
	       (vertical-line-segment ?xmid ?ymid ?x1 ?y1)))

 ((vertical-line-contained ?x0 ?y0 ?x1 ?y1)
  :relations ((vertical-line-segment ?x0 ?y0 ?x1 ?y1)
	      (vertical-line-segment ?x2 ?y2 ?x3 ?y3))
  :tests ((equal ?x0 ?x1)
	  (equal ?x1 ?y2)
	  (or (and (<= ?y2 ?y0) (> ?y3 ?y1))
	      (and (< ?y2 ?y0) (>= ?y3 ?y1)))))

 ((vertical-line ?x0 ?y0 ?x1 ?y1)
  :relations ((vertical-line-segment ?x0 ?y0 ?x1 ?y1)
	      (not (vertical-line-contained ?x0 ?y0 ?x1 ?y1))))


; Recognizing a Horizontal Line
 ((horizontal-line-segment ?x0 ?y0 ?x1 ?y1)
  :percepts ((ink ?ink0 xpos ?x0 ypos ?y0)
	     (ink ?ink1 xpos ?x1 ypos ?y1))
  :tests    ((equal ?y0 ?y1)
	     (equal (+ ?x0 1) ?x1)))

 ((horizontal-line-segment ?x0 ?y0 ?x1 ?y1)
  :percepts ((ink ?ink0 xpos ?x0 ypos ?y0)
	     (ink ?ink1 xpos ?x1 ypos ?y1)
	     (ink ?ink2 xpos ?xmid ypos ?ymid))
  :relations ((horizontal-line-segment ?x0 ?y0 ?xmid ?ymid)
	       (horizontal-line-segment ?xmid ?ymid ?x1 ?y1)))

 ((horizontal-line-contained ?x0 ?y0 ?x1 ?y1)
  :relations ((horizontal-line-segment ?x0 ?y0 ?x1 ?y1)
	      (horizontal-line-segment ?x2 ?y2 ?x3 ?y3))
  :tests ((equal ?y0 ?y1)
	  (equal ?y1 ?y2)
	  (or (and (<= ?x2 ?x0) (> ?x3 ?x1))
	      (and (< ?x2 ?x0) (>= ?x3 ?x1)))))

 ((horizontal-line ?x0 ?y0 ?x1 ?y1)
  :relations ((horizontal-line-segment ?x0 ?y0 ?x1 ?y1)
	      (not (horizontal-line-contained ?x0 ?y0 ?x1 ?y1))))


; Recognizing a Diagonal Line
; ((diagonal-line-segment ?x0 ?y0 ?x1 ?y1)
;  :percepts ((ink ?ink0 xpos ?x0 ypos ?y0)
;	     (ink ?ink1 xpos ?x1 ypos ?y1))
;  :tests    ((equal (+ ?y0 1) ?y1)
;	     (equal (+ ?x0 1) ?x1)))

; ((diagonal-line-segment ?x0 ?y0 ?x1 ?y1)
;  :percepts ((ink ?ink0 xpos ?x0 ypos ?y0)
;	     (ink ?ink1 xpos ?x1 ypos ?y1)
;	     (ink ?ink2 xpos ?xmid ypos ?ymid))
;  :relations ((diagonal-line-segment ?x0 ?y0 ?xmid ?ymid)
;	       (diagonal-line-segment ?xmid ?ymid ?x1 ?y1)))

; ((diagonal-line-contained ?x0 ?y0 ?x1 ?y1)
;  :relations ((diagonal-line-segment ?x0 ?y0 ?x1 ?y1)
;	      (diagonal-line-segment ?x2 ?y2 ?x3 ?y3))
;  :tests ((or (and (<= ?x2 ?x0) (> ?x3 ?x1))
;	      (and (< ?x2 ?x0) (>= ?x3 ?x1)))))

; ((diagonal-line ?x0 ?y0 ?x1 ?y1)
;  :relations ((diagonal-line-segment ?x0 ?y0 ?x1 ?y1)
;	      (not (diagonal-line-contained ?x0 ?y0 ?x1 ?y1))))


;;;; Other Concepts

; ON describes a situation in which one block is on top of another.
; ((on ?block1 ?block2)
;  :percepts ((block ?block1 xpos ?xpos1 ypos ?ypos1)
;	     (block ?block2 xpos ?xpos2 ypos ?ypos2 height ?height2))
;  :tests ((equal ?xpos1 ?xpos2) (= ?ypos1 (+ ?ypos2 ?height2))))

; ONTABLE describes a situation in which a block in sitting on the table.
; ((ontable ?block1 ?table)
;  :percepts ((block ?block1 xpos ?xpos1 ypos ?ypos1)
;	     (table ?table xpos ?xpos2 ypos ?ypos2 width ?width2
;		    height ?height2))
;  :tests ((<= ?xpos2 ?xpos1)
;	  (>= (+ ?xpos2 ?width2) ?xpos1)
;	  (>= ?ypos1 ?ypos2)
;	  (<= ?ypos1 (+ ?ypos2 ?height2))))

; HOLDING is satisfied when the hand is holding a block.
; ((holding ?block)
;  :percepts ((hand ?hand status ?block) (block ?block)))
)

; Create a set of primitive skill clauses for the Blocks World.
(create-skills

 ; lower the pen down
 ((lower-pen ?pen)
  :percepts ((pen ?pen))
  :conditions ((pen-up))
  :effects ((not (pen-up))
	    (pen-down))
  :action  (*place-pen-down ?pen))

 ; lift the pen up
 ((lift-pen ?pen)
  :percepts ((pen ?pen))
  :conditions ((pen-down))
  :effects ((not (pen-down))
	    (pen-up))
  :action   (*lift-pen-up ?pen))

 ; move the pen
 ((move-pen ?x ?y)
;  :percepts ((pen ?pen xpos ?x0 ypos ?y0))
  :conditions ((pen ?x0 ?y0)
	       (pen-down))
;  :effects ((line ?x0 ?y0 ?x ?y))
  :action (*move-pen-to ?x ?y))

 ((move-pen ?x ?y)
;  :percepts ((pen ?pen xpos ?x0 ypos ?y0))
  :conditions ((pen ?x0 ?y0)
	       (pen-up))
;  :effects ()
  :action (*move-pen-to ?x ?y))


; STACKED block on another block by moving and ungrasping the block.
; ((stack ?block ?to)
;  :percepts ((block ?block) (block ?to xpos ?x ypos ?y  height ?h))
;  :conditions ((holding ?block) (not (on ?any-block ?to)))
;  :effects ((on ?block ?to) (not (holding ?block)))
;  :action (*put-down-at-x-y ?block ?x (+ ?y ?h)))

; Achieve PUT-DOWN by moving and ungrasping a putdownable block.
; ((put-down ?block ?to)
;  :percepts ((block ?block width ?w) (table ?to xpos ?tx ypos ?ty height ?th)
;	     (marker FREE xpos ?x))
;  :conditions ((holding ?block))
;  :effects ((ontable ?block ?to) (not (holding ?block)))
;  :action (*put-down-at-x-y ?block ?x (+ ?ty ?th) ?w)) 

)
