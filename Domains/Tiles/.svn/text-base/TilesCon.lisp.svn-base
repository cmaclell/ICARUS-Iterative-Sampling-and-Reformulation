; ************************************************************************
; ************************************************************************
; TILES.L
; Icarus concepts and skills for Tiles and Squares puzzle.
; Copyright (C) 2006 Institute for the Study of Learning and Expertise
; ************************************************************************
; ************************************************************************

(create-concepts

((on ?tile ?square)
 :percepts  ((tile ?tile xpos ?xpos)
             (square ?square xpos ?xpos)))

((clear ?square)
 :percepts ((square ?square))
 :relations ((not (on ?any ?square))))

#|
((on-and-clear ?tile ?square1 ?square2)
 :percepts  ((tile ?tile) (square ?square1) (square ?square2))
 :relations ((on ?tile ?square1)
             (not (on ?any ?square2))))

((two-on ?tile1 ?square1 ?tile2 ?square2)
 :percepts  ((tile ?tile1) (tile ?tile2)
             (square ?square1) (square ?square2))
 :relations ((on ?tile1 ?square1)
             (on ?tile2 ?square2)))
|#

)

(create-skills

((move-tile ?tile ?from ?to)
 :percepts   ((tile ?tile) (square ?from) (square ?to))
 :conditions ((on ?tile ?from) (clear ?to))
 :effects    ((on ?tile ?to) (not (on ?tile ?from)) (clear ?from) (not (clear ?to)))
 :action     (*move ?tile ?from ?to)
 )
 
#|
((on-and-clear ?tile ?to ?from)
 :percepts  ((tile ?tile) (square ?from) (square ?to))
 :start     ((on-and-clear ?tile ?from ?to))
 :actions   ((*move ?tile ?from ?to)))
|#

)

; (create-goals (on A S1))

; (create-goals (two-on A S1 B S2))

; (create-goals (on A S1) (on B S2) (on C S3))

; (defun l () (clear)(load "tiles.l"))
