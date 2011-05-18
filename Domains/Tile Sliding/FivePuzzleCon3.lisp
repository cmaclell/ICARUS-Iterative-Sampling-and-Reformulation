;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-concepts

((cell ?cell)
 :percepts ((cell ?cell)))

((different-cells ?cell1 ?cell2)
 :percepts ((cell ?cell1) (cell ?cell2)))

((tile ?tile)
 :percepts ((tile ?tile)))

((on ?tile ?cell)
 :percepts  ((cell ?cell)
             (tile ?tile position ?cell)))

((blank ?cell)
 :percepts ((blank ?blank position ?cell)))

((neighbor ?from ?to)
 :percepts ((cell ?from row ?from-row col ?from-col)
	    (cell ?to row ?to-row col ?to-col))
 :tests ((= 1 (+ (abs (- ?from-row ?to-row))
		 (abs (- ?from-col ?to-col))))))

((four-cells-in-square ?c11 ?c12 ?c21 ?c22)
 :relations ((cell ?c11) (cell ?c12) (cell ?c21) (cell ?c22)
	     (neighbor ?c11 ?c12) (neighbor ?c12 ?c22)
	     (neighbor ?c22 ?c21) (neighbor ?c21 ?c11)
	     (different-cells ?c11 ?c22) (different-cells ?c12 ?c21)))

)


(create-skills

((move-blank ?from-cell ?to-cell)
 :percepts   ((tile ?tile) (cell ?from-cell) (cell ?to-cell)) 
 :conditions ((on ?tile ?to-cell) (blank ?from-cell)
              (neighbor ?from-cell ?to-cell))
 :effects    ((on ?tile ?from-cell) (not (on ?tile ?to-cell))
	      (blank ?to-cell) (not (blank ?from-cell)))
 :action     (*move-blank ?to-cell))

;; Macro skills

; note that ?from-cell and ?to-cell will necessarily be different,
;    since first is blank, and 2nd has tile
((move-blank-2 ?from-cell ?via-cell ?to-cell)
 :conditions ((cell ?from-cell) (cell ?via-cell) (cell ?to-cell)
	      (neighbor ?from-cell ?via-cell) (neighbor ?via-cell ?to-cell)
	      (different-cells ?from-cell ?to-cell)
	      (blank ?from-cell)
	      (on ?via-tile ?via-cell) (on ?to-tile ?to-cell))
 :effects ((blank ?to-cell) (not (blank ?from-cell))
	   (not (on ?via-tile ?via-cell)) (on ?via-tile ?from-cell)
	   (not (on ?to-tile ?to-cell)) (on ?to-tile ?via-cell))
 :subskills ((move-blank ?from-cell ?via-cell)
	     (move-blank ?via-cell ?to-cell)))

;; note blank must be at ?to-cell for this macro
;;    and this forces ?to-cell and ?from-cell to be distinct
;;    so the 2 squares overlap but are not the same
((move-tile-2-in-line ?from-cell ?to-cell)
 :conditions ((four-cells-in-square ?via-cell ?from-cell
				    ?via-neighbor ?from-neighbor)
	      (four-cells-in-square ?to-cell ?via-cell
				    ?to-neighbor ?via-neighbor)
	      (different-cells ?from-cell ?to-cell)
	      (blank ?to-cell)
	      (on ?from-tile ?from-cell) (on ?via-tile ?via-cell)
	      (on ?from-neighbor-tile ?from-neighbor)
	      (on ?via-neighbor-tile ?via-neighbor)
	      (on ?to-neighbor-tile ?to-neighbor))
 :effects ((blank ?via-cell)
	   (on ?from-tile ?to-cell) (on ?via-tile ?to-neighbor)
	   (on ?from-neighbor-tile ?from-cell)
	   (on ?via-neighbor-tile ?from-neighbor)
	   (on ?to-neighbor-tile ?via-neighbor)
	   (not (blank ?to-cell))
	   (not (on ?from-tile ?from-cell) (not (on ?via-tile ?via-cell)))
	   (not (on ?from-neighbor-tile ?from-neighbor))
	   (not (on ?via-neighbor-tile ?via-neighbor))
	   (not (on ?to-neighbor-tile ?to-neighbor)))
 :subskills ((move-blank-2 ?to-cell ?via-cell ?from-cell)
	     (move-blank-2 ?from-cell ?from-neighbor ?via-neighbor)
	     (move-blank-2 ?via-neighbor ?to-neighbor ?to-cell)
	     (move-blank ?to-cell ?via-cell)))
 
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;