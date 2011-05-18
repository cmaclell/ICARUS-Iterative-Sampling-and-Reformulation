
(create-concepts

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

)


(create-skills

((move-blank ?from-cell ?to-cell)
 :percepts   ((tile ?tile) (cell ?from-cell) (cell ?to-cell)) 
 :conditions ((on ?tile ?to-cell) (blank ?from-cell))
 :effects    ((on ?tile ?from-cell) (not (on ?tile ?to-cell))
	      (blank ?to-cell) (not (blank ?from-cell)))
 :action     (*move-blank ?to-cell))

)