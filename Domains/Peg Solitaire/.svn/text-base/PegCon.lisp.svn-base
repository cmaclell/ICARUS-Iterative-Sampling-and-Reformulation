
(create-concepts

((peg ?cell)
 :percepts  ((cell ?cell peg? t)))

((hole ?cell)
 :percepts  ((cell ?cell peg? nil)))


((up ?from ?to)
 :percepts ((cell ?from row ?from-row col ?from-col)
	    (cell ?to row ?to-row col ?to-col))
 :tests ((= ?from-col ?to-col)
	 (= -1 (- ?to-row ?from-row))))

((down ?from ?to)
 :percepts ((cell ?from row ?from-row col ?from-col)
	    (cell ?to row ?to-row col ?to-col))
 :tests ((= ?from-col ?to-col)
	 (= 1 (- ?to-row ?from-row))))

((right ?from ?to)
 :percepts ((cell ?from row ?from-row col ?from-col)
	    (cell ?to row ?to-row col ?to-col))
 :tests ((= ?from-row ?to-row)
	 (= 1 (- ?to-col ?from-col))))

((left ?from ?to)
 :percepts ((cell ?from row ?from-row col ?from-col)
	    (cell ?to row ?to-row col ?to-col))
 :tests ((= ?from-row ?to-row)
	 (= -1 (- ?to-col ?from-col))))

((three-lined-right ?first ?second ?third)
 :relations ((right ?first ?second)
	     (right ?second ?third)))

((three-lined-left ?first ?second ?third)
 :relations ((left ?first ?second)
	     (left ?second ?third)))

((three-lined-up ?first ?second ?third)
 :relations ((up ?first ?second)
	     (up ?second ?third)))

((three-lined-down ?first ?second ?third)
 :relations ((down ?first ?second)
	     (down ?second ?third)))

)


(create-skills

((jump-up ?from ?over ?to)
 :conditions ((three-lined-up ?from ?over ?to)
	      (peg ?from) (peg ?over) (hole ?to))
 :effects    ((hole ?from) (hole ?over) (peg ?to))
 :action     (*jump-up ?from ?over ?to))

((jump-down ?from ?over ?to)
 :conditions ((three-lined-down ?from ?over ?to)
	      (peg ?from) (peg ?over) (hole ?to))
 :effects    ((hole ?from) (hole ?over) (peg ?to))
 :action     (*jump-down ?from ?over ?to))

((jump-right ?from ?over ?to)
 :conditions ((three-lined-right ?from ?over ?to)
	      (peg ?from) (peg ?over) (hole ?to))
 :effects    ((hole ?from) (hole ?over) (peg ?to))
 :action     (*jump-right ?from ?over ?to))

((jump-left ?from ?over ?to)
 :conditions ((three-lined-left ?from ?over ?to)
	      (peg ?from) (peg ?over) (hole ?to))
 :effects    ((hole ?from) (hole ?over) (peg ?to))
 :action     (*jump-left ?from ?over ?to))

)