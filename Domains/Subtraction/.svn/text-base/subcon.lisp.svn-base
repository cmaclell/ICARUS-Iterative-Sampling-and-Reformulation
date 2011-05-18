
(create-concepts

; Columns look like:
; (column ?col xpos ?x top ?t bottom ?b below ?a decremented ?d)

;; Primitive concepts

((column ?col)
 :percepts ((column ?col)))

((top-greater ?col)
 :percepts ((column ?col top ?t bottom ?b))
 :tests ((not (null ?b))
         (>= ?t ?b)))

((top-greater ?col)
 :percepts ((column ?col bottom ?b))
 :tests ((null ?b)))

;; use (not (top-greater ?col)) instead
; ((top-lesser ?col)
;   :percepts ((column ?col top ?t bottom ?b))
;   :tests ((not (null ?b))
;           (< ?t ?b))
;    )

((top-positive ?col)
 :percepts ((column ?col top ?t))
 :tests ((> ?t 0))
 )

((decremented ?left)
 :percepts ((column ?left decremented T))
 )

((added-ten ?right)
 :percepts ((column ?right top ?t decremented T))
 :tests ((>= ?t 9))
; :relations ((decremented ?right))
 )

((added-ten ?right)
 :percepts ((column ?right top ?t))
 :tests ((>= ?t 10))
; :relations ((top-positive ?right))
 )

((left-of ?left ?right)
 :percepts ((column ?left  xpos ?xpos1)
            (column ?right xpos ?xpos2))
 :tests ((> ?xpos1 ?xpos2)))

((next-left ?left ?right)
 :percepts ((column ?left  xpos ?xpos1)
            (column ?right xpos ?xpos2))
 :tests ((= ?xpos1 (1+ ?xpos2))))

((processed ?col)
 :percepts ((column ?col below ?bel))
 :tests ((neq ?bel nil)))

((unprocessed ?col)
 :percepts ((column ?col below ?bel))
 :tests ((eq ?bel nil)))

((bottom-null ?col)
 :percepts ((column ?col bottom ?b))
 :tests ((null ?b)))

;; Higher-level concepts

((all-processed)
 :relations ((not (unprocessed ?col))))

((borrowed ?left ?right)
 :relations ((next-left ?left ?right)
             (decremented ?left)
             (added-ten ?right)))

((left-of-unprocessed ?left ?right)
 :relations ((left-of ?left ?right)
             (unprocessed ?right)))

((rightmost-unprocessed ?col)
 :relations ((column ?col)
	     (not (left-of-unprocessed ?col ?other-col))
             (unprocessed ?col)))

)


(create-skills

;; the allowed basic actions are: *find-diff, *add-ten, *sub-one

;;; Primitive Skills

((process-column ?col)
 :percepts ((column ?col))
 :conditions (;(rightmost-unprocessed ?col)
	      (not (bottom-null ?col))
	      (top-greater ?col))
 :effects ((processed ?col))
 :action (*find-diff ?col)
 )

((process-column ?col)
 :percepts ((column ?col top ?t))
 :conditions (;(rightmost-unprocessed ?col)
	      (bottom-null ?col)
	      (top-greater ?col))
 :effects ((processed ?col))
 :action (*find-diff ?col)
 )

((decrement-column ?col)
 :percepts ((column ?col))
 :conditions ((top-positive ?col)
	      (unprocessed ?col)
	      (not (decremented ?col)))
 :effects ((decremented ?col))
 :action (*sub-one ?col)
 )

((add-ten ?col)
 :percepts ((column ?col top ?t2))
 :conditions ((unprocessed ?col)
	      (not (top-greater ?col))
	      (not (added-ten ?col)))
 :effects ((added-ten ?col))
 :action (*add-ten ?col)
 )

;;; Macro Skills

;; when only the left-most is unprocessed
((process-all-columns)
 :conditions ((column ?col)
	      (rightmost-unprocessed ?col)
	      (not (left-of ?any ?col))
	      ;; do we need to say rightmost?  Apparently we do.
	      )
 :effects ((all-processed))
 :subskills ((process-column ?col))
 )

;; when there is more than one column unprocessed
((process-all-columns)
 :conditions ((column ?col)
	      (rightmost-unprocessed ?col)
	      (column ?left)
	      (left-of ?left ?col)
	      (unprocessed ?left)
	      ;; do we need to say rightmost?  Apparently we do.
	      )
 :effects ((all-processed))
 :subskills ((process-column ?col)
	     (process-all-columns))
 )

((process-column ?col)
 :conditions (;(rightmost-unprocessed ?col)
	 (column ?col)
	 (column ?left)
         (next-left ?left ?col)
         (not (top-greater ?col)))
 :effects ((processed ?col))
 :subskills ((borrow ?left ?col)
	     (process-column ?col))
 )

((borrow ?left ?right)
 :conditions ((column ?left)
	      (column ?right)
	      (unprocessed ?right)
	      ; (top-lesser ?right)
	      (top-positive ?left))
 :effects ((borrowed ?left ?right))
 :subskills ((decrement-column ?left)
	     (add-ten ?right))
 )

((borrow ?left ?right)
 :conditions ((column ?left)
	      (column ?right)
	      (unprocessed ?right)
	      ; (next-left ?lefter ?left)
	      ; (top-lesser ?right)
	      (not (top-positive ?left)))
 :effects ((borrowed ?left ?right))
 :subskills ((make-top-positive ?left)
	     ;; (borrowed ?lefter ?left)
	     ;; (top-greater ?left)
	     (borrow ?left ?right)
	     ;; (top-greater ?right)
            )
 )

((make-top-positive ?col)
 :conditions ((column ?col)
	      (column ?lefter)
	      (unprocessed ?col)
	      (next-left ?lefter ?col)
	      ; (top-lesser ?col)
	      (not (top-positive ?col)))
 :effects ((top-positive ?col))
 :subskills ((borrow ?lefter ?col))
 )

)


;;; set up problemd
(create-problems ((all-processed)))
