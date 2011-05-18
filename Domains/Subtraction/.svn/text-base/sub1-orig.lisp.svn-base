
(create-concepts

; Columns look like:
; (column ?col xpos ?x top ?t bottom ?b below ?a decremented ?d)


((top-greater ?col)
 :percepts ((column ?col top ?t bottom ?b))
 :tests ((not (null ?b))
         (>= ?t ?b))
 )

((top-greater ?col)
 :percepts ((column ?col))
 :relations ((bottom-null ?col))
 )

((top-lesser ?col)
 :percepts ((column ?col top ?t bottom ?b))
 :tests ((not (null ?b))
         (< ?t ?b))
 )

((top-positive ?col)
 :percepts ((column ?col top ?t))
 :tests ((> ?t 0))
 )

((decremented ?left)
 :percepts ((column ?left decremented T))
 )

#|((decremented ?left)
 :percepts ((column ?left decremented ?td))
 :tests ((eq ?td t))
 )
|#

((added-ten ?right)
 :percepts ((column ?right top ?t))
 :tests ((>= ?t 9))
 :relations ((decremented ?right))
 )

((added-ten ?right)
 :percepts ((column ?right top ?t))
 :tests ((>= ?t 10))
; :relations ((top-positive ?right))
 )

((left-of ?left ?right)
 :percepts ((column ?left  xpos ?xpos1)
            (column ?right xpos ?xpos2))
 :tests ((> ?xpos1 ?xpos2))
 )

((next-left ?left ?right)
 :percepts ((column ?left  xpos ?xpos1)
            (column ?right xpos ?xpos2))
 :tests ((= ?xpos1 (1+ ?xpos2)))
 )

((borrowed ?left ?right)
 :relations ((next-left ?left ?right)
             (decremented ?left)
             (added-ten ?right))
 )

((processed ?col)
 :percepts ((column ?col below ?bel))
 :tests ((neq ?bel nil))
 )

((unprocessed ?col)
 :percepts ((column ?col below ?bel))
 :tests ((eq ?bel nil))
 )

((left-of-unprocessed ?left ?right)
 :percepts ((column ?left)
            (column ?right))
 :relations ((left-of ?left ?right)
             (unprocessed ?right))
 )

((rightmost-unprocessed ?col)
 :percepts ((column ?col))
 :relations ((not (left-of-unprocessed ?col ?other-col))
             (unprocessed ?col))
 )

((all-processed)
; :percepts ((column ?col))
 :relations ((not (unprocessed ?col))))

((bottom-null ?col)
 :percepts ((column ?col bottom ?b))
 :tests ((null ?b)))

)


(create-skills

;; the allowed basic actions are: *find-diff, *add-ten, *sub-one


((all-processed)
 :percepts ((column ?col)
;            (column ?lefterer)
            )            
 :start ((rightmost-unprocessed ?col)
         ; do we need to say rightmost?  Apparently we do.
         )
 :subgoals ((processed ?col) (all-processed))
 )

((processed ?col)
 :percepts ((column ?col top ?t bottom ?b))
 :start (;(rightmost-unprocessed ?col)
         (not (bottom-null ?col))
         (top-greater ?col))
 :actions ((*find-diff ?col ?t ?b))
 )

((processed ?col)
 :percepts ((column ?col top ?t))
 :start (;(rightmost-unprocessed ?col)
         (bottom-null ?col)
         (top-greater ?col))
 :actions ((*find-diff ?col ?t 0))
 )

((processed ?col)
 :percepts ((column ?col)
            (column ?left))
 :start (;(rightmost-unprocessed ?col)
         (next-left ?left ?col)
         (top-lesser ?col))
 :subgoals ((borrowed ?left ?col)
;            (top-greater ?col)
            (processed ?col))
 )

((borrowed ?left ?right)
 :percepts ((column ?left)
            (column ?right))
 :start ((unprocessed ?right)
;         (top-lesser ?right)
         (top-positive ?left))
 :subgoals ((decremented ?left)
            (added-ten ?right)
;            (top-greater ?right)
            )
 )

((borrowed ?left ?right)
 :percepts ((column ?left)
            (column ?right)
;            (column ?lefter)
            )
 :start ((unprocessed ?right)
;         (next-left ?lefter ?left)
;         (top-lesser ?right)
         (not (top-positive ?left)))
 :subgoals ((top-positive ?left)
;            (borrowed ?lefter ?left)
;            (top-greater ?left)
            (borrowed ?left ?right)
;            (top-greater ?right)
            )
 )

((top-positive ?col)
 :percepts ((column ?col)
            (column ?lefter))
 :start ((unprocessed ?col)
         (next-left ?lefter ?col)
;         (top-lesser ?col)
         (not (top-positive ?col)))
 :subgoals ((borrowed ?lefter ?col)
;            (top-greater ?col)
;            (top-positive ?col)
            )
 )

((decremented ?left)
 :percepts ((column ?left top ?t1))
 :start ((top-positive ?left)
         (unprocessed ?left)
         (not (decremented ?left)))
 :actions ((*sub-one ?left ?t1))
 )

((added-ten ?col)
 :percepts ((column ?col top ?t2))
 :start ((unprocessed ?col)
         (top-lesser ?col)
         (not (added-ten ?col)))
 :actions ((*add-ten ?col ?t2))
 )


)



(create-goals (all-processed))
