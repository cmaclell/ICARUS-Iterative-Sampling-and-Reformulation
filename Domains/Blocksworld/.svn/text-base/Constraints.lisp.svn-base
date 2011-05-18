(create-constraints

 ((order-goals)
  :type GOAL-CONSTRAINT
  :goal-conditions ((on ?x ?y)
		    (on ?y ?z))
  :delete ((on ?x ?y)))

 ((clear-block)
  :type GOAL-CONSTRAINT
  :goal-conditions ((on ?x ?y))
  :belief-conditions ((on ?w ?x))
  :add ((not (on ?w ?x))))
)