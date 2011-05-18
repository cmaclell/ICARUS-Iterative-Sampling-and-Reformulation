(create-constraints

 ((city-reachable)
  :type BINDINGS-CONSTRAINT
  :goal-conditions ((truck-at ?truck ?location))
  :belief-conditions ((truck-at ?truck ?curr-location)
  		      (different-city ?location ?curr-location)))
)