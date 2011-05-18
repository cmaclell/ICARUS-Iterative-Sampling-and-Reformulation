; Logistics-con.LISP
; Icarus concepts, primitive skills, and goals for the Logistics.

; Create a set of conceptual clauses for the Logistics.
(create-concepts
 
 ((location ?location)
  :percepts ((location ?location)))

 ((airport ?location)
  :percepts ((location ?location type airport)))

 ((same-city ?location1 ?location2)
    :percepts ((location ?location1 city ?city)
 	       (location ?location2 city ?city)))
   
 ((different-city ?location1 ?location2)
   :percepts ((location ?location1 city ?city1)
	     (location ?location2 city ?city2))
   :tests ((not (eq ?city1 ?city2))))

 ((truck ?truck)
  :percepts ((truck ?truck)))

 ((airplane ?airplane)
  :percepts ((airplane ?airplane)))

 ((package ?package)
  :percepts ((package ?package)))
 
 ((truck-at ?truck ?location)
  :percepts ((truck ?truck at ?location)
	     (location ?location)))
	     
 ((airplane-at ?plane ?location)
  :percepts ((airplane ?plane at ?location)
	     (location ?location)))

 ((package-at ?package ?location)
  :percepts ((package ?package at ?location)
	     (location ?location)))

 ((in ?package ?vehicle)
  :percepts ((package ?package inside ?vehicle)
	     (truck ?vehicle)))

 ((in ?package ?vehicle)
  :percepts ((package ?package inside ?vehicle)
	     (airplane ?vehicle)))
)

;Create a set of primitive skill clauses for the Logistics.
(create-skills

 ((load ?package ?truck ?location)
  :conditions ((package ?package)
	       (truck ?truck)
	       (location ?location)
	       (package-at ?package ?location)
	       (truck-at ?truck ?location))
  :effects ((in ?package ?truck))
  :action (*load-package ?package ?truck))

 ((load ?package ?plane ?location)
  :conditions ((package ?package)
	       (airplane ?plane)
	       (airport ?location)
	       (package-at ?package ?location)
	       (airplane-at ?plane ?location))
  :effects ((in ?package ?plane))
  :action (*load-package ?package ?plane))

 ((unload ?package ?truck ?location)
  :conditions ((package ?package)
	       (truck ?truck)
	       (location ?location)
	       (truck-at ?truck ?location)
	       (in ?package ?truck))
  :effects ((not (in ?package ?truck))
  	    (package-at ?package ?location))
  :action (*unload-package ?package ?location))

 ((unload ?package ?plane ?location)
  :conditions ((package ?package)
	       (airplane ?plane)
	       (airport ?location)
	       (airplane-at ?plane ?location)
	       (in ?package ?plane))
  :effects ((not (in ?package ?plane))
  	    (package-at ?package ?location))
  :action (*unload-package ?package ?location))

 ((drive ?truck ?from ?to)
  :conditions ((truck ?truck)
	       (location ?from)
	       (location ?to)
	       (same-city ?from ?to)
	       (truck-at ?truck ?from))
  :effects ((truck-at ?truck ?to)
	    (not (truck-at ?truck ?from)))
  :action (*drive ?truck ?to))

 ((fly ?plane ?from ?to)
  :conditions ((airplane ?plane)
	       (airport ?from)
	       (airport ?to)
	       (airplane-at ?plane ?from))
  :effects ((airplane-at ?plane ?to)
	    (not (airplane-at ?plane ?from)))
  :action (*fly ?plane ?to))
)