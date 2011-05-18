; gripper-con.LISP
; Icarus concepts, primitive skills, and goals for the Gripper domain.

(create-concepts

 ((room ?room)
  :percepts ((room ?room)))

 ((ball ?ball)
  :percepts ((ball ?ball)))

 ((robot ?robot)
  :percepts ((robot ?robot)))

 ((gripper ?gripper)
  :percepts ((gripper ?gripper)))

 ((in ?ball ?room)
  :percepts ((ball ?ball in ?room)
	     (room ?room)))

 ((at ?robot ?room)
  :percepts ((robot ?robot at ?room)
	     (room ?room)))

 ((holding ?gripper ?ball)
  :percepts ((gripper ?gripper holding ?ball)
	     (ball ?ball)))

 ((empty ?gripper)
  :percepts ((gripper ?gripper holding nil)))

 ((holding-maximum-objects ?robot ?room)
  :percepts ((robot ?robot)
	     (room ?room))
  :relations ((at ?robot ?room)
	      (not (empty ?any-gripper))))
 
 ((holding-maximum-objects ?robot ?room)
  :percepts ((room ?room)
	     (robot ?robot)
	     (gripper ?gripper))
  :relations ((at ?robot ?room)
	      (empty ?gripper)
	      (not (in ?any-object ?room))))

 ((holding-nothing ?robot)
  :percepts ((robot ?robot))
  :relations ((not (holding ?any-gripper ?any-object))))

 ((moved-objects ?from-room ?to-room)
  :percepts ((room ?from-room)
	     (room ?to-room)
	     (ball ?ball))
  :relations ((in ?ball ?to-room)
	      (not (in ?ball ?from-room))))
)

(create-skills

 ((move ?robot ?from ?to)
  :conditions ((robot ?robot)
	       (room ?from)
	       (room ?to)
	       (at ?robot ?from))
  :effects ((not (at ?robot ?from))
	    (at ?robot ?to))
  :action (*move ?robot ?to))

 ((pick-up ?ball ?room ?gripper)
  :conditions ((ball ?ball)
	       (room ?room)
	       (gripper ?gripper)
	       (robot ?robot)
	       (in ?ball ?room)
	       (at ?robot ?room)
	       (empty ?gripper))
  :effects ((holding ?gripper ?ball)
	    (not (empty ?gripper))
	    (not (in ?ball ?room)))
  :action (*pick-up ?ball ?gripper))
		 
 ((drop-one-object)
  :conditions ((ball ?ball)
	       (room ?room)
	       (gripper ?gripper)
	       (robot ?robot)
	       (holding ?gripper ?ball)
	       (at ?robot ?room))
  :effects ((not (holding ?gripper ?ball))
	    (empty ?gripper)
	    (in ?ball ?room))
  :action (*drop ?ball ?room ?gripper))

 ((move-objects ?from-room ?to-room)
  :conditions ((room ?from-room)
	       (room ?to-room)
	       (robot ?robot)
	       (at ?robot ?from-room)
	       (holding-maximum-objects ?robot ?from-room))
  :effects ((moved-objects ?from-room ?to-room))
  :subskills ((move ?robot ?from-room ?to-room)
	      (dropped-all-objects ?robot)))

 ((dropped-all-objects ?robot)
  :conditions ((robot ?robot)
	       (not (holding-nothing ?robot)))
  :effects ((holding-nothing ?robot))
  :subskills ((drop-one-object)))
)