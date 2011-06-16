;; need to add rules that implement the sorts used throughout the
;; original shopping knowledge base.

;; this knowledge base is broken. for example, the only back-chaining
;; possible off of (liquor-store X) leads to (liqst-shopping
;; Y). there's no equivalent liqst-robbing that would connect the
;; robbing action to the liquor store.

(defparameter *shopping-rules* '(

;; shopping

(=> (and (shopping ?s) (go-step ?s ?g)) 
 (going ?g))
(=> (and (shopping ?s) (go-step ?s ?g) (shopper ?s ?p)) 
 (goer ?g ?p))
(=> (and (shopping ?s) (go-step ?s ?g) (store ?s ?str)) 
 (dest-go ?g ?str))
(=> (and (shopping ?s) (store ?s ?sp)) 
 (shopping-place ?sp))

(=> (and (shopping ?s) (find-step ?s ?f)) 
 (finding ?f))
(=> (and (shopping ?s) (find-step ?s ?f) (shopper ?s ?a)) 
 (finder ?f ?a))
(=> (and (shopping ?s) (find-step ?s ?f) (thing-shopped-for ?s ?tf)) 
 (thing-found ?f ?tf))

(=> (and (shopping ?s) (buy-step ?s ?b)) 
 (buying ?b))
(=> (and (shopping ?s) (buy-step ?s ?b) (shopper ?s ?p)) 
 (buyer ?b ?p))
(=> (and (shopping ?s) (buy-step ?s ?b) (thing-shopped-for ?s ?tb)) 
 (thing-bought ?tb))

(=> (and (buying ?b) (pay-step ?b ?p)) 
 (paying ?p))
(=> (and (buying ?b) (pay-step ?b ?p) (buyer ?b ?a)) 
 (payer ?p ?a))
(=> (and (buying ?b) (pay-step ?b ?p) (thing-bought ?b ?tp)) 
 (thing-paid ?p ?tp))

(=> (and (smarket-shopping ?s) (store ?s ?str)) 
 (smarket ?str))
(=> (and (smarket-shopping ?s) (thing-shopped-for ?s ?f)) 
 (food ?f))

(=> (and (liqst-shopping ?s) (store ?s ?ls)) 
 (liquor-store ?ls))
(=> (and (liqst-shopping ?s) (thing-shopped-for ?s ?l)) 
 (liquor ?l))

;; robbing

(=> (and (robbing ?r) (get-weapon-step ?r ?g)) 
 (getting ?g))
(=> (and (robbing ?r) (get-weapon-step ?r ?g) (robber ?r ?a))
 (agent-get ?g ?a))
(=> (and (robbing ?r) (get-weapon-step ?r ?g) (weapon-rob ?r ?w)) 
 (patient-get ?g ?w))

(=> (and (robbing ?r) (go-step ?r ?g)) 
 (going ?g))
(=> (and (robbing ?r) (go-step ?r ?g) (robber ?r ?a)) 
 (goer ?g ?a))
(=> (and (robbing ?r) (go-step ?r ?g) (place-rob ?r ?p)) 
 (dest-go ?g ?p))

(=> (and (robbing ?r) (point-weapon-step ?r ?p)) 
 (pointing ?p))
(=> (and (robbing ?r) (point-weapon-step ?r ?p) (robber ?r ?a)) 
 (agent-point ?p ?a))
(=> (and (robbing ?r) (point-weapon-step ?r ?p) (victim-rob ?r ?a))
 (patient-point ?p ?a))
(=> (and (robbing ?r) (point-weapon-step ?r ?p) (weapon-rob ?r ?i))
 (instr-point ?p ?i))
(=> (and (robbing ?r) (weapon-rob ?r ?i)) 
 (weapon ?i))

(=> (and (robbing ?r) (get-valuable-step ?r ?g)) 
 (getting ?g))
(=> (and (robbing ?r) (get-valuable-step ?r ?g) (robber ?r ?a)) 
 (agent-get ?g ?a))
(=> (and (robbing ?r) (get-valuable-step ?r ?g) (thing-robbed ?r ?t)) 
 (patient-get ?g ?t))
(=> (and (robbing ?r) (get-valuable-step ?r ?g) (victim-rob ?r ?a)) 
 (from-get ?g ?a))
(=> (and (robbing ?r) (thing-robbed ?r ?t)) 
 (valuable ?t))

;; restaurant dining

(=> (and (rest-dining ?d) (go-step ?d ?g))
 (going ?g))
(=> (and (rest-dining ?d) (go-step ?d ?g) (diner ?d ?a))
 (goer ?g ?a))
(=> (and (rest-dining ?d) (go-step ?d ?g) (restaurant-name ?d ?r))
 (dest-go ?g ?r))
(=> (and (rest-dining ?d) (restaurant-name ?d ?r))
 (restaurant ?r))

(=> (and (rest-dining ?d) (order-step ?d ?o))
 (ordering ?o))
(=> (and (rest-dining ?d) (order-step ?d ?o) (diner ?d ?a))
 (agent-order ?o ?a))
(=> (and (rest-dining ?d ) (order-step ?d ?o) (rest-thing-ordered ?d ?p))
 (patient-order ?o ?p))

(=> (and (rest-dining ?d) (drink-step ?d ?o))
 (drinking ?o))
(=> (and (rest-dining ?d) (drink-step ?d ?o) (diner ?d ?a))
 (drinker ?o ?a))
(=> (and (rest-dining ?d) (drink-step ?d ?o) (rest-thing-drunk ?d ?p))
 (patient-drink ?o ?p))
(=> (and (rest-dining ?d) (drink-step ?d ?o) (rest-drink-straw ?d ?p))
 (instr-drink ?o ?p))

(=> (and (rest-dining ?d) (pay-step ?d ?o))
 (paying ?o))
(=> (and (rest-dining ?d) (pay-step ?d ?o) (diner ?d ?a))
 (payer ?o ?a))
(=> (and (rest-dining ?d) (pay-step ?d ?o) (rest-thing-ordered ?d ?p))
 (thing-paid ?o ?p))

(=> (and (drinking ?d) (get-straw-step ?d ?g))
 (getting ?g))
(=> (and (drinking ?d) (get-straw-step ?d ?g) (drinker ?d ?a))
 (agent-get ?g ?a))
(=> (and (drinking ?d) (get-straw-step ?d ?g) (instr-drink ?d ?p))
 (patient-get ?g ?p))

(=> (and (drinking ?d) (put-straw-step ?d ?p))
 (putting ?p))
(=> (and (drinking ?d) (put-straw-step ?d ?p) (drinker ?d ?a))
 (agent-put ?p ?a))
(=> (and (drinking ?d) (put-straw-step ?d ?p) (instr-drink ?d ?a))
 (patient-put ?p ?a))
(=> (and (drinking ?d) (instr-drink ?d ?a))
 (straw ?a))
(=> (and (drinking ?d) (put-straw-step ?d ?p) (patient-drink ?d ?a))
 (place-put ?p ?a))

(=> (and (drinking ?d) (ingest-step ?d ?i))
 (ingesting ?i))
(=> (and (drinking ?d) (ingest-step ?d ?i) (drinker ?d ?a))
 (agent-ingest ?i ?a))
(=> (and (drinking ?d) (ingest-step ?d ?i) (patient-drink ?d ?p))
 (patient-ingest ?i ?p))
(=> (and (drinking ?d) (ingest-step ?d ?i) (instr-drink ?d ?p))
 (instr-ingest ?i ?p))

;; going-by-vehicle

(=> (and (going-by-vehicle ?v) (go-step ?v ?g))
 (going ?g))
(=> (and (going-by-vehicle ?v) (go-step ?v ?g) (goer ?v ?a))
 (goer ?g ?a))
(=> (and (going-by-vehicle ?v) (go-step ?v ?g) (source-go ?v ?s))
 (dest-go ?g ?s))
    
(=> (and (going-by-vehicle ?v) (get-on-step ?v ?o))
 (getting-on ?o))
(=> (and (going-by-vehicle ?v) (get-on-step ?v ?o) (goer ?v ?a))
 (agent-get-on ?o ?a))
(=> (and (going-by-vehicle ?v) (get-on-step ?v ?o) (vehicle ?v ?w))
 (patient-get-on ?o ?w))
(=> (and (going-by-vehicle ?v) (get-on-step ?v ?o) (source-go ?v ?p))
 (place-get-on ?o ?p))
(=> (and (going-by-vehicle ?v) (vehicle ?v ?w))
 (instance-vehicle ?w))
    
(=> (and (going-by-vehicle ?v) (sit-step ?v ?s))
 (sitting ?s))
(=> (and (going-by-vehicle ?v) (sit-step ?v ?s) (goer ?v ?a))
 (agent-sit ?s ?a))
(=> (and (going-by-vehicle ?v) (sit-step ?v ?s) (vehicle-seat ?v ?p))
 (patient-sit ?s ?p))
(=> (and (going-by-vehicle ?v) (vehicle-seat ?v ?p))
 (seat ?p))
(=> (and (going-by-vehicle ?v) (vehicle-seat ?v ?p) (vehicle ?v ?w))
 (in ?p ?w))

(=> (and (going-by-vehicle ?v) (get-off-step ?v ?o))
 (getting-off ?o))
(=> (and (going-by-vehicle ?v) (get-off-step ?v ?o) (goer ?v ?a))
 (agent-get-off ?o ?a))
(=> (and (going-by-vehicle ?v) (get-off-step ?v ?o) (vehicle ?v ?w))
 (patient-get-off ?o ?w))
(=> (and (going-by-vehicle ?v) (get-off-step ?v ?o) (dest-go ?v ?p))
 (place-get-off ?o ?p))

;; going-by-bus, -taxi, -plane

(=> (and (going-by-bus ?b) (vehicle ?b ?v))
 (bus ?v))
(=> (and (going-by-taxi ?t) (vehicle ?t ?v))
 (taxi ?v))
(=> (and (going-by-plane ?p) (vehicle ?p ?v))
 (plane ?v))

;; going-by-bus
    
(=> (and (going-by-bus ?v) (source-go ?v ?s))
 (bus-station ?s))

(=> (and (going-by-bus ?b) (give-token-step ?b ?g))
 (giving ?g))
(=> (and (going-by-bus ?b) (give-token-step ?b ?g) (goer ?b ?a))
 (giver ?g ?a))
(=> (and (going-by-bus ?b) (give-token-step ?b ?g) (bus-driver ?b ?a))
 (recipient ?g ?a))
(=> (and (going-by-bus ?b) (bus-driver ?b ?a))
 (occupation ?a busdriver))
(=> (and (going-by-bus ?b) (give-token-step ?b ?g) (token ?b ?t))
 (thing-given ?g ?t))
(=> (and (going-by-bus ?b) (token ?b ?t))
 (instance-token ?t ))

;; going-by-taxi

(=> (and (going-by-taxi ?b) (pay-step ?b ?p))
 (paying ?p))
(=> (and (going-by-taxi ?b) (pay-step ?b ?p) (goer ?b ?a))
 (payer ?p ?a))
(=> (and (going-by-taxi ?b) (pay-step ?b ?p) (taxi-driver ?b ?a))
 (payee ?p ?a))
(=> (and (going-by-taxi ?b) (taxi-driver ?b ?a))
 (occupation ?a taxidriver))

;; going-by-plane

(=> (and (going-by-plane ?v) (source-go ?v ?s))
 (airport ?s))

(=> (and (going-by-plane ?p) (pack-step ?p ?s))
 (packing ?s))
(=> (and (going-by-plane ?p) (pack-step ?p ?s) (goer ?p ?a))
 (agent-pack ?s ?a))
(=> (and (going-by-plane ?p) (pack-step ?p ?s) (plane-luggage ?p ?l))
 (patient-pack ?s ?l))
(=> (and (going-by-plane ?p) (plane-luggage ?p ?l))
 (bag ?l))

(=> (and (going-by-plane ?s) (buy-ticket-step ?s ?b))
 (buying ?b))
(=> (and (going-by-plane ?s) (buy-ticket-step ?s ?b) (goer ?s ?a))
 (buyer ?b ?a))
(=> (and (going-by-plane ?s) (buy-ticket-step ?s ?b) (plane-ticket ?s ?t))
 (thing-bought ?b ?t))
(=> (and (going-by-plane ?s) (plane-ticket ?s ?t))
 (ticket ?t ))

;; jogging

(=> (and (jogging ?j ) (drink-step ?j ?d))
 (drinking ?d))
(=> (and (jogging ?j) (drink-step ?j ?d) (jogger ?j ?a))
 (drinker ?d ?a))
(=> (and (jogging ?j) (drink-step ?j ?d) (jog-thing-drunk ?j ?a))
 (patient-drink ?d ?a))
(=> (and (jogging ?j) (drink-step ?j ?d) (jog-drink-straw ?j ?a))
 (instr-drink ?d ?a))

;; partying

(=> (and (partying ?p) (drink-step ?p ?d))
 (drinking ?d))
(=> (and (partying ?p) (drink-step ?p ?d) (agent-party ?p ?a))
 (drinker ?d ?a))
(=> (and (partying ?p) (drink-step ?p ?d) (party-thing-drunk ?p ?a))
 (patient-drink ?d ?a))
(=> (and (partying ?p) (drink-step ?p ?d) (party-drink-straw ?p ?a))
 (instr-drink ?d ?a))
))

(defparameter *sort-hierarchy* '(

(=> (and (physical ?x)) (any ?x))
(=> (and (action ?x)) (any ?x))

(=> (and (apparel ?x)) (physical ?x))
(=> (and (bag ?x)) (physical ?x))
(=> (and (food ?x)) (physical ?x))
(=> (and (gift ?x)) (physical ?x))
(=> (and (liquor ?x)) (physical ?x))
(=> (and (place ?x)) (physical ?x))
(=> (and (seat ?x)) (physical ?x))
(=> (and (shelf ?x)) (physical ?x))
(=> (and (straw ?x)) (physical ?x))
(=> (and (ticket ?x)) (physical ?x))
(=> (and (instance-token ?x)) (physical ?x))
(=> (and (valuable ?x)) (physical ?x))
(=> (and (instance-vehicle ?x)) (physical ?x))
(=> (and (weapon ?x)) (physical ?x))

(=> (and (buying ?x)) (action ?x))
(=> (and (courting ?x)) (action ?x))
(=> (and (drinking ?x)) (action ?x))
(=> (and (finding ?x)) (action ?x))
(=> (and (getting ?x)) (action ?x))
(=> (and (getting-off ?x)) (action ?x))
(=> (and (getting-on ?x)) (action ?x))
(=> (and (giving ?x)) (action ?x))
(=> (and (going ?x)) (action ?x))
(=> (and (ingesting ?x)) (action ?x))
(=> (and (jogging ?x)) (action ?x))
(=> (and (ordering ?x)) (action ?x))
(=> (and (packing ?x)) (action ?x))
(=> (and (partying ?x)) (action ?x))
(=> (and (paying ?x)) (action ?x))
(=> (and (pointing ?x)) (action ?x))
(=> (and (putting ?x)) (action ?x))
(=> (and (rest-dining ?x)) (action ?x))
(=> (and (robbing ?x)) (action ?x))
(=> (and (shopping ?x)) (action ?x))
(=> (and (sitting ?x)) (action ?x))
(=> (and (working ?x)) (action ?x))

(=> (and (shirt ?x)) (apparel ?x))
(=> (and (skirt ?x)) (apparel ?x))
(=> (and (trousers ?x)) (apparel ?x))
(=> (and (uniform ?x)) (apparel ?x))

(=> (and (suitcase ?x)) (bag ?x))

(=> (and (bread ?x)) (food ?x))
(=> (and (milk ?x)) (food ?x))
(=> (and (milkshake ?x)) (food ?x))

(=> (and (flower ?x)) (gift ?x))
(=> (and (jewelry ?x)) (gift ?x))

(=> (and (bourbon ?x)) (liquor ?x))

(=> (and (airport ?x)) (place ?x))
(=> (and (bus-station ?x)) (place ?x))
(=> (and (park ?x)) (place ?x))
(=> (and (prison ?x)) (place ?x))
(=> (and (restaurant ?x)) (place ?x))
(=> (and (school ?x)) (place ?x))
(=> (and (shopping-place ?x)) (place ?x))

(=> (and (money ?x)) (valuable ?x))

(=> (and (bus ?x)) (instance-vehicle ?x))
(=> (and (plane ?x)) (instance-vehicle ?x))
(=> (and (taxi ?x)) (instance-vehicle ?x))

(=> (and (gun ?x)) (weapon ?x))
(=> (and (knife ?x)) (weapon ?x))

(=> (and (going-by-vehicle ?x)) (going ?x))

(=> (and (liqst-shopping ?x)) (shopping ?x))
(=> (and (smarket-shopping ?x)) (shopping ?x))
(=> (and (liquor-store ?x)) (shopping-place ?x))
(=> (and (smarket ?x)) (shopping-place ?x))

(=> (and (going-by-bus ?x)) (going-by-vehicle ?x))
(=> (and (going-by-plane ?x)) (going-by-vehicle ?x))
(=> (and (going-by-taxi ?x)) (going-by-vehicle ?x))
))

;; accel training case 1

;; jack went to the supermarket.
(defparameter *e1a*
      '((going go1)
	(goer go1 jack1)
	(name jack1 jack)
	(dest-go go1 sm1)
	(smarket sm1)))

;; jack found some milk on the shelf.
(defparameter *e1b*
      '((finding find1)
	(finder find1 jack1)
	(thing-found find1 milk1)
	(milk milk1)
	(on milk1 shf1)
	(shelf shf1)))

;; jack paid for the milk.
(defparameter *e1c*
      '((paying pay1)
	(payer pay1 jack1)
	(thing-paid pay1 milk1)))

;; jack went to the supermarket.
;; jack found some milk on the shelf.
;; jack paid for the milk.
(defparameter *e1*
      (append *e1a* (append *e1b* *e1c*)))

;; "Bill went to the supermarket.
;;  He paid for some milk."

(defparameter *e2a*
      '((going go2 )
	(goer go2 bill2)
	(name bill2 bill)
	(dest-go go2 sm2)
	(smarket sm2)))

(defparameter *e2b*
      '((paying pay2)
	(payer pay2 bill2)
	(thing-paid pay2 milk2)
	(milk milk2)))

(defparameter *e2* 
      (append *e2a* *e2b*))


;; jack gave the busdriver a token. 
(defparameter *e3a*
      '((giving give3)
	(giver give3 jack3)
	(name jack3 jack)
	(recipient give3 bd3)
	(occupation bd3 busdriver)
	(thing-given give3 tk3)
	(instance-token tk3)))

;; jack got off at the supermarket
(defparameter *e3b*
      '((getting-off getoff3)
	(agent-get-off getoff3 jack3)
	(place-get-off getoff3 sm3)
	(smarket sm3)))

;; jack gave the busdriver a token. 
;; jack got off at the supermarket
(defparameter *e3*
      (append *e3a* *e3b*))


;"Jack got off the bus at the liquor-store.
; He pointed a gun at the owner."
(defparameter *e4a*
      '((getting-off getoff4 )
	(agent-get-off getoff4 jack4)
	(name jack4 jack)
	(patient-get-off getoff4 bus4)
	(bus bus4 )
	(place-get-off getoff4 ls4)
	(liquor-store ls4 )))

(defparameter *e4b*
      '((pointing point4 )
	(agent-point point4 jack4)
	(instr-point point4 gun4)
	(gun gun4 )
	(patient-point point4 o4)
	(own o4 ls4)))

(defparameter *e4*
      (append *e4a* *e4b*))


;"Jack went to the liquor-store.
; He found some bourbon on the shelf."
(defparameter *e5a*
      '((going go5 )
	(goer go5 jack5)
	(name jack5 jack)
	(dest-go go5 ls5)
	(liquor-store ls5)))

(defparameter *e5b*
      '((finding find5 )
	(finder find5 jack5)
	(thing-found find5 bourbon5)
	(bourbon bourbon5 )
	(on bourbon5 shf5)
	(shelf shf5 )))

(defparameter *e5*
      (append *e5a* *e5b*))


;; bill went to a liquor store
(defparameter *e6a*
      '((going go6) 
	(goer go6 bill6) 
	(name bill6 bill) 
	(dest-go go6 ls6) 
	(liquor-store ls6)))
      
;; bill pointed a gun at the owner
(defparameter *e6b*
      '((pointing point6) 
	(agent-point point6 bill6) 
	(instr-point point6 gun6) 
	(gun gun6) 
	(patient-point point6 o6) 
	(own o6 ls6)))

(defparameter *e6*
      (append *e6a* *e6b*))

;; "Bill gave the busdriver a token."
(defparameter *e7*
      '((giving give7 )
	(giver give7 bill7)
	(name bill7 bill)
	(recipient give7 bd7)
	(occupation bd7 busdriver)
	(thing-given give7 tk7)
	(token-instance tk7 )))

;"Fred robbed the liquor-store.
; Fred pointed a gun at the owner."

(defparameter *e8a*
      '((robbing rob8 )
	(robber rob8 fred8)
	(name fred8 fred)
	(place-rob rob8 ls8)
	(liquor-store ls8 )))

(defparameter *e8b*
      '((pointing point8 )
	(agent-point point8 fred8)
	(instr-point point8 gun8)
	(gun gun8 )
	(patient-point point8 o8)
	(own o8 ls8)))

(defparameter *e8* (append *e8a* *e8b*))


;; bill got a gun.
(defparameter *e9a*
       '((getting get9)
	(agent-get get9 bill9)
	(name bill9 bill)
	(patient-get get9 gun9)
	(gun gun9)))

;; bill went to the supermarket
(defparameter *e9b*
      '((going go9)
	(goer go9 bill9)
	(dest-go go9 sm9)
	(smarket sm9)))

;; bill got a gun.
;; bill went to the supermarket.
(defparameter *e9*
      (append *e9a* *e9b*))

;;"Fred went to the supermarket.
;; He pointed a gun at the owner.
;; He packed his bag.
;; He went to the airport."

(defparameter *e10a*
      '((going go10 )
	(goer go10 fred10)
	(name fred10 fred)
	(dest-go go10 sm10)
	(smarket sm10 )))

(defparameter *e10b*
      '((pointing point10 )
	(agent-point point10 fred10)
	(instr-point point10 gun10)
	(gun gun10 )
	(patient-point point10 o10)
	(own o10 sm10)))

(defparameter *e10c*
      '((packing pack10 )
	(agent-pack pack10 fred10)
	(patient-pack pack10 bag10)
	(bag bag10 )))

(defparameter *e10d*
      '((going go10b )
	(goer go10b fred10)
	(dest-go go10b airport10)
	(airport airport10 )))

(defparameter *e10*
      (append *e10a* (append *e10b* (append *e10c* *e10d*))))