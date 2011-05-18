

;; step BlockWorlds (movements in steps), with durative action(s)
(load "./Domains/Blocksworld-Step/BlockEnv-Step")
(load "./Domains/Blocksworld-Step/Block-RW-Step")

;; originally defined and set to 2 in "interpreter.lisp"
(setf max-length-print-full-percept* 80)

;(create-problems ((holding D)))		; try other goals for testing
;(create-problems ((ontable C ?table)))	; test UNSTACK-TO-TABLE macro
;(create-problems ((clear A)))		; test recursive MAKE-CLEAR skill(s)
;(create-problems ((on C D)))                ; test back-chaining



;; DURATIVE ACTION EXAMPLE:
;;  Lower hand by steps until (hand-touch-down) is a true belief
;;   1. Load this file
;;   2. (RUN 1)
;;   3. (CALL-INTENTION '(LOWER-EMPTY-HAND)
;;   4. (CONT 10)
;;   5. Observe that when (hand-touch-down) is in beliefs
;;        the execution stops
