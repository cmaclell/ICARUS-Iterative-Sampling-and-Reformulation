
;; Standard Strips Blockworld

(load "./Domains/Ninedots/NinedotsEnv")
(load "./Domains/Ninedots/ninedots-problem")
(load "./Domains/Ninedots/pprint-ninedots")	; Separate file to setup pprint-state

;(create-problems ((holding D)))		; try other goals for testing
;(create-problems ((ontable C ?table)))	; test UNSTACK-TO-TABLE macro
;(create-problems ((clear A)))		; test recursive MAKE-CLEAR skill(s)
;(create-problems ((on C D)))                ; test back-chaining
(create-problems ((pen-down) (line 0 0 0 2)))

(set-search-direction)

;(task1)

(setq ptrace* 0)