
;; Standard Strips Blockworld

(load "./Domains/Ninedots/NinedotsEnv")
;(load "./Domains/Blocksworld/Block-RW-Strips")		; Lower-level Strips-like description of BlocksWorld
(load "./Domains/Ninedots/Block-Sussman")
(load "./Domains/Ninedots/pprint-block-state")	; Separate file to setup pprint-state

;(create-problems ((holding D)))		; try other goals for testing
;(create-problems ((ontable C ?table)))	; test UNSTACK-TO-TABLE macro
;(create-problems ((clear A)))		; test recursive MAKE-CLEAR skill(s)
;(create-problems ((on C D)))                ; test back-chaining
;(create-problems ((on C D) (on A B)))

(set-search-direction)

(task2)

(setq ptrace* 0)