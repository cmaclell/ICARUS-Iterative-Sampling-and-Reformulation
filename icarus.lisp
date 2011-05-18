;******************************************************************************
;******************************************************************************
; ICARUS.LISP
;
; This file loads all the ICARUS files that are needed for running 
; ICARUS-Rewrite.
;******************************************************************************
;******************************************************************************

(load "holdover.lisp")
(load "changed.lisp")

(load "matcher")
(load "StanfordCompatible")

(load "Unification")                    ; unification code adapted from Norvig
(load "solver-rewrite")			; main routines for rewritten solver

