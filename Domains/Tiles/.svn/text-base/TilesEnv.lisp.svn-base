; ************************************************************************
; ************************************************************************
; Environment file for Tiles and Squares puzzle.
;   Derived from TILESENV.LISP
;       originally Copyright (C) 2006 Institute for the Study of Learning and Expertise
;   Modified for Icarus-Rewrite-Factored
; ************************************************************************
; ************************************************************************

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  World States - specified in order of difficulty
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar state1*)
(defvar state2*)
(defvar state3*)

(setq state1*
     (list '(square S1 xpos 1)
           '(square S2 xpos 2)
           '(square S3 xpos 3)
           '(square S4 xpos 4)
           '(tile A xpos 4)
           '(tile B xpos 1)
           '(tile C xpos 2)
	   '(blank X xpos 3)))

(setq state2*
     (list '(square S1 xpos 1)
           '(square S2 xpos 2)
           '(square S3 xpos 3)
           '(square S4 xpos 4)
           '(square S5 xpos 5)
           '(tile A xpos 5)
           '(tile B xpos 1)
           '(tile C xpos 2)
           '(tile D xpos 3)
	   '(blank X xpos 4)))

(setq state3*
     (list '(square S1 xpos 1)
           '(square S2 xpos 2)
           '(square S3 xpos 3)
           '(square S4 xpos 4)
           '(square S5 xpos 5)
           '(tile A xpos 4)
           '(tile B xpos 1)
           '(tile C xpos 2)
           '(tile D xpos 3)
	   '(blank X xpos 5)))

(defvar initial*)

(setf initial* (rcopy state1*))

(defun initialize-world ()
 (setq state* (rcopy initial*))
 nil)

(defun update-world ()
 nil)

(defun preattend ()
  (copy-list state*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Accessor / Settor Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro square-form (square)
  `(car (member ,square state* :key #'second)))

(defmacro square-xpos (square)
  `(second (member 'xpos (square-form ,square))))

(defmacro tile-form (tile)
  `(car (member ,tile state* :key #'second)))

(defmacro tile-xpos (tile)
  `(second (member 'xpos (tile-form ,tile))))

(defmacro blank-form ()
  `(car (member 'X state* :key #'second)))

(defmacro blank-xpos ()
  `(second (member 'xpos (blank-form))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definitions for executable actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun *move (tile from to)
  (let* ((xtile (tile-xpos tile))
	 (xfrom (square-xpos from))
	 (xto (square-xpos to))
	 (xblank (blank-xpos)))
    (cond ((not (eql xtile xfrom))
	   (format t "~%*** The tile ~a is not on square ~a."
		   tile from))
	  ((not (eql xto xblank))
	   (format t "~%*** The to square ~a is not blank."
		   to))
	  (t 
	   (setf (tile-xpos tile) xto
		 (blank-xpos) xfrom)))))


;;; This is the old code:
;;;   this shouldn't refer to pstm* should it??
;;;   also, it doesn't verify that the "to" location is empty

#|
(defun *move (tile from to)
 (let* ((ptile (car (member tile pstm* :key #'cadr)))
        (pfrom (car (member from pstm* :key #'cadr)))
        (pto (car (member to pstm* :key #'cadr)))
        (xtile (member 'xpos ptile))
        (xfrom (member 'xpos pfrom))
        (xto (member 'xpos pto)))
   (cond ((not (eq (cadr xtile) (cadr xfrom)))
          (terpri)(princ "*** The tile ")(princ tile)
          (princ " is not on square ")(princ from)(princ "."))
         (t (setf (cadr xtile) (cadr xto))))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tasks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun task1 ()
  (setq initial* state1*)
  (clear-goals)
  ;;  (eval (list 'create-problems '((on A S1))))
  (create-problems ((on A S1)))
  nil)

(defun task2 ()
  (setq initial* state1*)
  (clear-goals)
  ;;  (eval (list 'create-problems '((ON A S1) (ON B S2))))
  (create-problems ((ON A S1) (ON B S2)))
  nil)

(defun task3 ()
  (setq initial* state3*)
  (clear-goals)
  ;;  (eval (list 'create-problems '((ON A S1) (ON B S2) (ON C S3) (ON D S4))))
  (create-problems ((ON A S1) (ON B S2) (ON C S3) (ON D S4)))
  nil)

; After you have loaded this file and your Icarus program, you can
; demonstrate its behavior on three different Tiles and Squares tasks
; by typing the commands:
;
; (task1)
; (grun)
; (task2)
; (grun)
; (task3)
; (grun)