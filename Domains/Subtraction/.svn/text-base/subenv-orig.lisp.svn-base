
; SUBENV.LISP - Simulated environment for multi-column subtraction

; Define the executable actions and supporting functions. 

; FIND-DIFF

(defun *find-diff (col top bottom)
  (let* ((column (car (member col columns* :key #'cadr)))
	 (below-plus (member 'below column)))
    (setf (cadr below-plus) (- top bottom))))

; SUB-ONE

(defun *sub-one (col top)
  (let* ((column (car (member col columns* :key #'cadr)))
	 (dec-plus (member 'decremented column))
	 (top-plus (member 'top column)))
    (setf (cadr top-plus) (- top 1))
    (setf (cadr dec-plus) t)))

; ADD-TEN

(defun *add-ten (col top)
  (let* ((column (car (member col columns* :key #'cadr)))
	 (top-plus (member 'top column)))
    (setf (cadr top-plus) (+ top 10))))

; UPDATE-WORLD does nothing, since only icarus' actions can alter
; the environment. 

(defun update-world ())

; PREATTEND

(defun preattend () columns*)

; Specify initial states for various subtraction problems

(defvar columns*)
(defvar columns1*)
(defvar columns2*)
(defvar columns3*)
(defvar columns4*)
(defvar columns5*)

; 45 - 32 = 13

(setq columns1*
      (list 
       '(column C1 xpos 1 top 5 bottom 2 below nil decremented nil)
       '(column C2 xpos 2 top 4 bottom 3 below nil decremented nil)))

; 45 - 17 = 28

(setq columns2*
      (list 
       '(column C1 xpos 1 top 5 bottom 7 below nil decremented nil)
       '(column C2 xpos 2 top 4 bottom 1 below nil decremented nil)))

; 40 - 17 = 23

(setq columns3*
      (list 
       '(column C1 xpos 1 top 0 bottom 7 below nil decremented nil)
       '(column C2 xpos 2 top 4 bottom 1 below nil decremented nil)))

; 805 - 237 = 568

(setq columns4*
      (list 
       '(column C1 xpos 1 top 5 bottom 7 below nil decremented nil)
       '(column C2 xpos 2 top 0 bottom 3 below nil decremented nil)
       '(column C3 xpos 3 top 8 bottom 2 below nil decremented nil)))

; 2005 - 237 = 1768

(setq columns5*
      (list 
       '(column C1 xpos 1 top 5 bottom 7 below nil decremented nil)
       '(column C2 xpos 2 top 0 bottom 3 below nil decremented nil)
       '(column C3 xpos 3 top 0 bottom 2 below nil decremented nil)
       '(column C4 xpos 4 top 2 bottom nil below nil decremented nil)))

; Define functions for initializing the environment. 

(defun task1 ()
  (defun initialize-world ()
    (setq columns* columns1*))
    nil)

(defun task2 ()
  (defun initialize-world ()
    (setq columns* columns2*))
    nil)

(defun task3 ()
  (defun initialize-world ()
    (setq columns* columns3*))
    nil)

(defun task4 ()
  (defun initialize-world ()
    (setq columns* columns4*))
    nil)

(defun task5 ()
  (defun initialize-world ()
    (setq columns* columns5*))
    nil)

; After you have loaded this file and your concept definitions, you
; can demonstrate their behavior on five different subtraction tasks
; by typing the commands: 
;
; (task1)
; (grun)
; (task2)
; (grun)
; (task3)
; (grun)
; (task4)
; (grun)
; (task5)
; (grun)
