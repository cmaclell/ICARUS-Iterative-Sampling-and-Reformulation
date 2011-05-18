
; TOHENV.LISP - Simulated environment for Tower of Hanoi puzzle. 

; Define the basic *move action and supporting functions. 

(defun *move (disk from to)
  (let* ((disk-desc (get-object disk state*))
         (peg-desc (get-object to state*))
         (xpos (cadr (member 'xpos peg-desc)))
         (highest (get-highest-disk-with xpos state* 0)))
    (setf (cadr (member 'xpos disk-desc)) xpos)
    (setf (cadr (member 'ypos disk-desc)) (+ highest 1))))

(defun get-object (object state)
  (cond ((null state) nil)
        ((eq (cadar state) object)
         (car state))
        (t (get-object object (cdr state)))))

(defun get-highest-disk-with (xpos state max)
  (cond ((null state) max)
        ((and (eq (caar state) 'disk)
              (eq (cadr (member 'xpos (car state))) xpos))
         (get-highest-disk-with xpos (cdr state)
                           (max (cadr (member 'ypos (car state))) max)))
        (t (get-highest-disk-with xpos (cdr state) max))))

(defun update-world ()
  nil)

(defun preattend ()
  state*)

(setq gstm* nil)

; Specify descriptions for different configurations of disks. 

(defvar state*)
(defvar state2*)
(defvar state3*)
(defvar state4*)
(defvar state5*)

(setq state2*
      (list '(peg pegA xpos 10 ypos 0 width 1 height 5)
            '(peg pegB xpos 20 ypos 0 width 1 height 5)
            '(peg pegC xpos 30 ypos 0 width 1 height 5)
            '(disk disk1 xpos 10 ypos 2 width 2 height 1)
            '(disk disk2 xpos 10 ypos 1 width 4 height 1)))

(setq state3*
      (list '(peg pegA xpos 10 ypos 0 width 1 height 5)
            '(peg pegB xpos 20 ypos 0 width 1 height 5)
            '(peg pegC xpos 30 ypos 0 width 1 height 5)
            '(disk disk1 xpos 10 ypos 3 width 2 height 1)
            '(disk disk2 xpos 10 ypos 2 width 4 height 1)
            '(disk disk3 xpos 10 ypos 1 width 6 height 1)))

(setq state4*
      (list '(peg pegA xpos 10 ypos 0 width 1 height 5)
            '(peg pegB xpos 20 ypos 0 width 1 height 5)
            '(peg pegC xpos 30 ypos 0 width 1 height 5)
            '(disk disk1 xpos 10 ypos 4 width 2 height 1)
            '(disk disk2 xpos 10 ypos 3 width 4 height 1)
            '(disk disk3 xpos 10 ypos 2 width 6 height 1)
            '(disk disk4 xpos 10 ypos 1 width 8 height 1)))

(setq state5*
      (list '(peg pegA xpos 10 ypos 0 width 1 height 5)
            '(peg pegB xpos 20 ypos 0 width 1 height 5)
            '(peg pegC xpos 30 ypos 0 width 1 height 5)
            '(disk disk1 xpos 10 ypos 2 width 2 height 1)
            '(disk disk2 xpos 30 ypos 2 width 4 height 1)
            '(disk disk3 xpos 30 ypos 1 width 6 height 1)
            '(disk disk4 xpos 10 ypos 1 width 8 height 1)))

; Define functions for initializing the environment. 

(defun task2 ()
  (defun initialize-world ()
    (setq state* state2*)
    nil))

(defun task3 ()
  (defun initialize-world ()
    (setq state* state3*)
    nil))

(defun task4 ()
  (defun initialize-world ()
    (setq state* state4*)
    nil))

(defun task5 ()
  (defun initialize-world ()
    (setq state* state5*)
    nil))

; After you have loaded this file and your concept definitions, you
; can demonstrate their behavior on four different disk configurations
; by typing the commands: 
; 
; (task2)
; (grun)
; (task3)
; (grun)
; (task4)
; (grun)
; (task5)
; (grun)
;
; You will need to type (clear) to clear conceptual memory before 
; you load files for another domain. 
