
(setf *print-circle* t)    ; avoid infinite loops when printing circular structure references

(load "icarus")

(defmacro ex (skill)
  "Execute a skill, and reset the scene."
  `(progn (execute-skill ,skill)))

(defmacro ex+ (skill)
  "Execute a skill, and reset the scene."
  `(progn (execute-skill+ ,skill)))


(defparameter *domain-map*
  (list (list "Ninedots"
	      "Domains/Ninedots/loader-ninedots")
	(list "Blocks World"
	      "Domains/Blocksworld/loader-blocks")
	(list "Blocks World Step"
	      "Domains/Blocksworld-Step/loader-step")
	(list "Tower of Hanoi"
	      "Domains/Tower of Hanoi/loader-toh")
	(list "Tiles and Squares"
	      "Domains/Tiles/loader-tiles")
	(list "Tile Sliding - Five Puzzle"
	      "Domains/Tile Sliding/loader-five")
	(list "Peg Solitaire"
	      "Domains/Peg Solitaire/loader-peg")
	(list "Subtraction"
	      "Domains/Subtraction/loader-sub")
	(list "Depots"
	      "Domains/Depots/loader-depots")
	(list "Logistics"
	      "Domains/Logistics/loader-logistics")
	(list "Gripper"
	      "Domains/Gripper/loader-gripper")))

(defun prompt-user-for-domain ()
  ;; Print the domain options
  (format t "~%~%*********************************")
  (format t "~%Here are the domain options:")
  (loop for i from 1
       for domain-list in *domain-map*
       do
       (format t "~%   ~a  ~a" i (first domain-list)))
  ;; Prompt user and read selection
  (format t "~%Enter the number of the domain to load (0 for None):")
  (let ((domain-number (read)))
    (cond ((and (numberp domain-number)
		(> domain-number 0)
		(<= domain-number (length *domain-map*)))
	   (loop with domain-list = (nth (1- domain-number) *domain-map*)
	      for file in (cdr domain-list)
	      do
		(load file)
	      finally
		(format t "~%~%Loaded Domain: ~a" (first domain-list))))
	  (t
	   (format t "~%~%No Domain Loaded")))))


(prompt-user-for-domain)

       
