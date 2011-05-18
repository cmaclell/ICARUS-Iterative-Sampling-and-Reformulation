; BLOCKENV.LISP
; Perceptions and actions for a simulated Blocks World environment.

; Create a data structure that holds the state of the environment.
(setq initial*
      (list '(block A xpos 0 ypos 2 width 2 height 2)
	    '(block B xpos 0 ypos 4 width 2 height 2)
	    '(block C xpos 0 ypos 6 width 2 height 2)
	    '(block D xpos 0 ypos 8 width 2 height 2)
	    '(table T1 xpos 0 ypos 0 width 20 height 2)
	    '(hand H1 xpos 0 ypos 20 status empty width 2)
	    '(world bottom 0 top 20 left 0 right 20)
	    ))



; Define a function that Icarus will call to initialize the environment.
(defun initialize-world ()
  (setq state* (rcopy initial*))
  nil)

; Define a function that Icarus will call to update the environment.
(defun update-world () nil)

; Define a function that Icarus will call to update the perceptual buffer.
(defun preattend () (copy-list state*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Accessors / Setters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; HAND

; note: macros should be setf'able

(defmacro hand-form ()
  `(assoc 'hand state*))

(defmacro hand-status ()
  `(second (member 'status (hand-form))))

(defmacro hand-xpos ()
  `(second (member 'xpos (hand-form))))

(defmacro hand-ypos ()
  `(second (member 'ypos (hand-form))))

;; not settable
(defun hand-width ()
  (second (member 'width (hand-form))))

;; BLOCKS

(defmacro block-form (block)
  `(car (member ,block state* :key #'second)))

(defmacro block-xpos (block)
  `(second (member 'xpos (block-form ,block))))

(defmacro block-ypos (block)
  `(second (member 'ypos (block-form ,block))))

(defmacro block-width (block)
  `(second (member 'width (block-form ,block))))

(defmacro block-height (block)
  `(second (member 'height (block-form ,block))))

;; not settable
(defun block-top (block)
  (+ (block-ypos block)
     (block-height block)))

;; WORLD specs

(defun world-form ()
  (assoc 'world state*))

(defun world-top ()
  (second (member 'top (world-form))))

(defun world-bottom ()
  (second (member 'bottom (world-form))))

(defun world-right ()
  (second (member 'right (world-form))))

(defun world-left ()
  (second (member 'left (world-form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Define an action for grasping a block.
(defun *grasp (block)
  (let* ((hand-status (hand-status))
	 (hand-xpos (hand-xpos))
	 (hand-ypos (hand-ypos))
	 (block-xpos (block-xpos block))
	 (block-top (block-top block)))
    (cond ((not (eql hand-status 'empty))
	   (warn "Can't Grasp Block ~a, already holding ~a" 
		 block hand-status))
	  ((not (= hand-xpos block-xpos))
	   (warn "Can't Grasp Block ~a since block-xpos ~a differs from hand-xpos ~a"
		 block block-xpos hand-xpos))
	  ((not (= hand-ypos block-ypos))
	   (warn "Can't Grasp Block ~a since block-ypos ~a differs from hand-ypos ~a"
		 block block-ypos hand-ypos))
	  (t
	   (when atrace*
	     (format t "Grasping ~a" block))
	   (setf (hand-status) block)))))

; Define an action for ungrasping a block.
;  NOTE: does not check that block is supported before releasing
;        gravity is not implemented so block will not fall
(defun *ungrasp (block)
  (let* ((hand-status (hand-status)))
    (cond ((not (eql hand-status block))
	   (warn "Can't UnGrasp Block ~a, not holding it"
		 block))
	  (t
	   (when atrace*
	     (format t "UnGrasping ~a" block))
	   (setf (hand-status) empty)))))

;; Moves Hand Down one step (2 units)
;;   If holding block, also moves block down same distance
;;   NOTE: does not check for collisions with lower block or table
(defun *hand-step-down (&optional (step-size 2))
  (let ((hand-status (hand-status)))
    (setf (hand-ypos)
	  (decf (hand-ypos) step-size))
    (unless (eql hand-status 'empty)
      ; hand-status is the block being held
      (setf (block-ypos hand-status)
	    (decf (block-ypos hand-status) step-size)))
    (when atrace*
      (format t "~%Moving hand down ~a" step-size))))

(defun *hand-step-up (&optional (step-size 2))
  (let ((hand-status (hand-status)))
    (setf (hand-ypos)
	  (incf (hand-ypos) step-size))
    (unless (eql hand-status 'empty)
      ; hand-status is the block being held
      (setf (block-ypos hand-status)
	    (incf (block-ypos hand-status) step-size)))
    (when atrace*
      (format t "~%Moving hand up ~a" step-size))))

(defun *hand-step-right (&optional (step-size 2))
  (let ((hand-status (hand-status))
	(hand-xpos (hand-xpos)))
    (cond ((not (< (+ hand-xpos  (hand-width) step-size)
		   (world-right)))
	   (warn "Can't move hand right - no room to move"))
	  (t (incf (hand-xpos) step-size)
	     (unless (eql hand-status 'empty)
	       ;; hand-status is a grasped block
	       (incf (block-xpos hand-status) step-size))
	     (when atrace*
	       (format t "Moving Hand Right ~a" step-size))))))

(defun *hand-step-left (&optional (step-size 2))
  (let ((hand-status (hand-status))
	(hand-xpos (hand-xpos)))
    (cond ((not (> (- hand-xpos step-size)
		   (world-left)))
	   (warn "Can't move hand left - no room to move"))
	  (t (decf (hand-xpos) step-size)
	     (unless (eql hand-status 'empty)
	       ;; hand-status is a grasped block
	       (decf (block-xpos hand-status) step-size))
	     (when atrace*
	       (format t "Moving Hand Left ~a" step-size))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

