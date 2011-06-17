
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PPRINT STATE - Nine Dots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf pprint-state* t)

;;; Sample Target printouts:

;; STATE:
;;     ooo
;;     ooo
;;     ooo
;;  -----------

;; STATE:
;;     xxx
;;     oxo
;;     xxx
;;  -----------


(defun pprint-state (&optional (state state*))
  (terpri)
  (princ "STATE:")
  (terpri)
;  (pprint-canvas (canvas-size state) state)
  )

;(defun canvas-size (state)
;  nil)

;(defun pprint-canvas (xsize ysize state)
;  (cond ((null state)
;)	 

	
	