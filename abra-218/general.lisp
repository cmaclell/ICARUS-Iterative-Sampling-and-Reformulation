;;; a set of general purpose Lisp functions ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from Paul Graham's On Lisp
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns lst without items for which fn returns nil
(defun filter (fn lst)
  (loop for x in lst
     when (funcall fn x)
     collect x))

;; base - map: the value is a list of items
;; newdata - map: same structure as base
;;
;; adds entries in newdata to base. if base already contains the key, 
;; the entries are chained in a set (as a uniquified list).
(defun merge-hash-tables (base newdata)
  (maphash #'(lambda (k v)
	       (setf (gethash k base)
		     (delete-duplicates (append v (gethash k base)))))
	   newdata))

;; returns a list of all the keys in a hashtable
(defun hash-table-keys (ht)
  (loop for key being the hash-keys of ht
	collect key))

;; returns a list of all the values in a hashtable
(defun hash-table-values (ht)
  (loop for value being the hash-values of ht
	collect value))

;; returns a list of interleaved keys and values in a hashtable
(defun hash-table-values-keys (ht)
  (let ((values))
    (maphash #'(lambda (k v) (push v values) (push k values)) ht)
    values))

;; like (map 'list #'identity vec) but faster
;; transforms a simple vector into a list
(defun vector-to-list (vec)
  #+clisp (declare (type (simple-vector double-float) vec))
  #-clisp (declare (type (simple-array t) vec))
  (let ((lst))
    (do ((i (- (array-dimension vec 0) 1) (- i 1)))
	((< i 0) lst)
      (push (#+clisp svref #-clisp aref vec i) lst))))

;; returns a copy of the given vector
(defun copy-vector (v)
;  (declare (vector v))
  (let* ((vlength (array-dimension v 0))
	 (new-vector (make-array vlength)))
    (do ((i 0 (+ i 1)))
	((>= i vlength) new-vector)
      (setf (#+clisp svref #-clisp aref new-vector i) 
	    (#+clisp svref #-clisp aref v i)))))

;; basic while loop
;; ACL already has one
#-allegro (defmacro while (test &rest body)
	    `(do ()
		 ((not ,test))
	       ,@body))

;; true if x is in the closed interval (b1, b2)
(defun between (x b1 b2)
  (and (> x b1)
       (< x b2)))

;; removes the nth item from the list 
(defun remove-nth (n lst)
  (cond ((< n 0) (error "Bad arg to remove-nth"))
	((= n 0) (cdr lst))
	(t 
	 (cons (car lst) (remove-nth (- n 1) (cdr lst))))))

;; INPUT
;;   n: the lowest number in the list
;;   m: the highest number in the list
;;
;; OUTPUT
;;  a list of numbers in the interval [n, m]
(defun range-n-m (n m)
  (let (lst (i m))
    (while (<= n i)
      (push i lst)
      (decf i))
    lst))

;; input
;;  lol: a list of lists
;; output
;;  a list containing the elements that were nested in the input
;; 
;; example: ((a b) (c (d e) f) (g h)) --> (a b c (d e) f g h)
(defun flatten (lol)	
  (apply #'append (mapcar #'identity lol)))

;; a nondestructive version of mapcan
;; from Paul Graham's ANSI Common Lisp
(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

;; replace all the values in vec with val
;; input: val - any value
;;        vec - a vector
;; output: vec with all values replaced by val
;; (destructive)
(defun fill-vector (val vec)
  (dotimes (idx (array-dimension vec 0) vec)
    (setf (aref vec idx) val)))

;; looks for itm in the tree and returns the path if found. assumes
;; the tree has the following form.
;;
;; Ex/ (R (A) (B (C) (D (E) (F)) (G (H) (I)) (J)))
;; (path-to 'f (R (A) (B (C) (D (E) (F)) (G (H) (I)) (J))))
;; ==> (R B D F)
(defun path-to (itm tree &key (comparator #'eql))
  (cond ((null tree) nil)
        ((atom tree)
         (if (funcall comparator tree itm) (list itm)))
        (t
         (let ((tmp (path-to itm (cdr tree))))
           (if tmp
             (if (atom (car tree))
               (cons (car tree) tmp)
               tmp)
             (path-to itm (car tree)))))))