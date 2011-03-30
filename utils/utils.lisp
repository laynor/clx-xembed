;;;; Utility functions used in the package
(in-package :xembed)

(defun curry (function &rest args)
    (lambda (&rest more-args)
      (apply function (append args more-args))))
(defun compose (fn1 fn2)
  #'(lambda (&rest args)
      (funcall fn1 (apply fn2 args))))

(defun sequencep (object)
  (typecase object
    (sequence t)
    (otherwise nil)))

(defun maybe (pred)
  #'(lambda (arg)
      (or (funcall pred arg)
	  (null arg))))

(defun nullfn (&rest args)
  (declare (ignore args)))

;;;; Combine 2 functions in OR 
(defun combine-functions (hf1 hf2)
  (cond
    ((and (functionp hf1) (functionp hf2)
	  (not (eq hf1 #'nullfn)) (not (eq hf2 #'nullfn)))
     #'(lambda (&rest args)
	 (or (apply hf1 args)
	     (apply hf2 args))))
    ((and (not (eq hf1 #'nullfn))(functionp hf1))
     hf1)
    ((and (not (eq hf2 #'nullfn)) (functionp hf2))
     hf2)
    (t #'nullfn)))

;;; (1 2 3 4 5 6 7 8) => ((1 2) (3 4) (5 6) (7 8))
;;; (:a 1 :b 2 :c 3 :d 4) => ((:a 1) (:b 2) (:c 3) (:d 4))
(defun group-by-2 (list)
  (cond
    ((null list) nil)
    ((cons (list (car list) (cadr list)) (group-by-2 (cddr list))))))

;;; Flattens a list 1 level
(defun flatten (list)
  (cond
    ((null list)
     nil)
    ((atom (car list))
     (cons (car list) (flatten (cdr list))))
    ((listp (car list))
     (append (car list) (flatten (cdr list))))))

;;;Given a keyword argument list, returns the value for a given key
(defun get-keyword-value (key list)
  (let ((p (member key list)))
    (and p (cadr p))))

;;; Given a keyword argument LIST, returns another list
;;; whith the value associated to KEY changed to VALUE
(defun subst-keyword-value (key value list)
  (if (get-keyword-value key list)
      (let ((list-by-2 (group-by-2 list)))
	(flatten (mapcar #'(lambda (el)
			     (if (eq key (car el))
				 (list key value)
				 el))
			 list-by-2)))
      (cons key (cons value list))))


;;;; Binary flags
;;; key-alist : binary flag alist of the form
;;;    ((1 . :flag-1)
;;;     (2 . :flag-2)
;;;     (4 . :flag-3)
;;;     (8 . :flag-4)
;;;     ... )
;;; flags: flags in integer form
;;; key: keyword
;;; value: value associated to the keyword
;;;
;;; Returns: a copy of FLAGS with the value for KEY changed to VALUE
;;; ex. ((1 . :flag-1)
;;;      (2 . :flag-2))
;;;     (setflag 0 :flag-1 t) => 1
;;;     (setflag 3 :flag-1 nil) => 2
;;;     (setflag 2 :flag-1 t) => 3
(defun setflag (flags key-alist key value)
  (let ((f (rassoc key key-alist)))
    (cond ((not f) flags)
	  (value (logior flags (car f)))
	  (t (logand (lognot (car f)) flags)))))

;;; Same as above but operating on multiple flags
(defun setflags (flags key-alist &rest key-value-pairs &key &allow-other-keys)
  (cond ((null key-value-pairs)
	 flags)
	(t (apply #'setflags
		  (setflag flags key-alist (car key-value-pairs) (cadr key-value-pairs))
		  key-alist
		  (cddr key-value-pairs)))))

;;; Gets the value associated to KEY from FLAGS (with respect to KEY-ALIST)
(defun getflag (key flags key-alist)
  (let ((f (rassoc key key-alist)))
    (when f
      (not (zerop (logand (car f) flags))))))

(defun xor (x y)
  (and (or x y) (not (and x y))))

(defun move-next-to-helper (el next-to seq &key (where :right) (test #'eql))
  (if (find el seq :test test)
      (let* ((seq1 (remove el seq :test test))
	     (next-to-pos (position next-to seq1 :test test))
	     (splitpos (and next-to-pos (case where
					  (:right (1+ next-to-pos))
					  (:left next-to-pos))))
	     (subseq1 (and splitpos (subseq seq1 0 splitpos)))
	     (subseq2 (if splitpos (subseq  seq1 splitpos) seq1)))
	(concatenate (type-of seq) subseq1 (make-sequence (type-of seq) 1 :initial-element el) subseq2))
      seq))

(defun move-next-to (el next-to seq &key (where :right) (test #'eql))
  (cond ((not (find el seq :test test))
	 seq)
	((null next-to)
	 (case where
	   (:left (append (remove el seq :test test) (list el)))
	   (:right (cons el (remove el seq :test test)))))
	(t (move-next-to-helper el next-to seq :where where :test test))))

