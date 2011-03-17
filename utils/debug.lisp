(in-package :xembed)
(defparameter *debug-stream* *standard-output*)
(defparameter *debug-level* 0)
(defparameter *show-progress* t)
(defparameter *progress-stream* *standard-output*)
(defparameter *result-stream* *standard-output*)
(defun format-in-or-out (in-or-out)
  (case in-or-out
    (:in ">>>")
    (:out "<<<")
    (otherwise "")))
(defun format-call (fn in-or-out &rest arguments)
  (format nil "~a ~a~:[()~;~:*~S~]" (format-in-or-out in-or-out) fn arguments))

(defun dformat-call (level fn in-or-out &rest arguments)
  (dformat level "~a~%" (apply #'format-call fn in-or-out arguments)))
(defun dformat (level control-string &rest format-arguments)
  (when (<= level *debug-level*)
    (apply #'format *debug-stream* control-string format-arguments)))
(defun pformat (control-string &rest format-arguments)
  (when *show-progress*
    (apply #'format *progress-stream* control-string format-arguments)))

(defun rformat (control-string &rest format-arguments)
  (apply #'format *result-stream* control-string format-arguments))

(defmacro with-result-stream ((fname &rest key-value-pairs &key &allow-other-keys) &body body)
  `(with-open-file (*result-stream* ,fname :direction :output ,@key-value-pairs)
     ,@body))



