(in-package :xembed)

(defun encode-flags (flags flags-alist)
  (reduce #'+ (mapcar #'(lambda (el)
			  (cdr (assoc el flags-alist)))
		      flags)))

(defun decode-flags (flags flags-alist)
  (if (numberp flags)
      (mapcar #'car (remove-if #'(lambda (el)
				   (zerop (logand flags (cdr el))))
			       flags-alist))
      :error))

;;; Protocol version
(defparameter +XEMBED-VERSION+ 0)

;;; Internal return codes
(defparameter +XEMBED_RESULT_OK+ 0)
(defparameter +XEMBED_RESULT_UNSUPPORTED+ 1)
(defparameter +XEMBED_RESULT_X11ERROR+ 2)

;;; XEMBED messages

(defparameter +XEMBED-MESSAGE-ALIST+
  '((:XEMBED-EMBEDDED-NOTIFY . 0) 	; e -> c :: client embedded
    (:XEMBED-WINDOW-ACTIVATE . 1) 	; e -> c :: embedder gets (keyboard) focus
    (:XEMBED-WINDOW-DEACTIVATE . 2) 	; e -> c :: client loses focus
    (:XEMBED-REQUEST-FOCUS . 3) 	; c -> e :: client requests focus
    (:XEMBED-FOCUS-IN . 4) 		; e -> c :: client gets focus + focus move
    (:XEMBED-FOCUS-OUT . 5)		; e -> c :: client loses focus
    (:XEMBED-FOCUS-NEXT . 6)		; c -> e :: client reached end of the tab focus chain
    (:XEMBED-FOCUS-PREV . 7)		; c -> e :: client reached beg of TFC after a back tab
    (:XEMBED-MODALITY-ON . 10)		; e -> c :: embedder gets shadowed by a modal dialog
    (:XEMBED-MODALITY-OFF . 11)		; e -> c :: 
    (:XEMBED-REGISTER-ACCELERATOR . 12)
    (:XEMBED-UNREGISTER-ACCELERATOR . 13)
    (:XEMBED-ACTIVATE-ACCELERATOR . 14)
    (:XEMBED-GTK-GRAB-KEY . 108)
    (:XEMBED-GTK-UNGRAB-KEY . 109)
    (:XEMBED-PROTOCOL-FINISHED . 201)))  ;; Sent from the socket to its parent to signal the end of the protocol

;;;; XEMBED-PROTOCOL-FINISHED . 201
;;;; sent from the socket to its parent when the client finished the protocol for some reason
;;;; data1 : socket window-id 
;;;; data2 : client window-id 

;;; Details for  XEMBED_FOCUS_IN
(defparameter +XEMBED-DETAIL-ALIST+
  '((:XEMBED-FOCUS-CURRENT . 0)
    (:XEMBED-FOCUS-FIRST . 1)
    (:XEMBED-FOCUS-LAST . 2)))

;; TODO Check errors (nil)
(defun encode-xembed-message-type (type)
    (cdr (assoc type +XEMBED-MESSAGE-ALIST+)))
(defun decode-xembed-message-type (type)
    (car (rassoc type +XEMBED-MESSAGE-ALIST+)))

(defun encode-xembed-detail (detail)
  (cdr (assoc detail +XEMBED-DETAIL-ALIST+)))
(defun decode-xembed-detail (detail)
  (car (rassoc detail +XEMBED-DETAIL-ALIST+)))


;;; Modifiers field for XEMBED_REGISTER_ACCELERATOR */
(defparameter +XEMBED-MODIFIER-ALIST+
`((:XEMBED_MODIFIER_SHIFT . ,(ash 1 0))
  (:XEMBED_MODIFIER_CONTROL . ,(ash 1 1))
  (:XEMBED_MODIFIER_ALT . ,(ash 1 2))
  (:XEMBED_MODIFIER_SUPER . ,(ash 1 3))
  (:XEMBED_MODIFIER_HYPER . ,(ash 1 4))))

(defun encode-xembed-modifier-flags (flags)
  (encode-flags flags +XEMBED-MODIFIER-ALIST+))

(defun decode-xembed-modifier-flags (flags)
  (decode-flags flags +XEMBED-MODIFIER-ALIST+))

;;; Flags for XEMBED_ACTIVATE_ACCELERATOR 
(defparameter +XEMBED_ACCELERATOR_OVERLOADED+ (ash 1 0))

;;; Directions for focusing
(defparameter +XEMBED_DIRECTION_DEFAULT+ 0)
(defparameter +XEMBED_DIRECTION_UP_DOWN+ 1)
(defparameter +XEMBED_DIRECTION_LEFT_RIGHT+ 2)

;;; Flags for _XEMBED_INFO
(defparameter +XEMBED-INFO-FLAGS-ALIST+
  `((:XEMBED-MAPPED . ,(ash 1 0))))

(defun encode-xembed-info-flags (flags)
  (encode-flags flags +XEMBED-INFO-FLAGS-ALIST+))

(defun decode-xembed-info-flags (flags)
  (decode-flags flags +XEMBED-INFO-FLAGS-ALIST+))

(defparameter +CurrentTime+ nil)

;;; Last time received
(defparameter *timestamp* nil)


(defun set-property-notify (window)
  (setf (window-event-mask window)
	(logior (window-event-mask window)
		(make-event-mask :property-change))))


(let ((x 0))
  (flet ((some-value () (if (= x 1) (setf x 2) (setf x 1))))
    (defun get-server-time (win)
      (let ((dpy (window-display win)))
	(display-finish-output dpy)
	(change-property win :clx-xembed-timestamp `(,(some-value)) :clx-xembed-timestamp 32)
	(event-cond (dpy :force-output-p t)
	  (:property-notify
	   (window atom time)
	   (and (window-equal window win)
		(eq :clx-xembed-timestamp atom))
	   time))))))
      
(defun update-timestamp (win &optional timestamp)
  (format t "TIMESTAMP: ~a > ~a = ~a ~%" timestamp *timestamp* (when (and *timestamp* timestamp) (> timestamp *timestamp*)))
  (when (or (null *timestamp*) (and (numberp timestamp) (> timestamp *timestamp*)))
    (setf *timestamp* (or timestamp (get-server-time win))))
  *timestamp*)




(defun xembed-info-raw (window)
  (get-property window :_XEMBED_INFO))

;;; Protocol version
(defun xembed-info-version-raw (xembed-info) 
  (first xembed-info))

;;; Flags: Only one flag is supported: XEMBED_MAPPED
(defun xembed-info-flags-raw (xembed-info) ;; flags 
  (second xembed-info))

(defun xembed-info (window)
  (let ((prop (get-property window :_XEMBED_INFO)))
    (list (xembed-info-version prop)
	  (decode-xembed-info-flags (xembed-info-flags-raw prop)))))

(defun xembed-info-valid-p (xembed-info)
  (and (numberp (xembed-info-version xembed-info))
       (listp (xembed-info-flags xembed-info))))

(defun xembed-info-ready-p (window)
  (let ((info (xembed-info window)))
    (and info (xembed-info-valid-p info))))

;;; Protocol version
(defun xembed-info-version (xembed-info) 
  (first xembed-info))

(defun xembed-info-flags (xembed-info)
  (second xembed-info))

;;; Setters
(defun (setf xembed-info-raw) (new-val window)
  (change-property window :_XEMBED_INFO new-val :_XEMBED_INFO 32))

(defun (setf xembed-info) (new-val window)
  (destructuring-bind (version flags) new-val
    (setf (xembed-info-raw window)
	(list version (encode-xembed-info-flags flags))))
  new-val)


;; FIXME: setters for properties should check if the property exist
(defun (setf xembed-info-version) (new-val window)
  (let ((info (xembed-info window)))
    (assert (not (null info)))
    (setf (xembed-info window) (list new-val (xembed-info-flags-raw info)))))

(defun (setf xembed-info-flags) (new-val window)
  (let ((info (xembed-info window)))
    (assert (not (null info)))
    (setf (xembed-info window) (list (xembed-info-version info)
				     new-val))))


;;;; XEmbed message sending 

;;; Wrap in handler-case, sync with `display-finish-output' and
;;; handle errors
;;; FIXME: change current time to something more meaningful
(defun xembed-send (dest-win &key opcode (detail 0) (data1 0) (data2 0) timestamp)
  (assert (not (null *timestamp*)))
  (dformat 7 ">>> xembed-send~S~%" (list (list (window-id dest-win))
					 (or (and (keywordp opcode) opcode) (decode-xembed-message-type opcode))
					 detail data1 data2))
  (send-event dest-win :client-message nil
		   :type :_XEMBED
		   :format 32
		   :window dest-win
		   :data (list (or timestamp *timestamp*)
			       (or (and (numberp opcode) opcode) (encode-xembed-message-type opcode))
			       (encode-xembed-detail detail)
			       data1 data2)
		   :propagate-p nil)
  (dformat 7 "<<< xembed-send~%"))

(defun xembed-notify (client-window embedder-window &optional version)
  (dformat 7 ">>> xembed-notify~%")
  (xembed-send client-window
	       :opcode :xembed-embedded-notify
	       :detail nil
	       :data1 (window-id embedder-window)
	       :data2 (or version +XEMBED-VERSION+))
  (dformat 7 "<<< xembed-notify~%"))

(defun xembed-focus-in (client-window detail)
  (dformat 7 ">>> xembed-focus-in~%")
  (xembed-send client-window
	       :opcode :xembed-focus-in
	       :detail detail)
  (dformat 7 "<<< xembed-focus-in~%"))

(defun delete-create-notify-event (win)
  (display-force-output (window-display win))
  (event-cond ((window-display win))
    (:create-notify (window) (window-equal window win)
		    t)))

(defmacro send-wrapper (disp (&body send-sequence)
			&body error-handlers)
  `(handler-case
       (progn
	 ,@send-sequence
	 (display-finish-output ,disp))
     ,@error-handlers))

		 
(defun supported-protocol-version (win)
  (let* ((info (xembed-info win))
	 (vers (and info (xembed-info-version info))))
    (format t "INFO: ~a~%" info)
    (and (listp (xembed-info-flags info)) (numberp vers) (min +XEMBED-VERSION+ vers))))


