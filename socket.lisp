(in-package :xembed)

;;; Property :XEMBED-SOCKET-INFO
;;; (use-client-geometry protocol-started-p modality active focused)
(defparameter +xembed-socket-info-flags-alist+
  '((1 . :use-client-geometry-p)
    (2 . :protocol-started-p)
    (4 . :modality-on-p)
    (8 . :active-p)
    (16 . :focused-p)
    (32 . :focus-requested-p)))



(defun xsi-flags (xsi)
  (first xsi))

(defun xsi-setflags (xsi &rest keyword-value-pairs &key &allow-other-keys)
  (list (apply #'setflags (xsi-flags xsi) +xembed-socket-info-flags-alist+ keyword-value-pairs)))

(defun xsi-getflag (xsi key)
  (getflag key (xsi-flags xsi) +xembed-socket-info-flags-alist+))

(defun xembed-socket-info (window)
  (let ((prop (get-property window :XEMBED-SOCKET-INFO)))
    (when prop
      (list (first prop)))))


(defun (setf xembed-socket-info) (value window)
  (change-property window :XEMBED-SOCKET-INFO (list (xsi-flags value))
		   :XEMBED-SOCKET-INFO 32))

(defparameter +socket-event-mask+
  (make-event-mask  :substructure-notify :exposure :enter-window
		    :leave-window :button-press 
		    :property-change))

(defun create-socket (use-client-geometry &rest key-value-pairs &key &allow-other-keys)
  (let* ((em (get-keyword-value :event-mask key-value-pairs))
	 (em1 (logior +socket-event-mask+ (or em 0)))
	 (sockwin (apply #'create-window (subst-keyword-value :event-mask em1 key-value-pairs))))
    (let ((xsi (list 0)))
      (setf (xembed-socket-info sockwin) (xsi-setflags xsi :use-client-geometry-p use-client-geometry)))
    sockwin))
	  
(defun socketp (window)
  (xembed-socket-info window))

    
(defun client (win)
  (multiple-value-bind (children parent root)
      (query-tree win)
    (declare (ignore parent root))
    (when children 
      (car children))))

(defun requested-state (win)
  (let ((c (client win)))
    (let ((info (xembed-info c)))
      (cond ((null info)
	     :error)
	    ((eq :error (xembed-info-flags info))
	     :error)
	    ((member :xembed-mapped
		    (xembed-info-flags info))
	     :mapped)
	    (t :unmapped)))))

(defun forward-xembed-message-up (socketwin timestamp opcode detail data1 data2)
  (xembed-send (window-parent socketwin)
	       :opcode opcode
	       :detail detail
	       :timestamp timestamp
	       :data1 data1
	       :data2 data2))

(defun maybe-start-protocol (socketwin)
  (let* ((xsi (xembed-socket-info socketwin))
	 (client (client socketwin))
	 (ver (supported-protocol-version client))
	 (dpy (window-display socketwin)))
    ;;(display-finish-output dpy)
    (when client
      (cond
	((xsi-getflag xsi :protocol-started-p)
	 t)
	(ver
	 (send-wrapper dpy
	     ((xembed-notify client
			     socketwin
			     ver)
	      (xembed-focus-in client :xembed-focus-current)
	      (xembed-send client :opcode :xembed-window-activate)
	      (xembed-send client :opcode :xembed-modality-on)
	      (cond ((not (xsi-getflag xsi :focused-p))
		     (xembed-send client :opcode :xembed-focus-out))
		    (t (xembed-focus-in client :xembed-focus-first)))
	      (when (not (xsi-getflag xsi :active-p))
		(xembed-send client :opcode :xembed-window-deactivate))
	      (when (not (xsi-getflag xsi :modality-on-p))
		(xembed-send client :opcode :xembed-modality-off))
	      (setf (xembed-socket-info socketwin)
		    (xsi-setflags xsi :protocol-started-p t))
	      (display-finish-output dpy)
	      t)
	   (xlib::x-error () (format t "ERROR on start protocol ~%") nil)))
	(t nil)))))
      

(defmacro send-and-set-flag (socketwin flag-key flag-value &body send-form)
  (let ((xsi (gensym))
	(sw (gensym))
	(fv (gensym))
	(fk (gensym)))
    `(let* ((,sw ,socketwin)
	    (,xsi (xembed-socket-info ,sw))
	    (,fv ,flag-value)
	    (,fk ,flag-key))
       (display-finish-output (window-display ,sw))
       (setf (xembed-socket-info ,sw)
	     (xsi-setflags ,xsi ,fk ,fv))
       (format t "XSI~S, [~S, ~S], V ~S, STARTED ~S~%" ,xsi ,fk ,fv (xsi-getflag ,xsi ,fk) (xsi-getflag ,xsi :protocol-started-p))
       (when (and ,xsi (xor (xsi-getflag ,xsi ,fk) ,fv)
		  (xsi-getflag ,xsi :protocol-started-p))
	 (send-wrapper (window-display ,sw)
	     (,@send-form)
	   (xlib::x-error () (error "motita")))))))

(defun socket-focus-in (socketwin &optional (what :xembed-focus-current))
  (socket-clear-focus-request socketwin)
  (send-and-set-flag socketwin :focused-p t
    (xembed-focus-in (client socketwin) what)))

(defun socket-focus-out (socketwin)
  (send-and-set-flag socketwin :focused-p nil
    (xembed-send (client socketwin) :opcode :xembed-focus-out)))

(defun socket-focus-requested-p (socketwin)
  (xsi-getflag (xembed-socket-info socketwin) :focus-requested-p))

(defun socket-clear-focus-request (socketwin &optional value)
  (let ((xsi (xembed-socket-info socketwin)))
    (setf (xembed-socket-info socketwin) (xsi-setflags xsi :focus-requested-p value))))
  

(defun socket-activate (socketwin)
  (send-and-set-flag socketwin :active-p t 
    (xembed-send (client socketwin) :opcode :xembed-window-activate)))

(defun socket-deactivate (socketwin)
  (send-and-set-flag socketwin :active-p nil 
    (xembed-send (client socketwin) :opcode :xembed-window-deactivate)))

(defun socket-modality-on (socketwin)
  (send-and-set-flag socketwin :modality-on-p t 
    (xembed-send (client socketwin) :opcode :xembed-modality-on)))
(defun socket-modality-off (socketwin)
  (send-and-set-flag socketwin :modality-on-p nil 
    (xembed-send (client socketwin) :opcode :xembed-modality-off)))

(defun destroy-socket (socketwin &optional (reparent-p t))
  (when (and reparent-p (client socketwin))
    (unmap-window (client socketwin))
    (reparent-window (client socketwin)
		     (drawable-root socketwin)
		     0 0))
  (destroy-window socketwin))

(defun socket-reset (socketwin)
  (when (client socketwin)
    (reparent-window (client socketwin)
		     (drawable-root socketwin)
		     0 0))
  (let ((xsi (xembed-socket-info socketwin)))
    (setf (xembed-socket-info socketwin)
	  (xsi-setflags xsi :protocol-started-p nil
			:modality-on-p nil
			:active-p nil
			:focused-p nil))))


(defun satisfy-map/unmap-request (socketwin)
  (case (requested-state socketwin)
    ((:error :mapped) (map-window (client socketwin)))
    (:unmapped (unmap-window (client socketwin)))))

(defun socket-reset-geometry (socketwin)
  (let ((xsi (xembed-socket-info socketwin)))
    (cond ((xsi-getflag xsi :use-client-geometry-p)
	   (let ((sh (wm-normal-hints (client socketwin))))
	     (window-resize socketwin (wm-size-hints-width sh) (wm-size-hints-height sh))))
	  (t (window-resize (client socketwin) (drawable-width socketwin) (drawable-height socketwin))
	     (drop-configure-notify socketwin (drawable-width socketwin) (drawable-height socketwin))))))

(defun embed (socketwin clientwin &optional (reparent-p nil) (x 0) (y 0) (reset-geometry-p t))
  (set-property-notify clientwin)
  (display-finish-output (window-display socketwin)) ;; Throw error if bad window
  (when reparent-p
    (reparent-window clientwin socketwin x y))
  (when reset-geometry-p
    (socket-reset-geometry socketwin))
  (maybe-start-protocol socketwin))



;;;; Basic event processing

(defun socket-list-handler-vector (socketlist-fn)
  (flet ((socketlist-member (window)
	   (member window (funcall socketlist-fn) :test #'window-equal)))
    (handler-vector
     ((:configure-notify) (event-window)
      (when (socketlist-member event-window)
	(format t "CONFIGURE-NOTIFY RECEIVED~%")
	(socket-reset-geometry event-window)))
     ((:destroy-notify) (event-window window)
      (when (socketlist-member event-window)
	(socket-reset event-window)
	(xembed-send (window-parent event-window)
		     :opcode :xembed-protocol-finished
		     :data1 (window-id event-window)
		     :data2 (window-id window))))
     ((:create-notify) (parent window)
      (when (socketlist-member parent)
	(embed parent window nil)))
     ((:reparent-notify) (event-window parent window)
      (when (socketlist-member event-window)
	(cond ((window-equal event-window parent)
	       (embed parent window nil)
	       t)
	      (t (socket-reset event-window)
		 (xembed-send (window-parent event-window)
			      :opcode :xembed-protocol-finished
			      :data1 (window-id event-window)
			      :data2 (window-id window))))))
     ((:property-notify) (window atom time)
      (handler-case 
	  (let ((parent (window-parent window)))
	    (when (and (eq atom :_XEMBED_INFO)
		       (socketlist-member parent))
	      (update-timestamp parent time)
	      (when (maybe-start-protocol parent)
		(satisfy-map/unmap-request parent))
	      t))
	(xlib::x-error () nil))))))

(defun socket-handler-vector (socketwin)
  (socket-list-handler-vector #'(lambda () (list socketwin))))

(defun drop-configure-notify (socketwin w h)
  (declare (type window socketwin)
	   (type card32 w h))
  (event-cond ((window-display socketwin) :timeout 0)
    (:configure-notify (event-window window width height)
		       (and (window-equal event-window socketwin)
			    (window-equal window (client socketwin))
			    (= w (the card32 width)) (= h (the card32 height)))
		       t)))


(defun socket-resize (socketwin w h)
  (declare (type window socketwin)
	   (type card32 w h))
  (when (client socketwin)
    (window-resize (client socketwin) w h)
    (drop-configure-notify socketwin w h))
  (window-resize socketwin w h))


