(in-package :xembed-user)

(defparameter +SYSTEM-TRAY-OPCODES-ALIST+
  '((:SYSTEM-TRAY-REQUEST-DOCK . 0)
    (:SYSTEM-TRAY-BEGIN-MESSAGE . 1)
    (:SYSTEM-TRAY-CANCEL-MESSAGE . 2)))


(defun encode-system-tray-opcode (type)
    (cdr (assoc type +SYSTEM-TRAY-OPCODES-ALIST+)))
(defun decode-system-tray-opcode (type)
    (car (rassoc type +SYSTEM-TRAY-OPCODES-ALIST+)))


(defclass widget ()
  ((parent :accessor parent)
   (win :accessor win)
   (repaint-p :accessor repaint-p)
   (handler :accessor handler :initform (handler-vector))
   (children :accessor children)))


(defgeneric wdisplay (widget)
  (:documentation "Returns the display of a given WIDGET."))
(defgeneric wscreen (widget)
  (:documentation "Returns the screen of a given WIDGET."))
(defgeneric wroot (widget)
  (:documentation "Returns the widget's root WINDOW."))
(defgeneric wmap (widget)
  (:documentation "Maps the WIDGET."))
(defgeneric wunmap (widget)
  (:documentation "Unmaps the WIDGET."))
(defgeneric wwidth (widget)
  (:documentation "Returns the WIDGET width in pixels."))
(defgeneric wheight (widget)
  (:documentation "Returns the WIDGET height in pixels."))
(defgeneric wdestroy (widget)
  (:documentation "Destroys a widget."))
(defgeneric make-handler (widget)
  (:documentation "Builds the event handler for a given widget"))
(defgeneric repaint (widget)
  (:documentation "Repaints WIDGET"))

(defmethod wdisplay ((w widget))
  (window-display (win w)))

(defmethod wscreen ((w widget))
  (find (drawable-root (win w))
	(display-roots (wdisplay w))
	:key #'screen-root
	:test #'window-equal))

(defmethod wroot ((w widget))
  (drawable-root (win w)))

(defmethod wmap ((w widget))
  (map-window (win w)))

(defmethod wunmap ((w widget))
  (unmap-window (win w)))

(defmethod wwidth ((w widget))
  (drawable-width (win w)))
(defmethod (setf wwidth) ((value integer) (w widget))
  (setf (drawable-width (win w)) value))

(defmethod wheight ((w widget))
  (drawable-height (win w)))
(defmethod (setf wheight) ((value integer) (w widget))
  (setf (drawable-height (win w)) value))

(defmethod wdestroy ((w widget))
  (destroy-window (win w)))

(defmethod make-handler ((w widget))
  (handler-vector))

(defclass iconpack (widget)
  ((swin :accessor swin)
   (sidx :accessor sidx :initform nil)
   (icons :accessor icons :initform nil)
   (icon-height :accessor icon-height :initform 22 :initarg :icon-height)
   (sel-padding :accessor sel-padding :initform 1 :initarg :sel-padding)
   (sel-thickness :accessor sel-thickness :initform 2 :initarg :sel-thickness)))


(defmethod repaint ((pack iconpack))
  (setf (repaint-p pack) nil)
  (let ((x 0))
    ;;(setf (icons pack) (sort (icons pack) (curry #'icon<= pack)))
    (dolist (icon (icons pack))
      (setf (drawable-x icon) x)
      (incf x (drawable-width icon)))))

(defmethod make-configure-notify-handler ((pack iconpack))
  #'(lambda (&key event-window window width height &allow-other-keys)
      (let ((icon-size (icon-height pack)))
	(when (and (member event-window (icons pack) :test #'window-equal)
		   (window-equal (client event-window) window))
	  (dformat 2 "CONFIGURE ~S ~%" (list width height))
	  (socket-resize event-window
			 (scale-icon-width icon-size width height)
			 icon-size)))
      (setf (repaint-p pack) t)
      t))

(defun scale-icon-width (icon-size width height)
  (let ((aspect-ratio (if (or (zerop height) (zerop width))
			  1
			  (/ width height))))
   (ceiling (* icon-size  (max 1 aspect-ratio)))))

(defmethod make-handler ((ip iconpack))
  (combine-handlers (handler-vector
		     ((:configure-notify) (event-window window width height)
		      (let ((icon-size (icon-height ip)))
			(when (and (member event-window (icons ip) :test #'window-equal)
				   (window-equal (client event-window) window))
			  (dformat 2 "CONFIGURE ~S ~%" (list width height))
			  (socket-resize event-window
					 (scale-icon-width icon-size width height)
					 icon-size)))
		      (setf (repaint-p ip) t)
		      t)
		     ((:client-message) (window type data)
		      (case type
			((:_XEMBED)
			 (let ((opcode (decode-xembed-message-type (elt data 1))))
			   (when (and (window-equal window (win ip))
				      (eq opcode :xembed-protocol-finished))
			     (let ((socket (xlib::lookup-window (wdisplay ip)
								(elt data 3))))
			       (setf (icons ip) (remove socket (icons ip)
							:test #'window-equal))
			       (destroy-socket socket nil)
			       (setf (repaint-p ip) nil)
			       t)))))))
		    (socket-list-handler-vector #'(lambda () (icons ip)))))

(defgeneric add-icon (widget icon &key &allow-other-keys)
  (:documentation "Adds ICON to WIDGET."))

(defmethod add-icon ((pack iconpack) (icon window) &key (pos :tail))
  (let ((sock (create-socket nil :parent (win pack)
			     :background :parent-relative
			     :depth (drawable-depth (win pack))
			     :x 0 :y 0
			     :width (icon-height pack) :height (icon-height pack))))
    (xlib::withdraw-window icon (wscreen pack))
    (embed sock icon t)
    (map-window sock)
    (map-subwindows sock)
    (setf (repaint-p pack) t)
    ;; TODO ordering!
    (case pos 
      (:tail
       (setf (icons pack) (append (icons pack) (list sock))))
      (:head
       (push sock (icons pack))))
    (socket-activate sock)
    (map-window sock)))

(defparameter +iconpack-event-mask+
  (make-event-mask :property-change :exposure :enter-window :leave-window))

(defmethod wmap :before ((ip iconpack))
  (mapc #'map-window (icons ip)))

(defun iconpack-height (iconpack)
  (+ (icon-height iconpack)
     (sel-thickness iconpack)
     (sel-padding iconpack)))
(defun iconpack-sel-win-y (iconpack)
  (+ (icon-height iconpack)
     (sel-padding iconpack)))
 
(defmethod initialize-instance :after ((ip iconpack) &key x y parent sel-color bg-color)
  (assert (not (null parent)))
  (let ((root-window (wroot parent)))
    (setf (win ip) (create-window :parent (win parent)
				  :x x
				  :y y
				  :depth (drawable-depth root-window)
				  :width (icon-height ip)
				  :height (iconpack-height ip)
				  :event-mask +iconpack-event-mask+
				  :background (alloc-color (window-colormap root-window)
							   bg-color)))
    (setf (swin ip) (create-window :parent (win ip) :depth (drawable-depth root-window)
				   :x 0 :y (iconpack-sel-win-y ip)
				   :width 1 :height (sel-thickness ip)
				   :background (alloc-color (window-colormap root-window)
							    sel-color)))))

(define-condition contents-changed (error)
  ((widget :initarg :widget)))

(defclass systray (widget)
  ((vpack :accessor vpack)
   (selection-name :accessor selection-name)
   (sow :accessor sow)
   (fpw :accessor fpw)))


(defparameter +systray-event-mask+
  (make-event-mask :property-change :exposure :enter-window :leave-window))
(defparameter +systray-fpw-event-mask+
  (make-event-mask :property-change :key-press :key-release))

(defun calc-tray-length (tray)
  (calc-ip-length (vpack tray)))

(defmethod repaint ((tray systray))
  (repaint (vpack tray)))


(defmethod add-icon ((tray systray) (icon integer) &key)
  (let ((iconwin (xlib::lookup-window (wdisplay tray) icon)))
    (add-icon tray iconwin)))

(defmethod add-icon ((tray systray) (icon window) &key)
  (add-icon (vpack tray) icon :pos :tail))

(defmethod make-handler #|:after|# ((tray systray))
  (combine-handlers (make-handler (vpack tray))
		    (handler-vector
		     ((:client-message) (type data)
		      (case type
			((:_NET_SYSTEM_TRAY_OPCODE)
			 (format t "POTITA~%")
			 (destructuring-bind (timestamp message data1 data2 data3)
			     (coerce data 'list)
			   (declare (ignorable data2 data3))
			   (update-timestamp (fpw tray) timestamp)
			   (let ((opcode (decode-system-tray-opcode message)))
			     (case opcode
			       (:system-tray-request-dock
				(format t "ADDING ICON ~X~%" data1)
				(add-icon tray data1)
				t)
			       (:system-tray-begin-message t)
			       (:system-tray-cancel-message t))))))))))

(defun tray-selection-name (screen display)
  (intern 
   (format nil "_NET_SYSTEM_TRAY_S~a" (xlib::screen-position screen display))
   'keyword))

(defun send-manager-notification (tray)
  (let ((root-window (wroot tray))
	(atom-id (intern-atom (wdisplay tray) (selection-name tray))))
    (send-event root-window :client-message (make-event-mask :structure-notify)
		:window root-window
		:type :MANAGER
		:format 32
		:data (vector *timestamp* atom-id (window-id (sow tray)) 0 0)
		:propgate-p nil)))


(define-condition selection-acquisition-error (error)
  ())
    
(defun systray-fdo-init (tray)
  (change-property (sow tray) :_NET_SYSTEM_TRAY_ORIENTATION #(0)
		   :_NET_SYSTEM_TRAY_ORIENTATION 32)
  (setf (selection-name tray) (tray-selection-name (wscreen tray) (wdisplay tray)))
  (setf (selection-owner (wdisplay tray) (selection-name tray))
	(sow tray))
  (when (not (window-equal (selection-owner (wdisplay tray) (selection-name tray))
			   (sow tray)))
	(error 'selection-acquisition-error))
  (update-timestamp (fpw tray))
  (send-manager-notification tray))

  

(defmethod initialize-instance :after ((tray systray)
				       &key screen x y (sel-padding 1) (sel-thickness 2)
				       (sel-color (make-color :red 0 :green 1 :blue 0))
				       (vpack-bg-color (make-color :red 0.5 :green 0.5
								   :blue 0.5))
				       (bg-color (make-color :red 1 :green 1 :blue 1))
				       (icon-height 22))
  (let ((root-window (screen-root screen)))
    (setf (win tray) (create-window :parent (screen-root screen)
				    :x x
				    :y y
				    :width 1
				    :depth (drawable-depth root-window)
				    :height 1
				    :background (alloc-color (window-colormap root-window)
							     bg-color)
				    :event-mask +systray-event-mask+))
    (setf (fpw tray) (create-window :parent (win tray)
				    :x -1 :y -1
				    :width 1 :height 1
				    :event-mask +systray-fpw-event-mask+))
    (setf (sow tray) (create-window :parent (win tray)
				    :x -1 :y -1
				    :width 1 :height 1))
    (setf (vpack tray) (make-instance 'iconpack :x 0 :y 0 :parent tray :sel-color sel-color
				      :sel-thickness sel-thickness :sel-padding sel-padding
				      :bg-color vpack-bg-color
				      :icon-height icon-height))
    (setf (wwidth tray) (wwidth (vpack tray)))
    (setf (wheight tray) (wheight (vpack tray)))
    (systray-fdo-init tray)))



(defmethod wmap :before ((tray systray))
  (map-window (fpw tray))
  (map-window (sow tray))
  (wmap (vpack tray)))


(define-condition application-window-closed (error)
  ())

(defun basic-app-event-handler (display app-window focus-proxy)
  (handler-vector
   ((:client-message) (window type data)
    (case type
      ((:WM_PROTOCOLS)
       (let ((protocol-name (atom-name display (elt data 0)))
	     (timestamp (elt data 1)))
	 (when (window-equal app-window window)
	   (case protocol-name
	     ((:WM_DELETE_WINDOW)
	      (error 'application-window-closed))
	     ((:WM_TAKE_FOCUS)
	      (handler-case
		  (progn (set-input-focus display focus-proxy :parent timestamp)
			 (display-finish-output display)
			 t)
		(xlib::x-error () (error "SET-INPUT-FOCUS failed"))))))))))))

(defvar *systray*)

(defun start-systray (&optional (host ""))
  (dformat 2 "SCHIFO~%")
  (let* ((display (open-display host))
	 (screen (print (first (display-roots display))))
	 ;; (black (xlib:screen-black-pixel screen))
	 (tray (make-instance 'systray :x 0 :y 0 :screen screen))
	 (hnd (combine-handlers (basic-app-event-handler display (win tray) (fpw tray))
				(make-handler tray)))
	 (exit nil))
    (format t "Starting tray~%")
    (setf *systray* tray)
    (setf (wm-protocols (win tray))
	  (remove-duplicates (append (list :WM_TAKE_FOCUS :WM_DELETE_WINDOW)
				     (wm-protocols (win tray)))))
    (setf (wm-protocols (fpw tray))
	  (remove-duplicates (append (list :WM_TAKE_FOCUS :WM_DELETE_WINDOW)
				     (wm-protocols (fpw tray)))))
    (format t "Mapping~%")
    (wmap tray)
    (format t "Entering Loop~%")
    (do ()
	(exit)
      (format t "#############################################~%")
      (handler-case 
	  (progn
	    (display-finish-output display)
	    (process-event display :discard-p t :handler hnd))
	(application-window-closed () (setf exit t))
	(contents-changed (ip) (repaint tray))))
    (when exit
      (wdestroy tray)
      (close-display display))))
