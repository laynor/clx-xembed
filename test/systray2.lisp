(defpackage :test-systray
  (:use #:cl #:xembed #:xlib #:asdf))

(in-package :test-systray)


(defmacro defsystray ()
  '(defstruct (tray)
    win
    sowin
    fpwin
    viwin
    hiwin
    vicons
    hicons
    swin
    sidx
    dpy
    scn
    iheight
    spadding
    sthickness
    show-hiwin-p
    icon-bg
    hidden-classes
    vicons-ordering
    hicons-ordering
    config))

(defsystray)

(defparameter +tray-win-event-mask+
  (make-event-mask))
(defparameter +tray-fpwin-event-mask+
  (make-event-mask :key-press :key-release))
(defparameter +tray-sowin-event-mask+
  (make-event-mask))

(defparameter +tray-viwin-event-mask+
  (make-event-mask :property-change))
(defparameter +tray-hiwin-event-mask+
  (make-event-mask :property-change))

(defparameter +tray-default-config+
  `((:win-bg   .   ,(make-color))
    (:viwin-bg .   ,(make-color :red 0.8 :green 0.8 :blue 0.8))
    (:hiwin-bg .   ,(make-color :red 0.6 :green 0.6 :blue 0.6))
    (:icon-bg  .   ,(make-color :red 0.9 :green 0.9 :blue 0.9))
    (:swin-bg  .   ,(make-color :red 1 :green 0 :blue 0))
    (:iheight  .   22)
    (:spadding .   1)
    (:sthickness . 2)))

(defun config-file-name ()
  "/tmp/tray-config.lisp")

(defun dump-config (systray)
  (with-open-file (s (config-file-name) :direction :output :if-exists :supersede)
    (print (list (tray-config systray)
		 (tray-vicons-ordering systray)
		 (tray-hicons-ordering systray)
		 (tray-hidden-classes systray))
	   s)))

(defun read-config ()
  (with-open-file (s (config-file-name))
    (read s)))

(defun config-item (config item)
  (cdr (assoc item config)))

(defun calculate-tray-height (iheight spadding sthickness)
  (+ iheight spadding sthickness))

(defmacro with-config-items ((config &rest items) &body body)
  `(let ,(mapcar #'(lambda (item)
		     `(,item (config-item ,config ,(intern (symbol-name item) :keyword))))
		 items)
     ,@body))

(defun make-systray (display screen parent
		     &key (x 0) (y 0) custom-config)
  (with-config-items ((append +tray-default-config+ (first custom-config))
		      iheight sthickness spadding win-bg viwin-bg hiwin-bg swin-bg
		      icon-bg)
    (let* ((root-window (drawable-root parent))
	   (depth (drawable-depth root-window))
	   (tray-height (calculate-tray-height iheight spadding sthickness))
	   (win (create-window :parent parent 
			       :x x :y y
			       :depth depth
			       :width iheight
			       :height tray-height
			       :background (alloc-color (window-colormap root-window) win-bg)
			       :event-mask +tray-win-event-mask+)))
      (flet ((create-1x1-invisible-win (event-mask)
	       (create-window :parent win
			      :x -1 :y -1
			      :width 1 :height 1
			      :event-mask event-mask))
	     (create-visible-win (bgcolor event-mask x y)
	       (create-window :parent win :depth depth
			      :x x :y y
			      :width iheight :height tray-height
			      :background (alloc-color (window-colormap root-window)
						       bgcolor)
			      :event-mask event-mask)))
	(let* ((fpwin (create-1x1-invisible-win +tray-fpwin-event-mask+))
	       (sowin (create-1x1-invisible-win +tray-sowin-event-mask+))
	       (viwin (create-visible-win viwin-bg +tray-viwin-event-mask+ iheight 0))
	       (hiwin (create-visible-win hiwin-bg +tray-hiwin-event-mask+ 0 0))
	       (swin (create-visible-win swin-bg (make-event-mask) 0 0)))
	  (make-tray :win win :sowin sowin :fpwin fpwin
		     :viwin viwin :hiwin hiwin :iheight iheight
		     :vicons-ordering (second custom-config)
		     :hicons-ordering (third custom-config)
		     :hidden-classes (fourth custom-config)
		     :dpy display :scn screen :show-hiwin-p t :icon-bg icon-bg
		     :spadding spadding :sthickness sthickness :swin swin
		     :sidx nil :config (first custom-config)))))))


(defun map-systray (systray)
  (map-window (tray-win systray))
  (map-window (tray-fpwin systray))
  (map-window (tray-sowin systray))
  (map-window (tray-viwin systray))
  (if (tray-sidx systray)
      (map-window (tray-swin systray))
      (unmap-window (tray-swin systray)))
  (if (tray-show-hiwin-p systray)
      (map-window (tray-hiwin systray))
      (unmap-window (tray-hiwin systray))))

(defparameter *test-viwidth* 22)
(defparameter *test-hiwidth* 22)

(defun calculate-icon-windows-length (systray)
  (values (max (tray-iheight systray)
	       (reduce #'+ (tray-vicons systray) :key #'drawable-width))
	  (max (tray-iheight systray)
	       (reduce #'+ (tray-hicons systray) :key #'drawable-width))))


(defun tile-systray-icons (systray)
  (tile-icons (tray-hicons systray))
  (tile-icons (tray-vicons systray)))
 
(defun tile-icons (icons)
  (let ((x 0))
    (dolist (icon icons)
      (setf (drawable-x icon) x)
      (incf x (drawable-width icon)))))

(defun update-icon-packs-geometry (systray)
  (let ((viwin (tray-viwin systray))
	(hiwin (tray-hiwin systray)))
    (multiple-value-bind (viwin-width hiwin-width)
	(calculate-icon-windows-length systray)
      (cond ((tray-show-hiwin-p systray)
	     (setf (drawable-width hiwin) hiwin-width)
	     (setf (drawable-x viwin) hiwin-width)
	     (setf (drawable-width viwin) viwin-width))
	    (t (setf (drawable-x viwin) 0)
	       (setf (drawable-width viwin) viwin-width))))))

(defun icon-at-sel (systray)
  (let* ((nv (length (tray-vicons systray)))
	 (nh (length (tray-hicons systray)))
	 (tray-sidx (mod (tray-sidx systray) (+ nv nh))))
    (setf (tray-sidx systray) tray-sidx)
    (cond
      ((zerop (+ nv nh)) nil)
      ((>= tray-sidx nh)
       (elt (tray-vicons systray) (- tray-sidx nh)))
      (t (elt (tray-hicons systray) tray-sidx)))))

(defun update-swin-geometry (systray)
  (when (numberp (tray-sidx systray))
    (let ((icon (icon-at-sel systray))
	  (swin (tray-swin systray)))
      (when icon
	(multiple-value-bind (dx) (translate-coordinates (window-parent icon)
							 (drawable-x icon)
							 (drawable-y icon)
							 (tray-win systray))
	  (setf (drawable-x swin) dx)
	  (setf (drawable-y swin) (+ (tray-spadding systray) (tray-iheight systray)))
	  (setf (drawable-height swin) (tray-sthickness systray))
	  (setf (drawable-width swin) (drawable-width icon)))))))

(defun icon-rank (ordering icon)
  (position (client-wm-class icon) ordering :test #'equalp))
(defun icon-comp (ordering icon1 icon2)
  (let ((ir1 (icon-rank ordering icon1))
	(ir2 (icon-rank ordering icon2)))
    (cond ((equalp ir1 ir2) 0)
	  ((null ir1) 1)
	  ((null ir2) -1)
	  ((< ir1 ir2) -1)
	  (t 1))))
(defun icon< (ordering icon1 icon2)
  (< (icon-comp ordering icon1 icon2) 0))

(defun icon> (ordering icon1 icon2)
  (> (icon-comp ordering icon1 icon2) 0))

(defun sort-systray-icons (systray)
  (setf (tray-vicons systray)
	(sort (tray-vicons systray) (curry #'icon< (tray-vicons-ordering systray))))
  (setf (tray-hicons systray) (sort (tray-hicons systray)
				    (curry #'icon> (tray-hicons-ordering systray)))))

(defun systray-height (systray)
  (calculate-tray-height (tray-iheight systray)
			 (tray-spadding systray)
			 (tray-sthickness systray)))

(defun tray-width (systray)
  (multiple-value-bind (viwin-width hiwin-width)
      (calculate-icon-windows-length systray)
    (if (tray-show-hiwin-p systray)
	(+ viwin-width hiwin-width)
	viwin-width)))
  

(defun update-windows-geometry (systray)
  (sort-systray-icons systray)
  (tile-systray-icons systray)
  (setf (tray-show-hiwin-p systray) (or (tray-show-hiwin-p systray) (tray-sidx systray)))
  (update-icon-packs-geometry systray)
  (update-swin-geometry systray)
  (setf (drawable-width (tray-win systray))
	(tray-width systray))
  (setf (drawable-height (tray-win systray))
	(systray-height systray)))

    
(defun update-systray (systray &optional (map-p t))
  (update-windows-geometry systray)
  (when map-p
    (map-systray systray)))

(defun set-hiwin-visibility (systray visibility)
  (setf (tray-show-hiwin-p systray) visibility)
  (setf (tray-sidx systray) (and visibility (tray-sidx systray)))
  (update-systray systray))

(defun show-hiwin (systray)
  (set-hiwin-visibility systray t))
(defun hide-hiwin (systray)
  (set-hiwin-visibility systray nil))
	 
(defun tray-selection-name (screen display)
  (intern 
   (format nil "_NET_SYSTEM_TRAY_S~a" (xlib::screen-position screen display))
   'keyword))


;;; TODO check for errors in this function
(defun set-tray-property (systray)
  (change-property (tray-sowin systray)
		   :_NET_SYSTEM_TRAY_ORIENTATION #(0)
		   :_NET_SYSTEM_TRAY_ORIENTATION 32))

(defun set-tray-selection-owner (systray)
  (setf (selection-owner (tray-dpy systray) (tray-selection-name (tray-scn systray)
								 (tray-dpy systray)))
	(tray-sowin systray)))
(defun send-manager-notification (systray)
  (let ((root-window (drawable-root (tray-win systray)))
	(atom-id (intern-atom  (tray-dpy systray) (tray-selection-name (tray-scn systray)
								       (tray-dpy systray)))))
    (send-event root-window :client-message (make-event-mask :structure-notify)
		:window root-window
		:type :MANAGER
		:format 32
		:data (vector *timestamp* atom-id (window-id (tray-sowin systray)) 0 0)
		:propgate-p nil)))

(defun init-fdo-systray (systray)
  (set-tray-property systray)
  (set-tray-selection-owner systray)
  (send-manager-notification systray))
  


(defun init-xembed-systray (systray)
  (mapc #'(lambda (win)
	    (setf (wm-protocols win)
		  (union '(:WM_TAKE_FOCUS) (wm-protocols win))))
	(list (tray-win systray)
	      (tray-fpwin systray))))
  

(defun init-app-mode-systray (systray appmode)
  (let ((traywin (tray-win systray)))
    (setf (wm-protocols traywin)
	  (union '(:WM_DELETE_WINDOW) (wm-protocols traywin)))))

(defun init-systray (systray &optional appmode)
  (init-fdo-systray systray)
  (init-xembed-systray systray)
  (init-app-mode-systray systray appmode)
  (update-systray systray nil))

(define-condition close-window-requested () ())

(defun make-basic-app-event-handler (systray)
  (handler-vector
   ((:client-message) (window type data)
    (let ((display (tray-dpy systray))
	  (app-window (tray-win systray))
	  (focus-proxy (tray-fpwin systray)))
      (case type
	((:WM_PROTOCOLS)
	 (let ((protocol-name (atom-name display (elt data 0)))
	       (timestamp (elt data 1)))
	   (when (window-equal app-window window)
	     (case protocol-name
	       ((:WM_DELETE_WINDOW)
		(error 'close-window-requested))
	       ((:WM_TAKE_FOCUS)
		(format t "SETTING INPUT FOCUS ~%")
		(handler-case
		    (progn (set-input-focus display focus-proxy :parent timestamp)
			   (display-finish-output display)
			   t)
		  (xlib::x-error () (error "SET-INPUT-FOCUS failed")))))))))))))

(defun window-wm-class (window)
  (second (multiple-value-list (get-wm-class window))))
(defun client-wm-class (icon)
  (window-wm-class (client icon)))
(defun hidden-icon-p (systray icon &optional (socket-p t))
  (member (window-wm-class (if socket-p
			       (client icon)
			       icon))
	  (tray-hidden-classes systray) :test #'equalp))

(defun initialize-icon-socket (icon-socket)
  (map-window icon-socket)
  (map-subwindows icon-socket)
  (socket-activate icon-socket))

(defun make-icon-socket (systray parent)
  (let ((root (drawable-root parent))
	(iheight (tray-iheight systray)))
    (create-socket nil :parent parent :depth (drawable-depth root)
		   :background (alloc-color (window-colormap root) (tray-icon-bg systray))
		   :x 0 :y 0
		   :width iheight :height iheight)))

(defun update-tray-visible-icon-data (systray socket)
  (push socket (tray-vicons systray))
  (setf (tray-vicons-ordering systray)
	(remove-duplicates (append (list (client-wm-class socket))
				   (tray-vicons-ordering systray))
			   :test #'equalp)))

(defun add-visible-icon (systray icon)
  (let ((icon-socket (make-icon-socket systray (tray-viwin systray))))
    (embed icon-socket icon t 0 0)
    (initialize-icon-socket icon-socket)
    (update-tray-visible-icon-data systray icon-socket)))

(defun update-tray-hidden-icon-data (systray socket)
  (push socket (tray-hicons systray))
  (pushnew (client-wm-class socket)
	   (tray-hicons-ordering systray)
	   :test #'equalp))

(defun add-hidden-icon (systray icon)
  (let ((icon-socket (make-icon-socket systray (tray-hiwin systray))))
    (embed icon-socket icon t 0 0)
    (initialize-icon-socket icon-socket)
    (update-tray-hidden-icon-data systray icon-socket)))

(defun add-icon (systray icon-id)
  (let* ((dpy (tray-dpy systray))
	 (icon (xlib::lookup-window dpy icon-id)))
    (if (hidden-icon-p systray icon nil)
	(add-hidden-icon systray icon)
	(add-visible-icon systray icon))))

(defun unhide-icon (systray icon)
  (reparent-window icon (tray-viwin systray) 0 0)
  (setf (tray-hidden-classes systray)
	(remove (client-wm-class icon) (tray-hidden-classes systray) :test #'equalp))
  (setf (tray-hicons systray) (remove icon (tray-hicons systray) :test #'window-equal))
  (update-tray-visible-icon-data systray icon)
  (update-systray systray)
  (setf (tray-sidx systray) (icon->idx systray icon)))


(defun hide-icon (systray icon)
  (reparent-window icon (tray-hiwin systray) 0 0)
  (push (client-wm-class icon) (tray-hidden-classes systray))
  (setf (tray-vicons systray) (remove icon (tray-vicons systray) :test #'window-equal))
  (update-tray-hidden-icon-data systray icon)
  (update-systray systray)
  (setf (tray-sidx systray) (icon->idx systray icon)))

(defun toggle-icon-hiding (systray icon)
  (print (list (tray-hicons-ordering systray)
	       (tray-vicons-ordering systray)))
  (cond
    ((window-equal (window-parent icon) (tray-hiwin systray))
     (unhide-icon systray icon))
    (t (hide-icon systray icon))))

(defun adjacent-icons-helper (icons idx)
  (list (cons :left (cond ((zerop idx)
			   nil)
			  (t (elt icons (1- idx)))))
	(cons :right (cond ((= idx (1- (length icons))) nil)
			   (t (elt icons (1+ idx)))))))
  
(defun adjacent-icons (systray)
  (let ((icon (icon-at-sel systray)))
    (if (hidden-icon-p systray icon)
	(adjacent-icons-helper (tray-hicons systray) (tray-sidx systray))
	(adjacent-icons-helper (tray-vicons systray) (- (tray-sidx systray)
							(length (tray-hicons systray)))))))

(defun opposite-direction (direction)
  (case direction
    (:left :right)
    (:right :left)))

(defun icon->idx (systray icon)
  (or (position icon (tray-hicons systray) :test #'window-equal)
      (+ (position icon (tray-vicons systray) :test #'window-equal)
	 (length (tray-hicons systray)))))
	
(defun move-icon (systray direction)
  (let ((icon (icon-at-sel systray))
	(adjacent (cdr (assoc direction (adjacent-icons systray)))))
    (cond ((hidden-icon-p systray icon)
	   (setf (tray-hicons-ordering systray)
		 (move-next-to (client-wm-class icon)
			       (and adjacent (client-wm-class adjacent))
			       (tray-hicons-ordering systray)
			       :where (opposite-direction direction) :test #'equalp)))
	  (t (setf (tray-vicons-ordering systray)
		   (move-next-to (client-wm-class icon)
				 (and adjacent (client-wm-class adjacent))
				 (tray-vicons-ordering systray)
				 :where direction :test #'equalp))))
    (update-systray systray)
    (setf (tray-sidx systray) (icon->idx systray icon))))

(defparameter +SYSTEM-TRAY-OPCODES-ALIST+
  '((:SYSTEM-TRAY-REQUEST-DOCK . 0)
    (:SYSTEM-TRAY-BEGIN-MESSAGE . 1)
    (:SYSTEM-TRAY-CANCEL-MESSAGE . 2)))

(defun encode-system-tray-opcode (type)
    (cdr (assoc type +SYSTEM-TRAY-OPCODES-ALIST+)))
(defun decode-system-tray-opcode (type)
    (car (rassoc type +SYSTEM-TRAY-OPCODES-ALIST+)))

(defun make-fdo-systray-event-handler (systray)
  (handler-vector
   ((:client-message) (window type data)
    (when (eq type :_NET_SYSTEM_TRAY_OPCODE)
     (destructuring-bind (timestamp message data1 data2 data3)
	 (coerce data 'list)
       (update-timestamp (tray-fpwin systray) timestamp)
       (let ((opcode (decode-system-tray-opcode message)))
	 (dformat 2 "TRAY-MESSAGE[~S](~S)~%" window opcode)
	 (case opcode
	   (:system-tray-request-dock
	    (add-icon systray data1)
	    (update-systray systray))
	   (:system-tray-begin-message t)
	   (:system-tray-cancel-message t))))))))
    
(defparameter *test-bindings* nil)
(defmacro def-testbinding (char fn)
  `(push (cons ,char ,fn) *test-bindings*))

(def-testbinding #\h #'(lambda (systray &rest args)  
			 (declare (ignorable systray args))
			 (hide-hiwin systray) t))
(def-testbinding #\s #'(lambda (systray &rest args)
			  (declare (ignorable systray args))
			  (show-hiwin systray) t))
(def-testbinding #\+ #'(lambda (systray &rest args)
			  (declare (ignorable systray args))
			  (incf *test-viwidth* 5) t))
(def-testbinding #\= #'(lambda (systray &rest args)
			  (declare (ignorable systray args))
			  (setf *test-viwidth* (max 1 (- *test-viwidth* 5))) t))
(def-testbinding #\- #'(lambda (systray &rest args)
			  (declare (ignorable systray args))
			  (incf *test-hiwidth* 5) t))
(def-testbinding #\_ #'(lambda (systray &rest args)
			  (declare (ignorable systray args))
			  (setf *test-hiwidth* (max 1 (- *test-hiwidth* 5))) t))
(def-testbinding #\j #'(lambda (systray &rest args)
			 (declare (ignorable args))
			 (let ((idx (tray-sidx systray)))
			   (setf (tray-sidx systray) (1+ (or idx -1)))
			   (update-systray systray))))
(def-testbinding #\k #'(lambda (systray &rest args)
			 (declare (ignorable args))
			 (let ((idx (tray-sidx systray)))
			   (setf (tray-sidx systray) (1- (or idx 0)))
			   (update-systray systray))))
(def-testbinding #\! #'(lambda (systray &rest args)
			 (declare (ignorable args))
			 (toggle-icon-hiding systray (icon-at-sel systray))
			 (update-systray systray)))
(def-testbinding #\u #'(lambda (systray &rest args)
			 (declare (ignorable args))
			 (move-icon systray :left)
			 (update-systray systray)))
(def-testbinding #\i #'(lambda (systray &rest args)
			 (declare (ignorable args))
			 (move-icon systray :right)
			 (update-systray systray)))
(def-testbinding #\d #'(lambda (systray &rest args)
			 (declare (ignorable args))
			 (dump-config systray)))

(defun make-test-handler (systray)
  (handler-vector
   ((:key-press)(event-window state code)
    (when (window-equal event-window (tray-fpwin systray))
      (let* ((char (keycode->character (tray-dpy systray) code state))
	     (fn (cdr (assoc char *test-bindings* :test #'equalp))))
	(when fn (funcall fn systray char)))))))

(defun scale-icon-width (icon-size width height)
  (let ((aspect-ratio (if (or (zerop height) (zerop width))
			  1
			  (/ width height))))
   (ceiling (* icon-size  (max 1 aspect-ratio)))))

(defun remove-icon (systray socket)
  (setf (tray-vicons systray)
	(remove socket (tray-vicons systray) :test #'window-equal))
  (setf (tray-hicons systray)
	(remove socket (tray-hicons systray) :test #'window-equal))
  (destroy-socket socket)
  (update-systray systray))
(defun make-systray-xembed-handler (systray)
  (let ((hnd (socket-list-handler-vector #'(lambda ()
					     (append (tray-vicons systray)
						     (tray-hicons systray))))))
    (combine-handlers (handler-vector
		       ((:client-message) (window type data)
			(when (and (eq type :_XEMBED))
			  (let ((opcode (decode-xembed-message-type (elt data 1))))
			    (case opcode
			      (:xembed-protocol-finished
			       (let ((socket (xlib::lookup-window (tray-dpy systray)
								  (elt data 3))))
				 (remove-icon systray socket)))))))
		       ((:configure-notify) (event-window window width height)
			(let ((iheight (tray-iheight systray)))
			  (when (and (member event-window (append (tray-vicons systray)
								  (tray-hicons systray))
					     :test #'window-equal)
				     (window-equal (client event-window) window))
			    (dformat 2 "CONFIGURE ~S ~%" (list width height))
			    (socket-resize event-window
					   (scale-icon-width iheight width height)
					   iheight)))
			(update-systray systray)
			t))
		      hnd)))
			

(defun make-systray-handler (systray)
  (reduce #'combine-handlers
	  (list (make-test-handler systray)
		(make-systray-xembed-handler systray)
		(make-fdo-systray-event-handler systray)
		(make-basic-app-event-handler systray))))

(defun destroy-tray (systray)
  (destroy-window (tray-win systray)))

(defvar *systray* nil)

(defun ensure-config-file-exists ()
  (unless (probe-file (config-file-name))
    (with-open-file (s (config-file-name) :direction :output)
      (print (list nil nil nil nil) s))))

(defun start-systray (&optional (host ""))
  (ensure-config-file-exists)
  (let* ((display (open-display host))
	 (screen (display-default-screen display))
	 (root-window (screen-root screen))
	 (conf (read-config))
	 (systray (make-systray display screen root-window :custom-config conf))
	 (hnd (make-systray-handler systray)))
    (setf *systray* systray)
    (init-systray systray)
    (map-systray systray)
    (handler-case
	(do () (nil)
	  (display-finish-output display)
	  (process-event display :discard-p t :handler hnd)
	  )
      (close-window-requested ()
	(destroy-tray systray)
	(close-display display)))))



