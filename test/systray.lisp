(in-package :xembed-user)
(defparameter +icon-height+ 16)
(defparameter +grow-direction+ :right)



(defparameter +SYSTEM-TRAY-OPCODES-ALIST+
  '((:SYSTEM-TRAY-REQUEST-DOCK . 0)
    (:SYSTEM-TRAY-BEGIN-MESSAGE . 1)
    (:SYSTEM-TRAY-CANCEL-MESSAGE . 2)))


(defun encode-system-tray-opcode (type)
    (cdr (assoc type +SYSTEM-TRAY-OPCODES-ALIST+)))
(defun decode-system-tray-opcode (type)
    (car (rassoc type +SYSTEM-TRAY-OPCODES-ALIST+)))

(defun fchain (fc w what)
  (let* ((x (cond ((member what '(:xembed-focus-next :next))  1)
		  ((member what '(:xembed-focus-prev :prev)) -1)
		  ((integerp what) what)
		  (t (error "WHAT should be either :next, :prev or an integer number"))))
	 (idx (mod (+ x (position w fc :test #'window-equal))
		   (length fc))))
    (elt fc idx)))

(defun fchain1 (fc w opcode)
  (case opcode
    (:xembed-focus-next (values (fchain fc w opcode)
				:xembed-focus-first))
    (:xembed-focus-prev (values (fchain fc w opcode)
				:xembed-focus-last))
    (:xembed-request-focus (values w :xembed-focus-current))))

(defclass systray ()
  ((fpw :accessor fpw)
   (sow :accessor sow)
   (tlw :accessor tlw)
   (sel-idx :accessor sel-idx)
   (sel-win :accessor sel-win)
   (selection-name :accessor selection-name)
   (icon-size :accessor icon-size)
   (handler :accessor handler)
   (exitp :accessor exitp)
   (unhidden-icon-ordering :accessor uiordering)
   (screen :accessor xscreen)
   (focus :accessor current-focus)
   (icons :accessor icons)))

(defmethod xdisplay (tray)
  (window-display (tlw tray)))

(defun client-wm-class (socket)
  (second (multiple-value-list (get-wm-class (client socket)))))
(defmethod icon-classes ((tray systray))
  (mapcar #'client-wm-class
	  (icons tray)))

(defparameter *tray-config-file* "/tmp/trayconf.lisp")

(defmethod dump-current-ordering ((tray systray))
  (with-open-file (s *tray-config-file* :direction :output :if-exists :supersede)
    (print (list :unhidden (uiordering tray)
		 :hidden nil )
	   s)))

(defmethod load-config ((tray systray))
  (with-open-file (s *tray-config-file* :direction :input)
      (destructuring-bind (&key unhidden hidden)
	  (read s)
	(setf (uiordering tray) unhidden))))


(defmethod send-manager-notification ((tray systray))
  (let ((root-window (drawable-root (tlw tray)))
	(atom-id (intern-atom (window-display (tlw tray)) (selection-name tray))))
    (send-event root-window :client-message (make-event-mask :structure-notify)
		:window root-window
		:type :MANAGER
		:format 32
		:data (vector *timestamp* atom-id (window-id (sow tray)) 0 0)
		:propgate-p nil)))

(defun tray-selection-name (screen display)
  (intern 
   (format nil "_NET_SYSTEM_TRAY_S~a" (xlib::screen-position screen display))
   'keyword))

(defun scale-icon-width (icon-size width height)
  (let ((aspect-ratio (if (or (zerop height) (zerop width))
			  1
			  (/ width height))))
   (ceiling (* icon-size  (max 1 aspect-ratio)))))

(defmethod icon-rank ((tray systray) (socketwin window))
  (position (client-wm-class socketwin) (uiordering tray) :test #'string=))
    

(defmethod icon<= ((tray systray) icon1 icon2)
  (let ((ir1 (icon-rank tray icon1))
	(ir2 (icon-rank tray icon2)))
    (cond ((null ir1) nil)
	  ((null ir2) t)
	  ((<= ir1 ir2) t)
	  (t nil))))

(defmethod repaint ((tray systray))
  (let ((x 0))
    (setf (icons tray) (sort (icons tray) (curry #'icon<= tray)))
    (dolist (icon (icons tray))
      (setf (drawable-x icon) x)
      (incf x (drawable-width icon)))))

(defmethod icon-rescale ((tray systray) socketwin clientwin)
  (let ((icon-size (icon-size tray)))
    (let ((width (drawable-width clientwin))
	  (height (drawable-height clientwin)))
      (socket-resize socketwin
		     (scale-icon-width icon-size width height)
		     icon-size))))

(defmethod make-configure-notify-handler ((tray systray))
  #'(lambda (&key event-window window width height &allow-other-keys)
      (let ((icon-size (icon-size tray)))
	(when (and (member event-window (icons tray) :test #'window-equal)
		   (window-equal (client event-window) window))
	  (dformat 2 "CONFIGURE ~S ~%" (list width height))
	  (socket-resize event-window
			 (scale-icon-width icon-size width height)
			 icon-size)))
      (repaint tray)
      t))




(defmethod destroy ((tray systray))
  (destroy-window (tlw tray))
  (destroy-window (fpw tray))
  (destroy-window (sow tray)))

(defun display-selection-window (tray)
  (let ((sock (elt (icons tray) (sel-idx tray)))
	(sel-win (sel-win tray)))
    (setf (drawable-x sel-win) (drawable-x sock))
    (setf (drawable-y sel-win) (+ (drawable-y sock) (drawable-height sock)))
    (window-resize sel-win (drawable-width sock) 3)
    (map-window sel-win)
    (setf (window-priority sel-win) :top-if)))


(defmethod add-icon ((tray systray) icon-window-id)
  ;; XEMBED: create socket, embed and activate
  (let ((sock (create-socket nil (fpw tray) :parent (tlw tray)
			     :background (xlib:screen-white-pixel (xscreen tray))
			     :x (* (icon-size tray) (length (icons tray))) :y 0
			     :width (icon-size tray) :height (icon-size tray)))
	(tray-icon (xlib::lookup-window (xdisplay tray) icon-window-id)))
    (xlib::withdraw-window tray-icon (xscreen tray))
    ;;(icon-rescale tray sock tray-icon)
    (embed sock tray-icon t 0 0)
    (map-window sock)
    (map-subwindows sock)
    ;; Add the icon and update the ordering
    (setf (icons tray) (append (icons tray) (list sock)))
    (setf (uiordering tray) (remove-duplicates (append (uiordering tray) (list (client-wm-class sock))) :from-end t :test #'string=))
    ;;(pushnew sock (icons tray))
    (socket-activate sock)
    (map-window sock)))

  
(defmethod move-icon ((tray systray) where fn)
  (let ((icon-class (client-wm-class (elt (icons tray) (sel-idx tray))))
	(prev-class (client-wm-class (elt (icons tray) (mod (funcall fn (sel-idx tray)) (length (icons tray)))))))
    (setf (sel-idx tray) (funcall fn (sel-idx tray)))
    (setf (uiordering tray)
	  (move-next-to icon-class prev-class (uiordering tray) :test #'equalp :where where))))
(defmethod move-icon-left ((tray systray))
  (move-icon tray :left #'1-))
(defmethod move-icon-right ((tray systray))
  (move-icon tray :right #'1+))
	

(defmethod make-handler-vector ((tray systray))
  (declare (optimize (debug 3) (speed 0)))
  (handler-vector
   ((:key-press)(code sequence time event-window child root-x root-y x y state same-screen-p event-key)
    (when (window-equal event-window (fpw tray))
      (case (keycode->character (xdisplay tray) code state)
	(#\p (dformat 2 "Ordering : ~S" (icon-classes tray))
	     t)
	(#\d (dump-current-ordering tray)
	     t)
	(#\L (dformat 2 "Loading tray-info..")
	     (load-config tray)
	     (repaint tray)
	     (dformat 2 "OK.~%")
	     t)
	(#\v (cond ((sel-idx tray)
		    (unmap-window (sel-win tray))
		    (setf (sel-idx tray) nil))
		   (t (setf (sel-idx tray) 0)
		      (display-selection-window tray)))
	     t)
	(#\h (when (sel-idx tray)
	       (setf (sel-idx tray) (mod (1- (sel-idx tray)) (length (icons tray))))
	       (display-selection-window tray))
	     t)
	(#\l (when (sel-idx tray)
	       (setf (sel-idx tray) (mod (1+ (sel-idx tray)) (length (icons tray))))
	       (display-selection-window tray))
	     t)
	(#\j (when (sel-idx tray)
	       (dformat 2 "moving icon~%")
	       (move-icon-left tray)
	       (dump-current-ordering tray)
	       (display-selection-window tray)
	       (repaint tray)))
	(#\k (when (sel-idx tray)
	       (dformat 2 "moving icon~%")
	       (move-icon-right tray)
	       (dump-current-ordering tray)
	       (display-selection-window tray)
	       (repaint tray)))
	(otherwise (when (and (current-focus tray) (client (current-focus tray)))
		     (send-event (client (current-focus tray)) event-key nil
				 :code code :sequence sequence :time time :window (client (current-focus tray))
				 :child child :root-x root-x :root-y root-y :x x :y y :state state
				 :same-screen-p same-screen-p)
		     t)))))

		 
   ((:key-release) (code sequence time event-window child root-x root-y x y state same-screen-p event-key)
    (when (and (current-focus tray) (client (current-focus tray)))
      (send-event (client (current-focus tray)) event-key nil
		  :code code :sequence sequence :time time :window (client (current-focus tray))
		  :child child :root-x root-x :root-y root-y :x x :y y :state state
		  :same-screen-p same-screen-p)
      t))
   ((:client-message) (window type format data)
    (declare (ignorable format))
    (case type
      ((:WM_PROTOCOLS)
       (when (window-equal window (tlw tray))
	 (let ((protocol-name (atom-name (xdisplay tray) (elt data 0)))
	       (timestamp (elt data 1)))
	   (case protocol-name
	     (:WM_DELETE_WINDOW (setf (exitp tray) t))
	     (:WM_TAKE_FOCUS (update-timestamp (tlw tray) timestamp)
			     (handler-case
				 (progn (set-input-focus (xdisplay tray) (fpw tray) :parent timestamp)
					(display-finish-output (xdisplay tray))
					t)
			       (xlib::x-error () (error "SET-INPUT-FOCUS failed"))))))))
      ((:_XEMBED)
       (when (window-equal window (tlw tray))
	 (let ((opcode (decode-xembed-message-type (elt data 1))))
	   (case opcode
	     (:xembed-protocol-finished
	      (dformat 2 "PROTOCOL FINISHED MESSAGE ~%")
	      (let ((socket (xlib::lookup-window (xdisplay tray) (elt data 3))))
		(dformat 2 "REMOVE ICON: ~x~%" (elt data 2))
		(setf (icons tray) (remove socket (icons tray) :test #'window-equal))
		(destroy-socket socket nil)
		(repaint tray)
		t))))))
	       
      ((:_NET_SYSTEM_TRAY_OPCODE)
       (destructuring-bind (timestamp message data1 data2 data3)
	   (coerce data 'list)
	 (update-timestamp (tlw tray) timestamp)
	 (let ((opcode (decode-system-tray-opcode message)))
	   (dformat 2 "TRAY-MESSAGE[~S](~S)~%" window opcode)
	   (case opcode
	     (:system-tray-request-dock
	      (add-icon tray data1)
	      (dump-current-ordering tray)
	      ;;(unmap-systray tray)
	      ;;(map-systray tray)
	      (repaint tray))
	     (:system-tray-begin-message t)
	     (:system-tray-cancel-message t)))))))))

(defmethod initialize-instance ((tray systray) &key screen (x 0) (y 0) (icon-size 16) background)
  (setf (sel-idx tray) nil)
  (setf (icon-size tray) icon-size)
  (setf (exitp tray) nil)
  (setf (icons tray) nil)
  (setf (current-focus tray) nil)
  (setf (xscreen tray) screen)
  (setf (uiordering tray) nil)
  (when (probe-file *tray-config-file*)
    (load-config tray))
  (let* ((root-window (screen-root screen))
	 (display (window-display root-window)))
    (setf (selection-name tray) (tray-selection-name screen display))
    (dformat 2 "CREATING WINDOWS..")
    (handler-case
	(progn 
	  (let* ((depth (print (drawable-depth root-window)))
		 (red-pixel (alloc-color (window-colormap root-window) (make-color :red 1 :green 0 :blue 0))))
	    (setf (tlw tray) (create-window :parent root-window
					    :x x :y y :width icon-size  :height icon-size :background background
					    :event-mask (make-event-mask :property-change :exposure :enter-window :leave-window)))
	    (setf (fpw tray) (create-window :parent (tlw tray)
					    :x -1 :y -1 :width 1 :height 1
					    :event-mask (make-event-mask :property-change :key-press :key-release)))
	    (setf (sow tray) (create-window :parent (tlw tray)
					    :x -1 :y -1 :width 1 :height 1))
	    (setf (sel-win tray) (create-window :depth depth :parent (tlw tray) :x 0 :y 0 :width 1 :height 1 :background red-pixel
						:border-width 0 :border red-pixel))
	    (setf (wm-protocols (fpw tray)) '(:WM_TAKE_FOCUS))
	    (setf (wm-protocols (tlw tray)) '(:WM_DELETE_WINDOW :WM_TAKE_FOCUS))
	    (display-finish-output display)))
	  (x-error () (error "Error during tray creation.")))
      (dformat 2 "OK.~%SETTING SELECTION..")
      (change-property (sow tray) :_NET_SYSTEM_TRAY_ORIENTATION #(0) :_NET_SYSTEM_TRAY_ORIENTATION 32)
      (setf (selection-owner display (selection-name tray))
	(sow tray))
      (when (not (window-equal (selection-owner display (selection-name tray))
			       (sow tray)))
	(destroy tray)
	(error "Couldn't get the selection"))
      (dformat 2 "OK.~%UPDATING TIMESTAMP..")
      (update-timestamp (fpw tray))
      (dformat 2 "OK.~%SENDING MANAGER NOTIFICATION..")
      (send-manager-notification tray)
      (dformat 2 "OK.~%SETTING EVENT HANDLERS..")
      (let ((iconlist-event-handler-vector
	     (socket-list-handler-vector #'(lambda () (icons tray)))))
	(setf (svref iconlist-event-handler-vector (handler-pos :configure-notify))
	      (make-configure-notify-handler tray))
	(setf (handler tray) (combine-handlers iconlist-event-handler-vector
					       (make-handler-vector tray))))
      (dformat 2 "OK.~%")))
  


(defun unmap-systray (systray)
  (mapc #'map-window (list (tlw systray) (sow systray) (fpw systray))))
(defun map-systray (systray)
  (map-window (tlw systray))
  (map-window (sow systray))
  (map-window (fpw systray)))

(defvar *systray* nil)
(defun start-systray (&optional icon-size (host ""))
  (dformat 2 "SCHIFO~%")
  (let* ((display (open-display host))
	 (screen (print (first (display-roots display))))
	 ;; (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (tray (make-instance 'systray :icon-size icon-size :background white :screen screen)))
    (setf *systray* tray)
    (map-systray tray)
    (do ()
	((exitp tray))
      (display-finish-output display)
      (process-event display :discard-p t :handler (handler tray)))
    (when (exitp tray)
      (destroy tray)
      (close-display display))))

	   ;; (handlers (handler-vector
	   ;; 	      (:exposure () t)
	   ;; 	      ((:key-press :key-release) (code sequence time event-window child root-x root-y x y state same-screen-p event-key)
	   ;; 	       (when (and (client current-focus) (window-equal event-window focus-proxy-window))
	   ;; 		 (send-event (client current-focus) event-key nil
	   ;; 			     :code code :sequence sequence :time time :window (client current-focus)
	   ;; 			     :child child :root-x root-x :root-y root-y :x x :y y :state state
	   ;; 			     :same-screen-p same-screen-p))
	   ;; 	       t)
	   ;; 	      ((:enter-notify :leave-notify) (window kind event-key)
	   ;; 	       (when (and (window-equal window toplevel-window)
	   ;; 			  (not (eq kind :inferior)))
	   ;; 		 (format t "ENTER(~a)~%" kind)
	   ;; 		 (mapc (case event-key
	   ;; 			 (:enter-notify #'socket-activate)
	   ;; 			 (:leave-notify #'socket-deactivate))
	   ;; 		       focus-chain)))
	   ;; 	      ((:client-message) (window type data)
	   ;; 	       (format t "CLIENT MESSAGE[~S, ~S]:: " type window)
	   ;; 	       (case type
	   ;; 		 (:_XEMBED
	   ;; 		  (let ((client (client window))
	   ;; 			(timestamp (elt data 0))
	   ;; 			(opcode (decode-xembed-message-type (elt data 1)))
	   ;; 			(detail (decode-xembed-detail (elt data 2)))
	   ;; 			(data1 (elt data 3))
	   ;; 			(data2 (elt data 4)))
	   ;; 		    (update-timestamp focus-proxy-window timestamp)
	   ;; 		    (case opcode
	   ;; 		      ((:xembed-focus-next :xembed-focus-prev :xembed-request-focus)
	   ;; 		       (when (member window focus-chain :test #'window-equal)
	   ;; 			 (multiple-value-bind (next-focus detail)
	   ;; 			     (fchain1 focus-chain window opcode)
	   ;; 			   (socket-focus-out current-focus)
	   ;; 			   (setf current-focus next-focus)
	   ;; 			   (socket-focus-in current-focus detail)
	   ;; 			   t))))))))))
