(in-package :xembed)
(defun fchain (fc w what)
  (let* ((x (cond
	      ((member what '(:xembed-focus-next :next))  1)
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

(defun xembed-focus-handler (focus-chain current-focus window opcode)
  )

(defvar *myembedder*)
(defvar *mydisplay*)
(defun toplevel-embedder-test (&optional (host ""))
  (let* ((display (open-display host))
	 (screen (first (display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (toplevel-window (create-window :parent root-window
					 :x 0 :y 0 :width 400 :height 600 :background white
					 :event-mask (make-event-mask :exposure :enter-window :leave-window)))
	 (focus-proxy-window (create-window :parent toplevel-window
					    :x -1 :y -1 :width 1 :height 1
					    :event-mask (make-event-mask :key-press :key-release)))
	 (s1 (create-socket nil focus-proxy-window :parent toplevel-window
			    :background black
			    :x 0 :y 0 :width 200 :height 100))
	 (s2 (create-socket nil focus-proxy-window
			    :background black
			    :parent toplevel-window
			    :x 0 :y 105 :width 200 :height 100))
	 (current-focus s1)
	 (focus-chain (list s1 s2))
	 (hv (socket-list-handler-vector #'(lambda () focus-chain))))
    (setf (wm-protocols focus-proxy-window) '(:WM_TAKE_FOCUS))
    (setf (wm-protocols toplevel-window) '(:WM_DELETE_WINDOW :WM_TAKE_FOCUS))

    (map-window focus-proxy-window)
    (map-window toplevel-window)
    (map-window s1)
    (map-window s2)
    (format t "embedder-ids: ~a ~a~%" s1 s2)
    (let* (exit
	   (handlers (handler-vector
		      (:exposure () t)
		      ((:key-press :key-release) (code sequence time event-window child root-x root-y x y state same-screen-p event-key)
		       (when (and (client current-focus) (window-equal event-window focus-proxy-window))
			 (send-event (client current-focus) event-key nil
				     :code code :sequence sequence :time time :window (client current-focus)
				     :child child :root-x root-x :root-y root-y :x x :y y :state state
				     :same-screen-p same-screen-p))
		       t)
		      ((:enter-notify :leave-notify) (window kind event-key)
		       (when (and (window-equal window toplevel-window)
				  (not (eq kind :inferior)))
			 (format t "ENTER(~a)~%" kind)
			 (mapc (case event-key
				 (:enter-notify #'socket-activate)
				 (:leave-notify #'socket-deactivate))
			       focus-chain)))
		      ((:client-message) (window type data)
		       (format t "CLIENT MESSAGE[~S, ~S]:: " type window)
		       (case type
			 (:WM_PROTOCOLS
			  (when (window-equal window toplevel-window)
			    (let ((protocol-name (atom-name display (elt data 0)))
				  (timestamp (elt data 1)))
			      (case protocol-name
				(:WM_DELETE_WINDOW (setf exit t))
				(:WM_TAKE_FOCUS (update-timestamp toplevel-window timestamp)
						(handler-case
						    (progn (set-input-focus display focus-proxy-window :parent timestamp)
							   (display-finish-output display)
							   t)
						  (xlib::x-error () (error "SET-INPUT-FOCUS failed"))))))))
			 (:_XEMBED
			  (let ((client (client window))
				(timestamp (elt data 0))
				(opcode (decode-xembed-message-type (elt data 1)))
				(detail (decode-xembed-detail (elt data 2)))
				(data1 (elt data 3))
				(data2 (elt data 4)))
			    (update-timestamp focus-proxy-window timestamp)
			    (case opcode
			      ((:xembed-focus-next :xembed-focus-prev :xembed-request-focus)
			       (when (member window focus-chain :test #'window-equal)
				 (multiple-value-bind (next-focus detail)
				     (fchain1 focus-chain window opcode)
				   (socket-focus-out current-focus)
				   (setf current-focus next-focus)
				   (socket-focus-in current-focus detail)
				   t))))))))))
	   (combined-handler (combine-handlers hv handlers)))
      (print combined-handler)
      (do ()
	  (exit)
	(display-finish-output display)
	(process-event display :discard-p t :handler combined-handler ))
      (when exit
	(destroy-socket s1)
	(destroy-socket s2)
	(close-display display)))))
