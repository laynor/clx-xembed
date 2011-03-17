(defpackage :xembed
  (:use #:cl #:asdf #:xlib)
  (:export #:dformat #:rformat #:curry #:move-next-to
	   #:window-resize #:window-parent #:handler-pos #:handler-vector #:combine-handlers
	   #:decode-xembed-message-type  #:update-timestamp  #:*timestamp*
	   #:create-socket #:destroy-socket #:client #:embed #:socket-activate #:socket-resize
	   #:socket-list-handler-vector))

(defpackage :xembed-user
  (:use #:cl #:asdf #:xlib #:xembed))
