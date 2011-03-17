(asdf:defsystem xembed
  :depends-on (#:clx)
  :serial t
  :components ((:file "package")
	       (:module "utils" :serial t
			:components ((:file "debug")
				     (:file "utils")
				     (:file "xlib-utils")))
	       (:file "xembed-core")
	       (:file "socket")))


