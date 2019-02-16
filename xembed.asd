(asdf:defsystem xembed
  :depends-on (#:clx)
  :serial t
  :description "An implementation of the XEMBED protocol that integrates with CLX."
  :author "Alessandro Piras"
  :version "0.1.0"
  :license "MIT"
  :components ((:file "package")
	       (:module "utils" :serial t
			:components ((:file "debug")
				     (:file "utils")
				     (:file "xlib-utils")))
	       (:file "xembed-core")
	       (:file "socket")))
