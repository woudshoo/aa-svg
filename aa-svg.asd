;;;; aa-svg.asd

(asdf:defsystem #:aa-svg
  :description "Simple converter from ASCII diagrams to SVG"
  :author "Wim Oudshoorn <woudshoo@xs4all.nl>"
  :license "LGPL"
  :depends-on (#:cl-svg #:alexandria #:fset)
  :components ((:file "package")
               (:file "aa-svg" :depends-on ("image" "classify" "elements"))
	       (:file "point")
	       (:file "image" :depends-on ("point"))
	       (:file "runs" :depends-on ("point" "image"))
	       (:file "classify" :depends-on ("point" "runs"))
	       (:file "rectangle" :depends-on ("point"))
	       (:file "lines" :depends-on ("classify" "image" "point" "runs"))
	       (:file "text" :depends-on ("runs" "point" "classify"))
	       (:file "elements" :depends-on ("lines" "text"))))

