;;;; aa-svg.asd

(asdf:defsystem #:aa-svg
  :serial t
  :description "Describe aa-svg here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-svg #:alexandria #:fset)
  :components ((:file "package")
               (:file "aa-svg")
	       (:file "point")
	       (:file "image")
	       (:file "rectangle")
	       (:file "classify")
	       (:file "lines")
	       (:file "runs")
	       (:file "text")
	       (:file "elements")))

