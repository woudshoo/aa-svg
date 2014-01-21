(in-package :aa-svg)


(defun collect-elements (image)
  (concatenate 'list
	       (find-horizontal-lines image)
	       (find-vertical-lines image)
	       (find-text image)))
