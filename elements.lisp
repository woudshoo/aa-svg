(in-package :aa-svg)


(defun collect-elements (image)
  (let ((h-lines (find-horizontal-lines image))
	(v-lines (find-vertical-lines image)))
    (mapc (lambda (line) (remove-class image line #\t)) h-lines)
    (mapc (lambda (line) (remove-class image line #\t)) v-lines)
    (concatenate 'list
		 h-lines
		 v-lines
		 (find-text image))))
