(in-package #:aa-svg)


(defclass text (run)
  nil)


(defmethod find-text (image)
  (find-simple-x-runs image (lambda (point image)
			      (has-class-at-point image point #\t))
		      'text))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SVG 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod write-svg ((scene cl-svg:svg-toplevel) (text text))
  (with-slots (point-a content) text
    (cl-svg:text scene (:x (* 10 (point-x point-a))
			   :y (+ 4 (* 15 (point-y point-a))))
      (text text))))
