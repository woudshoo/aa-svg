(in-package :aa-svg)

(defclass line (run)
  nil)


(defun find-horizontal-lines (image)
  (find-simple-x-runs image 
		      (lambda (point image) (has-class-at-point image point #\-))
		      'line))


(defun find-vertical-lines (image)
  (let (result)
    (loop :for x :from 0 :below (image-width image)
       :for start-point = nil :do
       (loop :for y :from 0 :below (image-height image)
	  :for prev-point = point
	  :for point = (make-point :x x :y y)
	  :for is-vertical = (has-class-at-point image point #\|)
	  :do
	  (if is-vertical
	      (unless start-point
		(setf start-point point))
	      (when start-point
		(push (make-instance 'line 
				     :a start-point
				     :b prev-point
				     :image image) result)
		(setf start-point nil)))))
    result))




(defmethod write-svg ((scene cl-svg:svg-toplevel) (element line))
  (with-slots (point-a point-b) element
    (cl-svg:draw scene (:line :x1 (* 10 (point-x point-a))
			      :y1 (* 15 (point-y point-a))
			      :x2 (* 10 (point-x point-b))
			      :y2 (* 15 (point-y point-b))))))


