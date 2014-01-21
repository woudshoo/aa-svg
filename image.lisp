(in-package #:aa-svg)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Image class.
;;;
;;;
(defclass image ()
  ((raw-data :initform (make-array '(0 0) :adjustable t :initial-element #\Space) :reader raw-data)
   (classification :initform (make-array '(0 0) :adjustable t) :reader class-data))
  (:documentation "An image contains two parts, raw data as a 2
dimensional array of characters, and a 2 dimensional array of
classifications.  The classification array is conceptually the same
size as the image array, but contains information of what type of
character it is, a horizontal line, vertical line, a corner etc."))

(defmethod image-width ((image image))
  (array-dimension (raw-data image) 0))

(defmethod image-height ((image image))
  (array-dimension (raw-data image) 1))

(defmethod image-size ((image image))
  (make-point :x (image-width image)
	      :y (image-height image)))

;;;;;;;;;;;; image/point ;;;;;;;;;;;;
(defmethod char-at-point ((image image) (point point))
  (aref (raw-data image) (point-x point) (point-y point)))


(defmethod (setf char-at-point) (new-char (image image) (point point))
  (let* ((x (point-x point))
	 (y (point-y point))
	 (new-w (max (image-width image) (+ 2 x)))
	 (new-h (max (image-height image) (+ 2 y))))
    (adjust-array (raw-data image) (list new-w new-h) :initial-element #\Space)
    (setf (aref (raw-data image) x y) new-char)))

(defmethod past-end-of-row/line ((image image) (point point))
  "methods returning if the point is outside the image."
  (or (>= (point-x point) (image-width image))
      (>= (point-y point) (image-height image))))

(defmethod past-end-of-image ((image image) (point point))
  "method returning if the point is really outside the image.
THis is typically used when iterating over all points in the image."
  (and (>= (point-x point) (image-width image))
       (>= (point-y point) (image-height image))))
;;;;;;;;;;;; classification ;;;;;;;;;;;;
(defmethod classes-at-point ((image image) (point point))
  (let ((x (point-x point))
	(y (point-y point))
	(class-array (class-data image)))
    (if (or (>= x (array-dimension class-array 0))
	    (>= y (array-dimension class-array 1)))
	nil
	(aref class-array x y))))

(defmethod (setf classes-at-point) (value (image image) (point point))
  (let* ((x (point-x point))
	 (y (point-y point))
	 (class-array (class-data image))
	 (new-w (max (array-dimension class-array 0) (1+ x)))
	 (new-h (max (array-dimension class-array 1) (1+ y))))
    (adjust-array class-array (list new-w new-h) :initial-element nil)
    (setf (aref class-array x y) value)))

(defmethod has-class-at-point ((image image) (point point) (class t))
  (member class (classes-at-point image point)))


(defmethod points-with-class ((image image) (classes list))
  (let (result)
    (loop :with class-array = (class-data image)
       :for x :from 0 :below (array-dimension class-array 0) :do
       (loop :for y :from 0 :below (array-dimension class-array 1)
	  :for point = (make-point :x x :y y)
	  :do
	  (when (intersection (classes-at-point image point) classes)
	    (push point result))))
    result))

;;;;;;;;;;;; input/output ;;;;;;;;;;;;
(defmethod print-image ((image image) &optional (stream *standard-output*))
  (print-array (raw-data image) (image-width image) (image-height image) stream))

(defun print-array (array width height &optional (stream *standard-output*) (key #'identity))
  (loop :for y :from 0 :below height
     :do
     (format stream "~%")
     (loop :for x :from 0 :below width
	:do
	(write-char (funcall key (aref array x y)) stream))))


(defmethod print-classification ((image image) &optional (stream *standard-output*))
  (let ((array (class-data image)))
    (print-array array (array-dimension array 0) (array-dimension array 1) stream 
		 #'(lambda (a)
		     (or (car a) #\Space)))))

(defun make-image-from-stream (in-stream)
  "Return raw image (a 2 dimensional array) from the characters read from stream.
The resulting image is padded with spaces on both sides, so most detection code does not need to worry about
end of image"
  (let ((image (make-instance 'image)))
    (loop 
       :with x = 1
       :with y = 1
       :for char = (read-char in-stream nil nil)
       :while char
       :do
       (incf x)
       (case char
	 (#\Newline (incf y) (setf x 1))
	 ((#\Return #\Space) nil)
	 (t (setf (char-at-point image (make-point :x x :y y)) char))))
    image))

(defun make-image-from-file (file-name)
  (with-open-file (stream file-name)
    (make-image-from-stream stream)))

