
(in-package #:aa-svg)


(defclass run ()
  ((point-a :initarg :a :accessor point-a)
   (point-b :initarg :b :accessor point-b)
   (image :initarg :image :accessor image))
  (:documentation "A run is basically a straight line 
of positions in the image which belong together.

A run basic run is uniquely characterized by the two end points.
Subclasses can have a more fine grained distiction and 
it is entirely possible that two runs share the same
end points.

Note:  The run is directional, that is if the end points are swapped, 
       they are considered different.

Note:  That the coordinates of a run are inclusive, e.g.
       A run from (1,1) -- (1,2) contains 2 locations."))

(defmethod print-object ((obj run) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "(~D,~D) - (~D,~D)" 
	    (point-x (point-a obj)) 
	    (point-y (point-a obj))
	    (point-x (point-b obj)) 
	    (point-y (point-b obj)))))

(defmethod describe-object ((run run) stream)
  (format stream "~&~S ~A --> ~A~%"
	  run (point-a run) (point-b run)))

;;;;;;;;;;;; FSET integration ;;;;;;;;;;;;
(fset:define-cross-type-compare-methods run)

(defmethod fset:compare ((a run) (b run))
  "Two runs are equal if the end points are the same."
  (let ((a-comp (fset:compare (point-a a) (point-a b))))
    (if (eq a-comp :equal)
	(fset:compare (point-b a) (point-b b))
	a-comp)))
;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SVG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod write-svg ((scene cl-svg:svg-toplevel) (run run))
  (with-slots (point-a point-b) run
    (unless (eq (fset:compare point-a point-b) :equal)
      (cl-svg:draw scene 
		   (:line
		    :x1 (* 10 (point-x point-a))
		    :y1 (* 15 (point-y point-a))
		    :x2 (* 10 (point-x point-b))
		    :y2 (* 15 (point-y point-b))
		    :stroke "red")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod text ((run run))
  "Returns the text content of RUN in IMAGE.
Basically all characters in the original image which are contained in the
interval specified by RUN."
  (coerce  (loop
	      :with image = (image run)
	      :with y = (point-y (point-a run))
	      :for x :from (point-x (point-a run)) :to (point-x (point-b run))
	      :collect (char-at-point image (make-point :x x :y y)))
	   'string))

#+nil (defmethod end-points ((run run) (image t))
  "The set of end points of RUN."
  (fset:with (fset:set (point-a run)) (point-b run)))

;;;;;;;;;;;;

(defun find-simple-x-runs (image classifier class)
  "Temporary abstraction, find in IMAGE horizontal runs.  
A consists of characters recognized by CLASSIFIER.  
The resulting run class is of class CLASS.

CLASSIFIER is a function taking two arguments, 
a location and the image.

TODO: Simplify to use increments and check wrapping."
  (let (result)
    (loop :for y :from 0 :below (image-height image)
       :for start-point = nil :do
       (loop :for x :from 0 :below (image-width image)
	  :for prev-point = point
	  :for point = (make-point :x x :y y)
	  :for has-right-class = (funcall classifier point image)
	  :do
	  (if has-right-class
	      (unless start-point
		(setf start-point point))
	      (when start-point
		(push (make-instance class
				     :a start-point
				     :b prev-point
				     :image image) result)
		(setf start-point nil)))))
    result))


(defun find-runs (image char-set direction)
  "Return all runs in IMAGE consisting of characters in CHAR-SET.
This will find all runs which consists purely of characters in CHAR-SET.
This will only find runs in either top-down or left-right direction.  
The DIRECTION parameter indicates this and it should either by :x or :y

- CHAR-SET is a list of character.
- DIRECTION is either :x, meaning horizontal runs (increasing x) or
  :y which finds vertical runs (increasing y).
"
  (labels ((start-position () (make-point :x 0 :y 0))

	   (char-at-position-matches-set (position)
	     (find (char-at-point image position) char-set)))


    (loop 
       :with result = nil
       :with dir = (make-point :x (if (eql direction :x) 1 0)
			       :y (if (eql direction :y) 1 0))
       :with position = (start-position)
       :while (not (past-end-of-image image position))
       :finally (return result)
       :do
       (if (char-at-position-matches-set position)
	   (push (make-instance 'run 
				:a position
				:b (loop 
				      :with prev-pos = position
				      :while (and (not (past-end-of-row/line image position))
						  (char-at-position-matches-set position))
				      :finally (return prev-pos) 
				      :do
				      (setf prev-pos position)
				      (setf position (add-points position dir )))
				:image image)
		 result)
	   (setf position (add-points position dir)))
       (when (past-end-of-row/line image position)
	 (setf position (adjust-point position (image-size image)))))))


(defun find-horizontal-and-vertical-runs (image)
  "Find the default runs in IMAGE.
The default runs are all vertical or horizontal runs which are made of
`*horizontal-run-chars*' or `*vertical-run-chars*' respectively.

By default these special variables contain characters used for 
vertical or horizontal lines and boxes."
  (concatenate 'list 
	       (find-runs image *horizontal-run-chars* :x)
	       (find-runs image *vertical-run-chars* :y)))


#+NIL (defun find-text (image)
  (find-runs image "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" :x))
