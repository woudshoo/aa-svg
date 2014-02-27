(in-package #:aa-svg)

(defstruct point 
  "Point in ASCII coordinate space"
  x y)


(defparameter *up* (make-point :x 0 :y -1))
(defparameter *down* (make-point :x 0 :y 1))
(defparameter *left* (make-point :x -1 :y 0))
(defparameter *right* (make-point :x 1 :y 0))
(defparameter *up-right* (make-point :x 1 :y -1))
(defparameter *down-right* (make-point :x 1 :y 1))
(defparameter *down-left* (make-point :x -1 :y 1))
(defparameter *up-left* (make-point :x -1 :y -1))

(defparameter *here* (make-point :x 0 :y 0))
(defparameter *origin* *here*)

(defun any->= (a b)
  "Return true if a coordinate of A is less or equal than the corresponding coordinate of B."
  (or (>= (point-x a) (point-x b))
      (>= (point-y a) (point-y b))))

(defun add-points (a b)
  "Return the sum of points A and B."
  (make-point :x (+ (point-x a) (point-x b))
	      :y (+ (point-y a) (point-y b))))

(defun adjust-point (point bounds &optional (origin *here*))
  "Return POINT if inside BOUNDS, otherwise return adjusted POINT.
Adjusted means that the point is wrapped around.  This means that if
the x-coordinate is too large, the y-coordinate is increased and x is set to zero.
Similarly, if the y-coordinate is too large the x-coordinate is increased and y set to zero.

If both coordinates are too large, they are both increased."
  (if (any->= point bounds)
      (let ((x (point-x point))
	    (y (point-y point)))
	(when (>= x (point-x bounds))
	  (incf y)
	  (unless (>= y (point-y bounds))
	    (setf x (point-x origin))))
	(when (>= y (point-y bounds))
	  (incf x)
	  (unless (>= x (point-x bounds))
	    (setf y (point-y origin))))
	(make-point :x x :y y))
      point))

(defun do-rectangle (function origin end &optional (direction *right*))
  "Call FUNCTION for each point in rectangle indicated by ORIGIN and END.
The points iterated over are all points for which the x, and y coordinates
satisfy ORIGIN-X <= x < END-X and ORIGIN-Y <= y < END-Y.

The DIRECTION parameter can be either *DOWN* or *RIGHT*, and this indicates the primary
direction iterated over.  That is, if the DIRECTION is *RIGHT* it will first increase X,
if the DIRECTION is *DOWN* it will first increment Y."
  (do ((point origin (adjust-point (add-points point direction) end origin)))
      ((any->= point end))
    (funcall function point)))

(defun do-rectangle-inclusive (function origin end &optional (direction *right*))
  "Same as the `do-rectangle' function except that the end coordinate
is also included in the iteration.  That is all points (x,y) with
ORIGIN-X <= x <= END-X and ORIGIN-Y <= y <= END-Y
are iterated over"
  (do-rectangle function origin (add-points end (make-point :x 1 :y 1)) direction))

;;;;;;;;;;;; FSET integration ;;;;;;;;;;;;
(fset:define-cross-type-compare-methods point)

(defmethod fset:compare ((a point) (b point))
  (cond 
    ((< (point-x a) (point-x b)) :less)
    ((> (point-x a) (point-x b)) :greater)
    ((< (point-y a) (point-y b)) :less)
    ((> (point-y a) (point-y b)) :greater)
    (t :equal)))
;;;;;;;;;;;;


;;;;;;;;;;;; Generic inquiry functions

(defmethod end-points ((collection list) (image t))
  (let ((result (fset:empty-set)))
    (mapcar (lambda (r) (fset:unionf result (end-points r image)))
	    collection)
    result))
