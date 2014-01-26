(in-package :aa-svg)

(defclass line (run)
  nil)


(defun find-horizontal-lines (image)
  (find-simple-runs image *right*
		      (lambda (point image) (has-class-at-point image point #\-))
		      'line))

(defun find-vertical-lines (image)
  (find-simple-runs image *down*
		    (lambda (point image) (has-class-at-point image point #\|))
		    'line))


(defmethod marker-end ((line line))
  (has-class-at-point (image line) (point-b line) #\>))

(defmethod marker-start ((line line))
  (has-class-at-point (image line) (point-a line) #\<))

(defmethod write-svg ((scene cl-svg:svg-toplevel) (element line))
  (with-slots (point-a point-b) element
    (unless (eq :equal (fset:compare point-a point-b))
      (cl-svg:draw scene (:line :x1 (* 10 (point-x point-a))
				:y1 (* 15 (point-y point-a))
				:x2 (* 10 (point-x point-b))
				:y2 (* 15 (point-y point-b))
				:stroke "green"
				:marker-start (if (marker-start element) "url(#TRIANGLE-IN)" "") 
				:marker-end  (if (marker-end element) "url(#TRIANGLE-OUT)" ""))))))

;;;
;;; Arrow heads:
;;;
;;;  Defining the arrow head is in the defs section:
;;;  
;;; <defs>
;;;  <marker id="Triangle"
;;;	  viewBox="0 0 10 10" refX="0" refY="5" markerUnits="strokeWidth" markerWidth="8" markerHeight="10" orient="auto">
;;;    <path d="M 0 0 L 10 5 L 0 10 z" />
;;; </marker> </defs>
;;;
;;; Using the arrow head is done by the attribute marker-... e.g.:
;;; 
;;;   <line x1="20" y1="15" x2="20" y2="90" stroke="green" marker-end="url(#Triangle)"/>


