(in-package #:aa-svg)



(defparameter *horizontal-run-chars* "-+'/`\\<>")
(defparameter *vertical-run-chars* "|+'/`\\v^")
(defparameter *break-point-chars* "+'/`\\")
(defparameter *arrow-heads-end* "v>")
(defparameter *arrow-heads-start* "<^")
;; Every character is classifed with one or more characters:
;;
;;  t -- text
;;  | -- part of vertical element
;;  - -- part of horizontal element
;;  + -- hard 4 corner crossing/element --> implies - and |
;;  
;;  ` -- lower left corner element      --> implies - and |
;;  / -- upper left corner element     --> implies - and |
;;  , -- lower right corner element    --> implies - and |
;;  \ -- upper right corner element    --> implies - and |
;;
;;  all are equivalent to:
;;
;;
;;      6    3  7
;;       \  | /          
;;        \ |/
;;     2--  * -- 0
;;         /|\
;;        / | \ 
;;       5   1  4
;;
;;
;;                                                  |
;;  a combination of thes directions:  (0 1 3)  --> ?-
;;                                                  |
;;
;;
;;


(defparameter *up* (make-point :x 0 :y -1))
(defparameter *down* (make-point :x 0 :y 1))
(defparameter *left* (make-point :x -1 :y 0))
(defparameter *right* (make-point :x 1 :y 0))
(defparameter *up-right* (make-point :x 1 :y -1))
(defparameter *down-right* (make-point :x 1 :y 1))
(defparameter *down-left* (make-point :x -1 :y 1))
(defparameter *up-left* (make-point :x -1 :y -1))

(defparameter *here* (make-point :x 0 :y 0))

(defparameter *current-pos* nil)
(defparameter *current-image* nil)

(defun one-of (charset &optional (delta *here*))
  (find (char-at-point *current-image* (add-points *current-pos* delta)) charset))

(defun is-alphanumeric (&optional (delta *here*))
  #+nil (not (is-space delta))
  (alphanumericp (char-at-point *current-image* (add-points *current-pos* delta))))

(defun is-space (&optional (delta *here*))
  (eql #\Space (char-at-point *current-image* (add-points *current-pos* delta))))


(defmacro classifier (class &body form)
  `(lambda (image pos)
     (let ((*current-pos* pos)
	   (*current-image* image))
       (when ,@form ,class))))

;; defining classify rules:
;;
;;   -   (and (char-is-one-of *horizontal-run-chars*) (or (
;;
;;   - (and (one-of *horizontal-run-chars*) 
;;          (or (one-of *horizontal-run-chars* +left+)
;;              (one-of *horizontal-run-chars* +left+)))
;;
;;  ===>
;;
;;  '(#\- (lambda (image pos)
;;           (let ((*current-pos* pos))
;;             (and (one-of *horizontal-run-chars* ...

(defparameter *classifiers*
  (list (classifier #\t (or (is-alphanumeric)
			    (and (one-of " -")
				 (is-alphanumeric *right*)
				 (is-alphanumeric *left*))
			    (and (one-of "'")
				 (or
				  (is-alphanumeric *left*)
				  (is-alphanumeric *right*)))
			    (and (one-of "-")
				 (is-space *right*)
				 (is-space *left*))))
	(classifier #\- (and (one-of *horizontal-run-chars*)
			     (or (one-of *horizontal-run-chars* *left*)
				 (one-of *horizontal-run-chars* *right*))))
	(classifier #\| (and (one-of *vertical-run-chars*)
			     (or (one-of *vertical-run-chars* *up*)
				 (one-of *vertical-run-chars* *down*))))
	(classifier #\` (and (one-of *break-point-chars*)
			     (one-of *vertical-run-chars* *up*)
			     (one-of *horizontal-run-chars* *right*)
			     (not (one-of *horizontal-run-chars* *left*))
			     (not (one-of *vertical-run-chars* *down*))))
	(classifier #\\ (and (one-of *break-point-chars*)
			     (one-of *vertical-run-chars* *down*)
			     (one-of *horizontal-run-chars* *left*)
			     (not (one-of *horizontal-run-chars* *right*))
			     (not (one-of *vertical-run-chars* *up*))))
	(classifier #\/ (and (one-of *break-point-chars*)
			     (one-of *vertical-run-chars* *down*)
			     (one-of *horizontal-run-chars* *right*)
			     (not (one-of *horizontal-run-chars* *left*))
			     (not (one-of *vertical-run-chars* *up*))))
	(classifier #\, (and (one-of *break-point-chars*)
			     (one-of *vertical-run-chars* *up*)
			     (one-of *horizontal-run-chars* *left*)
			     (not (one-of *horizontal-run-chars* *right*))
			     (not (one-of *vertical-run-chars* *down*))))
	(classifier #\{ (and (one-of *break-point-chars*)
			     (one-of *vertical-run-chars* *up*)
			     (one-of *horizontal-run-chars* *left*)
			     (not (one-of *horizontal-run-chars* *right*))
			     (one-of *vertical-run-chars* *down*)))
	(classifier #\} (and (one-of *break-point-chars*)
			     (one-of *vertical-run-chars* *up*)
			     (not (one-of *horizontal-run-chars* *left*))
			     (one-of *horizontal-run-chars* *right*)
			     (one-of *vertical-run-chars* *down*)))
	(classifier #\^ (and (one-of *break-point-chars*)
			     (one-of *vertical-run-chars* *up*)
			     (one-of *horizontal-run-chars* *left*)
			     (one-of *horizontal-run-chars* *right*)
			     (not (one-of *vertical-run-chars* *down*))))
	(classifier #\v (and (one-of *break-point-chars*)
			     (not (one-of *vertical-run-chars* *up*))
			     (one-of *horizontal-run-chars* *left*)
			     (one-of *horizontal-run-chars* *right*)
			     (one-of *vertical-run-chars* *down*)))
	(classifier #\> (one-of *arrow-heads-end*))
	(classifier #\< (one-of *arrow-heads-start*))))

(defmethod classify ((image image) (point point))
  (loop 
     :with changed = nil
     :for classify :in *classifiers*
     :for class = (funcall classify image point)
     :finally (return changed)
     :do
     (when (and class (not (member class (classes-at-point image point))))
       (push class (classes-at-point image point))
       (setf changed t))))




(defun classify-image (image)
  "Update the classification data in IMAGE."
  (loop :for x :from 1 :below (- (image-width image) 1) :do
     (loop :for y :from 1 :below (- (image-height image) 1)
	:do
	(classify image (make-point :x x :y y)))))       
  
