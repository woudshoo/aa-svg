;;;; aa-svg.lisp

(in-package #:aa-svg)

;;; "aa-svg" goes here. Hacks and glory await!

#|
   The idea is to translate ascii art in svg.
   Let do it in two steps:
 
   1 - find lines
   2 - combine lines to bigger things

|#
;;;;;;;;;;;; image functions ;;;

;;;;;;;;;;;; points ;;;;;;;;;;;;
;;;;;;;;;;;; Runs ;;;;;;;;;;;;


;;;;;;;;;;;; analyzing ;;;;;;;;;;;;



;;;;;;;;;;;; Ideas ;;;;;;;;;;;;
;;
;;
;;  little problem, given a collection of points, find all rectangles.
;;
;;  Algorithm 1:
;;
;;    A - Loop over all pairs such that (x1, y1), (x2,y2) have: x1 != x2 and y1 != y2
;;    B - see if the points (x1,y2) (x2,y1) exist.
;;
;;  So basically, we have a 


(defun write-svg-defs (scene runs)
  "Write all defs in SCENE which are needed by the RUNS.
This needs to be improved to collect all distinct defs that are needed.
But for now just hardcode them."
  (declare (ignore runs))
  (cl-svg:make-marker scene (:id :triangle-out  
				 :ref-x 0 :ref-y 5 
				 :marker-units "strokeWidth" :orient "auto"
				 :marker-width 10
				 :marker-height 10)
    (cl-svg:draw* (:path :d "M 0 0 L 10 5 L 0 10 z")))
  (cl-svg:make-marker scene (:id :triangle-in
				 :ref-x 0 :ref-y 5 
				 :marker-units "strokeWidth" :orient "auto"
				 :marker-width 10
				 :marker-height 10)
    (cl-svg:draw* (:path :d "M 0 5 L 10 0 L 10 10 z"))))



(defun write-svg-test-2 (image elements &optional (file-name "/tmp/test-2.svg"))
  (let ((x-f 10) (y-f 15))
    (with-standard-io-syntax
      (cl-svg:with-svg-to-file
	  (scene 'cl-svg:svg-1.1-toplevel :width (* x-f (image-width image)) :height (* y-f (image-height image)))
	  (file-name :if-exists :supersede :if-does-not-exist :create)
	(loop :for element :in elements :do
	   (write-svg scene element))
	(write-svg-defs scene elements)))))

(defun test-file (in-file &optional (out-file "D:/Weeks/02/test.svg"))
  (let ((ti (make-image-from-file in-file)))
    (classify-image ti)
    (write-svg-test-2 ti (collect-elements ti) out-file)))

