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


(defun write-svg-test (runs text-runs image &optional (file-name "D:/Weeks/02/test.svg"))
  (with-standard-io-syntax
    (cl-svg:with-svg-to-file
	(scene 'cl-svg:svg-1.1-toplevel :height "600" :width "600" :viewbox "0 0 600 600")
	(file-name :if-exists :supersede :if-does-not-exist :create)
      (loop :for run :in runs 
	 :for a = (point-a run)
	 :for b = (point-b run)
	 :do
	 (write-svg scene run))
      (loop :for run :in text-runs :do
	 (write-svg scene run)))))


(defun write-svg-test-2 (image elements &optional (file-name "/tmp/test-2.svg"))
  (let ((x-f 10) (y-f 15))
    (with-standard-io-syntax
      (cl-svg:with-svg-to-file
	  (scene 'cl-svg:svg-1.1-toplevel :width (* x-f (image-width image)) :height (* y-f (image-height image)))
	  (file-name :if-exists :supersede :if-does-not-exist :create)
	(loop :for element :in elements :do
	   (write-svg scene element))))))


(defun test-file (in-file &optional (out-file "D:/Weeks/02/test.svg"))
  (let ((ti (make-image-from-file in-file)))
    (classify-image ti)
    (write-svg-test (find-horizontal-and-vertical-runs ti)
		    (find-text ti)
		    ti out-file)))
