;;;; package.lisp

(defpackage #:aa-svg
  (:use #:cl #:alexandria)
  (:export
   :make-image-from-file
   :print-image
   :print-classification
   :classify-image
   :test-file))

