(in-package :aa-svg)


(defun make-pair (a b)
  "Given A and B return the pair containing A and B.
If A and B are equal return nil."
  (unless (eq :equal (fset:compare a b))
    (fset:set a b)))

(defun make-opposite-pair (a b)
  "Return a pair of points completing the rectange containing A and B.
If A and B are opposite points in a rectange, there are two implicit corners
of the rectangle.  Return the pair containing the implicit points.

So if A and B are points as below:

    A       X

    Y       B

Return the pair containg X and Y."
  (make-pair (make-point :x (point-x a) :y (point-y b))
	     (make-point :x (point-x b) :y (point-y a))))

(defun make-pairs (points)
  "Given a set of POINTS return the set of all pairs of points.
The pairs are unordered, so given an input set of size of n
the result contains n(n-1)/2 elements."
  (let ((result (fset:empty-set)))
    (fset:do-set (a points)
      (fset:do-set (b points)
	(when-let ((pair (make-pair a b)))
	  (fset:adjoinf result pair))))
    result))
    
(defun possible-rectangles (points)
  "Returns a list of possible rectangles, each rectangle indicated by the four points of its corners.
Note that this can result in lots of rectangles, O (n^2) with n the number of points."
  (let ((pairs (make-pairs points))
	(result (fset:empty-set)))
    (fset:do-set (pair pairs)
      (let ((opposite-pair (apply #'make-opposite-pair (fset:convert 'list pair))))
	(when (and (not (eq :equal (fset:compare pair opposite-pair)))
		   (fset:contains? pairs opposite-pair))
	  (fset:adjoinf result (fset:union pair opposite-pair)))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Idea for more meaningful rectangles
;;;
;;; Make a partial order on the rectangles in one of the following ways:
;;;
;;;  1 - <=C   R <=C S if R is contained in S.
;;;  2 - <=D   R <=D S if R is contained in S and all the corners of R are on the edges of S.
;;;
;;; Now a possible algorithm for finding interesting rectangles is:
;;;   - Find all rectangles
;;;   - Only keep the minimal and maximal rectangles in a partial order.
;;;
;;; If we use <=D this is probably what we want.


(defun possible-rectangles (runs)
  "Return a list of possible rectangles indicated by the RUNS.
A rectangle R is possible in RUNS if there are 4 runs such that
4 subsets of the 4 different runs are the sides of R.")



