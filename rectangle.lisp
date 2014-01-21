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
  "Returns a list of possible rectangles, each rectangle indicated by the four points of its corners"
  (let ((pairs (make-pairs points))
	(result (fset:empty-set)))
    (fset:do-set (pair pairs)
      (let ((opposite-pair (apply #'make-opposite-pair (fset:convert 'list pair))))
	(when (and (not (eq :equal (fset:compare pair opposite-pair)))
		   (fset:contains? pairs opposite-pair))
	  (fset:adjoinf result (fset:union pair opposite-pair)))))
    result))




