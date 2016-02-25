(defconstant +width+ 3)
(defconstant +height+ 3)

(defparameter *field-1* (make-array (list +width+ +height+)
  :element-type 'bit
  :initial-element 0))
(defparameter *field-2* (make-array (list +width+ +height+)
  :element-type 'bit
  :initial-element 0))

;;(print (aref *field* 0 0))


;; update-cell :: [[Bit]] -> Num -> Num -> Num -> Num -> Bit
(defun update-cell (field width height x y)
  (let* ((live? (> (aref field x y) 0))
	 (near (list (aref field (- x 1) (- y 1))
	             (aref field x       (- y 1))
		     (aref field (+ x 1) (- y 1))
	             (aref field (- x 1) y)
		     (aref field x       y)
		     (aref field (+ x 1) y)
		     (aref field (- x 1) (+ y 1))
		     (aref field x       (+ y 1))
		     (aref field (+ x 1) (+ y 1))))
	 (lifes (reduce #'+ near)))
    (cond (live? (case lifes
		   ((0 1)     0)  ;; die
		   ((2 3)     1)  ;; living
		   (otherwise 0)));; die
	  (T     (case lifes
		   (3 1);; born
		   (otherwise 0))))));;dying

;; initialize	 
(setf (aref *field-1* 0 0) 1)   
(setf (aref *field-1* 0 1) 1)
(setf (aref *field-1* 2 2) 1)
;; test
(print *field-1*)
(print "before:")
(print (aref *field-1* 1 1))
(print "after: ")
(print (update-cell *field-1* +width+ +height+ 1 1))
