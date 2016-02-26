
;; update-cell :: [[Bit]] -> Num -> Num -> Num -> Num -> Bit
(defun update-cell (field x y)
  (destructuring-bind (width height) (array-dimensions field)
  (let* ((live? (eq (aref field x y) 1))
         (l (mod (1- x) width))
         (r (mod (1+ x) width))
         (u (mod (1- y) height))
         (d (mod (1+ y) height))
         (near (list (aref field l u)
                     (aref field x u)
               (aref field r u)
                     (aref field l y)
               (aref field r y)
               (aref field l d)
               (aref field x d)
               (aref field r d)))
         (lifes (reduce #'+ near)))
    (cond (live? (case lifes
                       ((2 3)     1)  ;; living
                       (otherwise 0)));; die
          (T     (case lifes
                       (3         1);; born
                       (otherwise 0)))))));;dying

(defun update-field (field1 field2)
  (destructuring-bind (width height) (array-dimensions field1)
    (loop for i from 0 to (1- width) do
      (loop for j from 0 to (1- height) do
        (setf (aref field2 i j) (update-cell field1 i j))))))


(defun view-field (field)
  (destructuring-bind (width height) (array-dimensions field)
    (loop for i from 0 to (1- width) do
      (loop for j from 0 to (1- height) do
        (prin1 (aref field i j)))
      (format t "~%"))))

(defconstant +width+ 30)
(defconstant +height+ 30)

(defparameter *field-1* (make-array (list +width+ +height+)
  :element-type 'bit
  :initial-element 0))
(defparameter *field-2* (make-array (list +width+ +height+)
  :element-type 'bit
  :initial-element 0))

;; initialize
(setf (aref *field-1* 1 1) 1) (setf (aref *field-1* 1 2) 1) (setf (aref *field-1* 1 3) 1)
                                                            (setf (aref *field-1* 2 3) 1)
                              (setf (aref *field-1* 3 2) 1)
(let ((lst (list *field-1* *field-2*)))
  (loop for i to 100 do
    (view-field (nth 0 lst))(format t "~%")
    (update-field (nth 0 lst) (nth 1 lst))
    (rotatef (nth 0 lst) (nth 1 lst))))
