(defconstant +width+ 30)
(defconstant +height+ 30)

(defstruct (field (:print-object view-field))
  (cells (make-array (list +width+ +height+)
                     :element-type 'bit
                     :initial-element 0))
  (width +width+)
  (height +height+))

(defconstant alive 1 "生")
(defconstant dead 0 "死")

(defmacro macro-make-field () (make-field))

(defmacro fref (field x y)
  "位置(x, y)のcellにアクセスする．"
  (aref (field-cells field) x y))


(defmacro fref-list (field &rest xylist)
  (apply #'list (mapcar #'(lambda (p) (apply #'fref (cons field p))) xylist)))

(defmacro alivep (cell)
  "cellが生きているか．"
  (= cell alive))

(defmacro update-cell (field x y)
  (let* ((width (field-width field))
         (height (field-height field))
         (l (mod (1- x) width))  (r (mod (1+ x) width))
         (u (mod (1- y) height)) (d (mod (1+ y) height))
         (near (fref-list field
                          (l u) (x u) (r u)
                          (l y)       (r y)
                          (l d) (x d) (r d)))
         (lifes (count alive near)))
    (if (alivep (fref field x y))
        (case lifes
          ((2 3)     alive)   ;; living
          (otherwise dead))   ;; die
        (case lifes
          (3         alive)   ;; born
          (otherwise dead)))));; dying

(print (field-cells (update-cell (macro-make-field) 0 0)))
