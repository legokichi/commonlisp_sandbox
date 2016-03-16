;;;; lifegame.lisp

;;;; このプログラムは http://qiita.com/DUxCA/items/6df2019f502db35597ca のライフゲームプログラムを改変したものです．

;; 画面サイズ
(defconstant +width+ 30)
(defconstant +height+ 30)

(defstruct (field (:print-object view-field))
  (cells (make-array (list +width+ +height+)
                     :element-type 'bit
                     :initial-element 0))
  (width +width+)
  (height +height+))

(defmacro fref (field x y)
  "位置(x, y)のcellにアクセスする．"
  `(aref (field-cells ,field) ,x ,y))

(defconstant alive 1 "生")
(defconstant dead 0 "死")

(defun alivep (cell)
  "cellが生きているか．"
  (= cell alive))

(defmacro fref-list (field &rest xylist)
  `(list ,@(loop for p in xylist
              collect `(fref ,field ,@p))))

(defun update-cell (field x y)
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

(defmacro for-all-cells (field (x y) inner &optional outer)
  "すべてのcellについて処理する．
(xloop
 (yloop inner)
 outer)
のように展開される．"
  `(loop for ,x below (field-width ,field)
      do (loop for ,y below (field-height ,field)
            do ,inner)
        ,@(when outer `(,outer))))

(defun update-field (field1 field2)
  (for-all-cells field1 (i j)
          (setf (fref field2 i j)
                (update-cell field1 i j)))) ;; バッファ1からバッファ2へ写す

(defun view-field (field stream)
  (for-all-cells field (i j)
          (format stream "~a"
                  (if (alivep (fref field i j)) "#" "."))
          (terpri stream)))

;; 画面バッファ 二次元ビット配列
(defparameter *field-1* (make-field))
(defparameter *field-2* (make-field))

(defun test-run ()
  ;; 初期状態としてグライダーを配置
  (setf *field-1* (make-field))

  (setf (fref *field-1* 1 1) alive
        (fref *field-1* 1 2) alive
        (fref *field-1* 1 3) alive

        (fref *field-1* 2 3) alive
        (fref *field-1* 3 2) alive)

  ;; 100ステップ実行
  (let ((current-field *field-1*)
        (next-field *field-2*))
    (loop repeat 100 do
         (print current-field)
         (update-field current-field next-field)
         (rotatef current-field next-field)
         (sleep 0.1))))

(test-run)
