;; count 1 to 10
(loop for i to 10 do
  (sleep 1)
  (format *standard-output* "~A ~C" i #\return))
