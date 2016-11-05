;file: ex4.cl
;Copyright 1997-2005 Thomas R. Shultz, All rights reserved

(defun continuous-xor (start step)
  "(start step)
Make training or test patterns for continuous-xor."
  (do ((x start (float->decimals (+ x step) 2))
       (patterns nil))
      ((> x 1.0) (reverse patterns))
    (do ((y start (float->decimals (+ y step) 2)))
        ((> y 1.0))
      (let ((output (cond ((and (< x .55)
                                (< y .55))
                           -.5)
                          ((and (> x .55)
                                (< y .55))
                           .5)
                          ((and (< x .55)
                                (> y .55))
                           .5)
                          ((and (> x .55)
                                (> y .55))
                           -.5)))
            (inputs (list x y)))
        (setf patterns (cons (list inputs (list output)) patterns))))))

(defun run-continuous-xor ()
  "Run 1 continuous-xor net with training & test patterns.
Record training & test error every output epoch."
  (seed-random)
  (setq *test* t
      *test-interval* 1
      *record-train-errors* t
      *record-test-errors* t
      *mark-hiddens-errors* t
      *path* "/Users/Elias/315/A2/results/ex4")
  (set-patterns (continuous-xor .1 .1) 'train)
  (set-patterns (continuous-xor .14 .1) 'test)
  (train 100 100 25)
  (lists->file 
   *train-errors* 
   (concatenate 'string
     *path*
     "train errors"))
  (lists->file 
   *test-errors* 
   (concatenate 'string
     *path*
     "test errors"))
  (pprint *weights*)
  (pprint *output-weights*))