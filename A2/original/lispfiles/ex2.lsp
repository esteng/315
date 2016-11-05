;file: ex2.cl
;Copyright 1997-2005 Thomas R. Shultz, All rights reserved

(defun run-xor ()
   "()
Run 1 xor net, look at the final weights, & record training errors."
   (setq *test* t
     *test-interval* 1
     *record-train-errors* t
     *mark-hiddens-errors* t)
   (setq *xor-patterns* '(((0.0 0.0) (-.5))
                          ((0.0 1.0) (.5))
                          ((1.0 0.0) (.5))
                          ((1.0 1.0) (-.5))))
   (set-patterns *xor-patterns* 'train)
   (train 100 100 25)
   (pprint *weights*)
   (pprint *output-weights*)
  (lists->file 
   *train-errors* 
   "/Users/Elias/315/A2/results/ex2/ex2"))
