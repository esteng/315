;file: ex3.cl
;Copyright 1997-2005 Thomas R. Shultz, All rights reserved

(defun run-n-xor (n)
  "(n)
Run n xor nets, recording training errors."
  (setq *test* t
      *test-interval* 1
      *record-train-errors* t
      *mark-hiddens-errors* t)
  (setq *xor-patterns* '(((0.0 0.0) (-.5))
                         ((0.0 1.0) (.5))
                         ((1.0 0.0) (.5))
                         ((1.0 1.0) (-.5))))
  (do ((i 0 (+ i 1)))
      ((= i n) 'done)
    (seed-random)
    (terpri)
    (set-patterns *xor-patterns* 'train)
    (train 100 100 25)
    (lists->file 
     *train-errors* 
     (concatenate 
         'string
       "c:\\Documents and Settings\\tom\\My Documents\\models\\sdcc\\problems\\examples\\train-errors"
       (princ-to-string i)))))
