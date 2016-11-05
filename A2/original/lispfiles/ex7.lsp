;file: ex7.cl
;Copyright 1997-2005 Thomas R. Shultz, All rights reserved

(defun knowledge-continuous-xor ()
  "()
Run 1 continuous-xor net for a final contribution analysis."
  (seed-random)
  (setq *test* nil
      *record-train-errors* nil
      *record-test-errors* nil
      *mark-hiddens-errors* nil
      *record-multi-errors* nil
      *record-train-contributions* nil
      *sdcc* nil)
  (set-patterns (continuous-xor .1 .1) 'train)
  (train 100 100 25)
  (setq *record-train-contributions* t)
  (test-epoch)
  (save-contributions 
   *train-contributions* 
   "/Users/Elias/315/A2/results/ex7/ex7"
   'train 
   1))
