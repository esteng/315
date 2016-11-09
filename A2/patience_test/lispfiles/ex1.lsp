;file: ex1.cl
;Copyright 1997-2005 Thomas R. Shultz, All rights reserved

(defvar *xor-patterns* nil)

(defun run-xor ()
  (setq *xor-patterns* '(((0.0 0.0) (-.5))
                         ((0.0 1.0) (.5))
                         ((1.0 0.0) (.5))
                         ((1.0 1.0) (-.5))))
  (set-patterns *xor-patterns* 'train)
  (train 100 100 25)
  (pprint *weights*)
  (pprint *output-weights*))
