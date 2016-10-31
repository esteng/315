;file: ex6.cl
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

(defun run-continuous-xor-perceptual ()
  "Run 1 continuous-xor net with multiple test patterns.
Record error every 10th output epoch."
  (seed-random)
  (setq *test* t
      *test-interval* 10
      *record-train-errors* nil
      *record-test-errors* nil
      *mark-hiddens-errors* nil
      *sdcc* nil
      *path* "c:\\Documents and Settings\\tom\\My Documents\\models\\sdcc\\problems\\ex6\\"
      *record-multi-errors* t
      *multi-test-patterns* '((((.15 .95) (.5))
                               ((.95 .95) (-.5))
                               ((.15 .15) (-.5))
                               ((.95 .15) (.5)))
                              (((.45 .65) (.5))
                               ((.65 .65) (-.5))
                               ((.45 .45) (-.5))
                               ((.65 .45) (.5)))))
  (set-patterns (continuous-xor .1 .1) 'train)
  (train 100 100 25)
  (lists->file 
   *multi-errors* 
   (concatenate 'string
     *path*
     "multi-errors")))
