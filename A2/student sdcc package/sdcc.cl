;file: sdcc
;purpose: additional interface procedures to load after cc. Replaces ccs to implement sdcc option. 
;   *max-units* boosted to *ninputs* + 20 in build-net for conservation
;programmer: Thomas R. Shultz
;sdcc parts programmed by Francois Rivest & Thomas R. Shultz
;started: 18 apr 90
;current: 11 apr 04

;;; ccs is a user shell for Fahlman's cc. The ccs shell, written 
;;; in Common Lisp, facilitates the setting up of runs and the 
;;; recording and analyzing of data. ccs, which includes 
;;; modifications of much of Fahlman's original public domain code, 
;;; is distributed under license by the author, Thomas R. Shultz. 

;;; Copyright 1997-2004 Thomas R. Shultz, All rights reserved

;;; Professor Thomas R. Shultz
;;; Department of Psychology
;;; McGill University
;;; 1205 Penfield Avenue
;;; Montreal, Quebec H3A 1B1
;;; Canada
;;; 
;;; Phone: 514-398-6139
;;; Internet: shultz@psych.mcgill.ca

;;;;;;;;;;;;additional variables for interface

(defvar *encoder* nil)

(defvar *tolerance* 0)

(defvar *proportion-threshold* nil)

(defvar *linear-outputs* nil)

;;;;;SDCC

;;;For sibling-descendant cascade-correlation, 
;;;   Keep track of how many hiddens are in each layer as units are recruited.
;;;   Split the pool of candidate units, and 0 the connections from the most-recently-added layer of hidden units 
;;;   for the first half of them, the siblings. Keep setting them to 0 as candidate weights are adjusted. 

(defvar *sdcc* nil)

(defvar *sibling-factor* 1.0)

(defvar *descendant-factor* 0.8)

(defvar *structure* '())

;;;;;display error results in listener

(defvar *display-error* nil)

;;;;;record unit activations

(defvar *record-train-activations* nil)

(defvar *train-activations* nil)

(defvar *record-test-activations* nil)

(defvar *test-activations* nil
)

(defvar *record-multi-test-activations* nil)

(defvar *multi-test-activations* nil)

;;;;;multiple test patterns (beyond test patterns)

(defvar *multi-test-patterns* nil)

;;;;;recording true error from training, test, & multiple test patterns

(defvar *record-train-errors* nil
  "If t, record errors from training patterns.")

(defvar *train-errors* nil
)

(defvar *record-test-errors* nil
  "If t, record errors from test patterns.")

(defvar *test-errors* nil)

(defvar *record-multi-errors* nil
  "If t, record errors from multiple test-patterns.")

(defvar *multi-errors* nil)

(defvar *mark-hiddens-errors* nil
  "If t, mark hidden unit installations in error files.")

;;;;;recording proportion of pattern correct every test epoch

(defvar *record-proportion-train-correct* nil
  "If t, record proportion of training patterns correct.")

(defvar *proportion-train-correct* nil)

(defvar *record-proportion-test-correct* nil
  "If t, record proportion of test patterns correct.")

(defvar *proportion-test-correct* nil)

(defvar *record-proportion-multi-correct* nil
  "If t, record proportions of multiple test patterns correct.")

(defvar *proportion-multi-correct* nil)

;;;;;recording net info

(defvar *save-nets-for-test-setups* nil
  "If t, record net info every test-epoch for later use by test-setup.")

(defvar *nets-info* nil
  "Lists of net info, in reverse order by epoch.")

;;;;;recording contributions

(defvar *test-last-output-phase-epoch* nil
  "If t, run test-epoch after last epoch of output phase.")

(defvar *record-train-contributions* nil
)

(defvar *scale-contributions-by-target* nil)

(defvar *train-contributions* nil)

(defvar *record-test-contributions* nil)

(defvar *test-contributions* nil
 )

;;;;;hinton values

(defvar *record-hintons* nil
  "If t, record hinton values every test-epoch.")

(defvar *hintons* nil
  "Lists of hinton values, in reverse order by epoch.")

;;;;;noise amounts

(defvar *uniform-input-noise* nil
  "Range of noise selected randomly from uniform distribution to be added or subtracted from inputs.")

(defvar *some-uniform-input-noise* nil
  "List of range of uniform noise followed by list of position numbers to be noised.")

(defvar *uniform-output-noise* nil
)

(defvar *normal-input-noise* nil
  "Sd of noise selected randomly from normal distribution to be added to inputs.")

(defvar *some-normal-input-noise* nil
  "List of sds of normal noise followed by list of position numbers to be noised.")

(defvar *normal-output-noise* nil
  "Sd of noise selected randomly from normal distribution to be added to targets.")

;;;;;standard patterns to be used if noise is to be added

(defvar *standard-training-inputs* nil
  "Vector of training inputs. Set in set-patterns if noise is added.")

(defvar *standard-training-outputs* nil
  "Vector of training outputs. Set in set-patterns if noise is added.")

(defvar *standard-test-inputs* nil
  "Vector of test inputs. Set in set-patterns if noise is added.")

(defvar *standard-test-outputs* nil
  "Vector of test outputs. Set in set-patterns if noise is added.")

;;;;;globals for random inputs

(defvar *random-inputs* nil
  "If t, add random inputs at each epoch.")

(defvar *high-random-input* nil
  "Highest random input. Set in random-inputs.")

(defvar *n-random-inputs* nil
  "Number of random inputs. Set in random-inputs.")

;;;;;select fresh training, test, or multiple test patterns each epoch

(defvar *select-patterns* nil
)

(defvar *select-multi-patterns* nil
)

;;;;;modified procedures

(defun init-net ()

  ;; Set up the *ALL-CONNECTIONS* vector.
  (setq *all-connections*
        (make-array *max-units* :element-type 'fixnum))
  (dotimes1 (i *max-units*)
            (setf (ivref *all-connections* i) i))
  ;; Initialize the active unit data structures.
  (dotimes1 (i *max-units*)
            (setf (fvref *extra-values* i) 0.0)
            (setf (ivref *nconnections* i) 0)
            (setf (svref *connections* i) nil)
            (setf (svref *weights* i) nil)
            (setf (svref *output-weights-record* i) nil))
  ;; Initialize the per-output data structures.
  (dotimes1 (i *noutputs*)
            (setf (fvref *outputs* i) 0.0)
            (setf (fvref *extra-errors* i) 0.0)
            (setf (fvref *sum-errors* i) 0.0)
            (setf (fvref *dummy-sum-errors* i) 0.0)
            (let ((ow (svref *output-weights* i))
                  (od (svref *output-deltas* i))
                  (os (svref *output-slopes* i))
                  (op (svref *output-prev-slopes* i)))
              (dotimes1 (j *max-units*)
                        (setf (fvref ow j) 0.0)
                        (setf (fvref od j) 0.0)
                        (setf (fvref os j) 0.0)
                        (setf (fvref op j) 0.0))
              ;; Set up initial random weights for the input-to-output connections.
              (dotimes1 (j (1+ *ninputs*))
                        (setf (fvref ow j) (random-weight)))))
  ;; Initialize the caches if they are in use.
  (when *use-cache*
    (dotimes1 (j *max-cases*)
              (let ((v (svref *values-cache* j))
                    (e (svref *errors-cache* j)))
                (dotimes1 (i *max-units*)
                          (setf (fvref v i) 0.0))
                (dotimes1 (i *noutputs*)
                          (setf (fvref e i) 0.0)))))	
  ;; Candidate units get initialized in a separate routine.
  (setq *structure* '())                                                              ;;sdcc
  (init-candidates)
  ;; Do some other assorted housekeeping.
  (setf (fvref *extra-values* 0) 1.0)
  (setq *epoch* 0)
  (setq *nunits* (+ 1 *ninputs*))
  (setq *error-bits* 0)
  (setq *true-error* 0.0)
  (setq *sum-sq-error* 0.0)
  (setq *best-candidate-score* 0.0)
  (setq *best-candidate* 0))

(defun build-net (ninputs noutputs)

  (declare (fixnum ninputs noutputs))
  ;; Fill in assorted variables and create top-level vectors.
  (setq *ninputs* ninputs
      *max-units* (+ ninputs 20)
      *noutputs* noutputs
      *max-cases* (length *training-inputs*)
      *ncases* *max-cases*
      *first-case* 0
      *nunits* (+ 1 *ninputs*)
      *values-cache* (make-array *max-cases* :initial-element nil)
      *extra-values* (make-array *max-units*
                                 :element-type 'short-float
                                 :initial-element 0.0)
      *values* *extra-values*
      *nconnections* (make-array *max-units*
                                 :element-type 'fixnum
                                 :initial-element 0)
      *connections* (make-array *max-units* :initial-element nil)
      *weights* (make-array *max-units* :initial-element nil)
      *outputs* (make-array *noutputs*
                            :element-type 'short-float
                            :initial-element 0.0)
      *errors-cache* (make-array *max-cases* :initial-element nil)
      *extra-errors* 	(make-array *noutputs*
                                  :element-type 'short-float
                                  :initial-element 0.0)
      *errors* *extra-errors*
      *sum-errors* (make-array *noutputs*
                               :element-type 'short-float
                               :initial-element 0.0)
      *dummy-sum-errors* (make-array *noutputs*
                                     :element-type 'short-float
                                     :initial-element 0.0)
      *output-weights* (make-array *noutputs* :initial-element nil)
      *output-weights-record* (make-array *max-units* :initial-element nil)
      *output-deltas* (make-array *noutputs* :initial-element nil)
      *output-slopes* (make-array *noutputs* :initial-element nil)
      *output-prev-slopes* (make-array *noutputs* :initial-element nil)
      *cand-sum-values* (make-array *ncandidates*
                                    :element-type 'short-float
                                    :initial-element 0.0)
      *cand-cor* (make-array *ncandidates* :initial-element nil)
      *cand-prev-cor* (make-array *ncandidates* :initial-element nil)
      *cand-weights* (make-array *ncandidates* :initial-element nil)
      *cand-deltas* (make-array *ncandidates* :initial-element nil)
      *cand-slopes* (make-array *ncandidates* :initial-element nil)
      *cand-prev-slopes* (make-array *ncandidates* :initial-element nil))
  ;; Only create the caches if *USE-CACHE* is on -- may not always have room.
  (when *use-cache*
    (dotimes1 (i *max-cases*)
              (setf (svref *values-cache* i)
                (make-array *max-units*
                            :element-type 'short-float
                            :initial-element 0.0))
              (setf (svref *errors-cache* i)
                (make-array *noutputs*
                            :element-type 'short-float
                            :initial-element 0.0))))
  ;; For each output, create the vectors holding per-weight information.
  (dotimes1 (i *noutputs*)
            (setf (svref *output-weights* i)
              (make-array *max-units*
                          :element-type 'short-float
                          :initial-element 0.0))
            (setf (svref *output-deltas* i)    
              (make-array *max-units*
                          :element-type 'short-float
                          :initial-element 0.0))
            (setf (svref *output-slopes* i)
              (make-array *max-units*
                          :element-type 'short-float
                          :initial-element 0.0))
            (setf (svref *output-prev-slopes* i)
              (make-array *max-units*
                          :element-type 'short-float
                          :initial-element 0.0)))
  ;; For each candidate unit, create the vectors holding the correlations,
  ;; incoming weights, and other stats.
  (dotimes1 (i *ncandidates*)
            (setf (svref *cand-cor* i)
              (make-array *noutputs*
                          :element-type 'short-float
                          :initial-element 0.0))
            (setf (svref *cand-prev-cor* i)
              (make-array *noutputs*
                          :element-type 'short-float
                          :initial-element 0.0))
            (setf (svref *cand-weights* i)
              (make-array *max-units*
                          :element-type 'short-float
                          :initial-element 0.0))
            (setf (svref *cand-deltas* i)
              (make-array *max-units*
                          :element-type 'short-float
                          :initial-element 0.0))
            (setf (svref *cand-slopes* i)
              (make-array *max-units*
                          :element-type 'short-float
                          :initial-element 0.0))
            (setf (svref *cand-prev-slopes* i)
              (make-array *max-units*
                          :element-type 'short-float
                          :initial-element 0.0))))

(defun output-forward-pass ()

  (dotimes1 (j *noutputs*)
            (let ((ow (svref *output-weights* j))
                  (sum 0.0))
              (declare (short-float sum))
              (dotimes1 (i *nunits*)
                        (incf-sf sum (*sf (fvref *values* i) (fvref ow i))))
              (setf (fvref *outputs* j)
                    (if (member j *linear-outputs*)
                      sum
                      (output-function sum))))))

(defmacro compute-errors (goal err-bits true-err sum-sq-err slopes-p)

  `(dotimes1 (j *noutputs*)
             (let* ((out (fvref *outputs* j))
                    (target (svref ,goal j))
                    (dif (-sf out target))
                    (err-prime (if *raw-error*
                                   dif
                                 (*sf dif (if (member j *linear-outputs*)
                                              1.0
                                            (output-prime out)))))
                    ,@(when slopes-p
                        '((os (svref *output-slopes* j)))))
               (declare (short-float dif err-prime))
               
               ;If using *proportion-threshold*, then score-threshold = *proportion-threshold* x target.
               ;Except, when target = 0, score-threshold = 1, to avoid trying to reach target of exact 0.
               (let ((score-threshold 
                      (if *proportion-threshold*
                          (if (zerop target)
                              1
                            (*sf *proportion-threshold* target))
                        *score-threshold*)))
                 (unless (< (abs dif) score-threshold)
                   (incf ,err-bits)))
               
               (incf-sf ,true-err (*sf dif dif))
               (setf (fvref *errors* j) err-prime)      
               (incf-sf (fvref *sum-errors* j) err-prime)
               (incf-sf ,sum-sq-err (*sf err-prime err-prime))
               ,@(when slopes-p
                   '((dotimes1 (i *nunits*)
                               (incf-sf (fvref os i) 
                                        (*sf err-prime (fvref *values* i)))))))))

(defmacro recompute-errors (goal)
  
  `(dotimes1 (j *noutputs*)
             (let* ((out (fvref *outputs* j))
                    (dif (-sf out (svref ,goal j)))
                    (err-prime (if *raw-error*
                                   dif
                                 (*sf dif (if (member j *linear-outputs*)
                                              1.0
                                            (output-prime out))))))
               (declare (short-float dif err-prime))
               (setf (fvref *errors* j) err-prime))))

(defun train-outputs-epoch ()

  (let ((err-bits 0)
        (true-err 0.0)
        (sum-sq-err 0.0))
    (declare (fixnum err-bits) (short-float true-err sum-sq-err))
    (dotimes1 (o *noutputs*)
              (setf (fvref *sum-errors* o) 0.0))
    ;; User may have changed mu between epochs, so fix shrink-factor.
    (setq *output-shrink-factor*
          (/sf *output-mu* (+sf 1.0 *output-mu*)))
    ;; Now run through the training examples.
    ;(system:serve-all-events 0)
    (do ((i *first-case* (1+ i)))
        ((= i (the fixnum (+ *first-case* *ncases*))))
      (declare (fixnum i))
      (cond (*use-cache*
             (setq *values* (svref *values-cache* i))
             (setq *errors* (svref *errors-cache* i))
             (output-forward-pass))
            (t (setq *values* *extra-values*)
               (setq *errors* *extra-errors*)
               (full-forward-pass (svref *training-inputs* i))))
      (compute-errors (svref *training-outputs* i)
                      err-bits true-err sum-sq-err t))
    (setq *error-bits* err-bits)
    ;; Stop if error can be tolerated.
    (if (< (/ *error-bits*
              (* *noutputs*
                 (length *training-outputs*)))
           *tolerance*)
        (setq *error-bits* 0))
    (setq *true-error* (+sf 0.0 true-err))
    (setq *sum-sq-error* (+sf 0.0 sum-sq-err))
    ;; Do not change weights or count epoch if this run was perfect.
    (unless (= 0 *error-bits*)
      (update-output-weights)
      (if *encoder* (zero-input-output-weights))
      (incf *epoch*))))

(defun train-outputs (max-epochs last-epoch)
  
  (declare (fixnum max-epochs))
  (let ((last-error 0.0)
        (quit-epoch (+ *epoch* *output-patience*))
        (first-time t))
    (declare (fixnum quit-epoch)
             (short-float last-error))
    (dotimes1 (i max-epochs (progn
                              (record-output-weights)
                              :timeout))
              (modify-patterns)
              (if *select-multi-patterns* (select-multi-patterns))
              ;; Maybe run a test epoch to see how we're doing.
              (when (and *test*
                         (not (= 0 *test-interval*))
                         (= 0 (mod i *test-interval*)))
                (progn
                  (test-epoch *score-threshold*)
                  (if *multi-test-patterns* (tests-epoch *score-threshold*))))
              (train-outputs-epoch)
              (cond ((> *epoch* last-epoch)
                     (return :stop))
                    ((zerop *error-bits*)
                     (record-output-weights)
                     (return :win))
                    ((zerop *output-patience*))
                    (first-time
                     (setq first-time nil)
                     (setq last-error *true-error*))
                    ((> (abs (- *true-error* last-error))
                        (* last-error *output-change-threshold*))
                     (setq last-error *true-error*)
                     (setq quit-epoch (+ *epoch* *output-patience*)))
                    ((>= *epoch* quit-epoch)
                     (record-output-weights)
                     (return :stagnant))))))

(defun init-candidates ()
 
  (dotimes1 (i *ncandidates*)
            (setf (fvref *cand-sum-values* i) 0.0)
            (let ((cw (svref *cand-weights* i))
                  (cd (svref *cand-deltas* i))
                  (cs (svref *cand-slopes* i))
                  (cp (svref *cand-prev-slopes* i))
                  (cc (svref *cand-cor* i))
                  (cpc (svref *cand-prev-cor* i)))
              (dotimes1 (j *nunits*)
                        (setf (fvref cw j) (random-weight))
                        (setf (fvref cd j) 0.0)
                        (setf (fvref cs j) 0.0)
                        (setf (fvref cp j) 0.0))
              (dotimes1 (o *noutputs*)
                        (setf (fvref cc o) 0.0)
                        (setf (fvref cpc o) 0.0))))
  (if (and *sdcc* *structure*)                             ;;sdcc
      (zero-sibling-weights)))                             ;;sdcc

(defun install-new-unit ()

  (when (>= *nunits* *max-units*)
    (error "Cannot add any more units."))
  ;; For now, assume total connectivity.
  (setf (ivref *nconnections* *nunits*) *nunits*)
  (setf (svref *connections* *nunits*) *all-connections*)
  ;; Copy the weight vector for the new unit.
  (let ((w (make-array *nunits* :element-type 'short-float))
        (cw (svref *cand-weights* *best-candidate*)))
    (dotimes1 (i *nunits*)
              (setf (fvref w i) (fvref cw i)))
    (setf (svref *weights* *nunits*) w)
    ;; Tell user about the new unit.
    (format t "  Add unit ~S: ~S~%"
      (+ 1 *nunits*) w))
  ;; Fix up output weights for candidate unit.
  ;; Use minus sign of correlation times a small random value as 
  ;; initial guess.  At least the sign should be right.
  (dotimes1 (o *noutputs*)
            (setf (fvref (svref *output-weights* o) *nunits*)
              (* -1.0
                 (sign (fvref (svref *cand-prev-cor* *best-candidate*) o))
                 (random 1.0))))
  ;; If using cache, run an epoch to compute this unit's values.
  (when *use-cache*
    (dotimes1 (i *max-cases*)
              (setq *values* (svref *values-cache* i))
              (compute-unit-value *nunits*)))
  ;; Reinitialize candidate units with random weights.
  (incf *nunits*)
  (if *sdcc*                                                                          ;;sdcc
      (if (or (>= *best-candidate* (/ *ncandidates* 2))                               ;;sdcc
              (not *structure*))                                                      ;;sdcc
        (setq *structure* (cons 1 *structure*))                                       ;;sdcc
        (setq *structure* (cons (+ 1 (car *structure*)) (cdr *structure*))))          ;;sdcc
    (setq *structure* (cons 1 *structure*)))                                          ;;sdcc 
  (init-candidates))

(defun adjust-correlations ()

  (setq *best-candidate* 0)
  (setq *best-candidate-score* 0.0)
  (dotimes1 (u *ncandidates*)
            (let* ((cc (svref *cand-cor* u))
                   (cpc (svref *cand-prev-cor* u))
                   (avg-value (/ (fvref *cand-sum-values* u) *ncases*))
                   (score 0.0)
                   (cor 0.0)
                   (factor (if *sdcc*                                                  ;;sdcc
                               (if (< u (/ *ncandidates* 2))                           ;;sdcc
                                   *sibling-factor*                                    ;;sdcc
                                   *descendant-factor*)                                ;;sdcc
                               1.0)))                                                  ;;sdcc
              (declare (short-float avg-value cor score))
              (dotimes1 (o *noutputs*)
                        (setq cor (/sf (-sf (fvref cc o)
                                            (*sf avg-value (fvref *sum-errors* o)))
                                       *sum-sq-error*))
                        (setf (fvref cpc o) cor)
                        (setf (fvref cc o) 0.0)
                        (incf-sf score (abs cor)))
              (setf score (* score factor))                                            ;;sdcc
              ;; Keep track of the candidate with the best overall correlation.
              (when (> score *best-candidate-score*)
                (setq *best-candidate-score* score)
                (setq *best-candidate* u)))))

(defun train-inputs-epoch ()
  
  (do ((i *first-case* (1+ i)))
      ((= i (the fixnum (+ *first-case* *ncases*))))
    (declare (fixnum i))
    ;; Compute values and errors, or recall cached values.
    (cond (*use-cache*
           (setq *values* (svref *values-cache* i))
           (setq *errors* (svref *errors-cache* i)))
          (t (setq *values* *extra-values*)
             (setq *errors* *extra-errors*)
             (full-forward-pass (svref *training-inputs* i))
             (recompute-errors (svref *training-outputs* i))))
    ;; Compute the slopes we will use to adjust candidate weights.
    (compute-slopes))
  ;; User may have changed mu between epochs, so fix shrink-factor.
  (setq *input-shrink-factor* (/sf *input-mu*
                                   (+sf 1.0 *input-mu*)))
  ;; Now adjust the candidate unit input weights using quickprop.
  (update-input-weights)
  (if (and *sdcc* *structure*)                                                     ;;sdcc
      (zero-sibling-weights))                                                      ;;sdcc
  ;; Fix up the correlation values for the next epoch.
  (adjust-correlations)
  (incf *epoch*))

(defun train-inputs (max-epochs last-epoch)

  (declare (fixnum max-epochs))
  ;; Turn sum-errors into average errors.
  (dotimes1 (o *noutputs*)
            (setf (fvref *sum-errors* o)
              (/ (fvref *sum-errors* o) *ncases*)))
  (correlations-epoch)
  (let ((last-score 0.0)
        (quit max-epochs)
        (first-time t))
    (declare (fixnum quit)
             (short-float last-score))
    (dotimes1 (i max-epochs :timeout)
              (train-inputs-epoch)
              (cond ((> *epoch* last-epoch)
                     (return :stop))
                    ((zerop *input-patience*))
                    (first-time
                     (setq first-time nil)
                     (setq last-score *best-candidate-score*))
                    ((> (abs (-sf *best-candidate-score* last-score))
                        (* last-score *input-change-threshold*))
                     (setq last-score *best-candidate-score*)
                     (setq quit (+ i *input-patience*)))
                    ((>= i quit)
                     (return :stagnant))))))

(defun list-parameters ()

  (format t "SigOff ~,2F, WtRng ~,2F, WtMul ~,2F~%"
    *sigmoid-prime-offset* *weight-range* *weight-multiplier*)
  (format t "OMu ~,2F, OEps ~,4F, ODcy ~,4F, OPat ~D, OChange ~,3F~%"
    *output-mu* *output-epsilon* *output-decay* *output-patience*
    *output-change-threshold*)
  (format t "IMu ~,2F, IEps ~,4F, IDcy ~,4F, IPat ~D, IChange ~,3F~%"
    *input-mu* *input-epsilon* *input-decay* *input-patience*
    *input-change-threshold*)
  (format t "Utype ~S, Otype ~S, RawErr ~S, Pool ~D,"
    *unit-type* *output-type* *raw-error* *ncandidates*)
  (if *proportion-threshold*
      (format t " PropTh ~,3F ~%"
        *proportion-threshold*)
    (format t " ScTh ~,3F ~%"
      *score-threshold*))
  (if *linear-outputs*
      (format t " LinO ~a ~%"
        *linear-outputs*))
  (if *random-inputs*
      (format t " NRanIn ~a, HiRanIn ~a"
        *n-random-inputs* *high-random-input*))
  (if *uniform-input-noise*
      (format t " UINoi ~,3F "
        *uniform-input-noise*))
  (if *some-uniform-input-noise*
      (format t " SUINoi ~a "
        *some-uniform-input-noise*))
  (if *uniform-output-noise*
      (format t " UONoi ~,3F "
        *uniform-output-noise*))
  (if *normal-input-noise*
      (format t " NINoi ~,3F "
        *normal-input-noise*))
  (if *some-normal-input-noise*
      (format t " SNINoi ~a "
        *some-normal-input-noise*))
  (if *normal-output-noise*
      (format t " NONoi ~,3F "
        *normal-output-noise*))
  (terpri))

(defun train (outlimit inlimit rounds &optional (last-epoch 999) (restart nil))

  (declare (fixnum outlimit inlimit rounds))
  (if (zerop (length *training-inputs*))
      (error "Error -- no *training-inputs* in neighborhood. Call set-patterns before train."))
  (if (zerop (length *training-outputs*))
      (error "Error -- no *training-outputs* in neighborhood. Call set-patterns before train."))
  (unless restart (init-net))
  (list-parameters)
  (setq *train-errors* nil
      *test-errors* nil
      *multi-errors* nil
      *proportion-train-correct* nil
      *proportion-test-correct* nil
      *proportion-multi-correct* nil
      *train-contributions* nil
      *train-activations* nil
      *test-activations* nil
      *multi-test-activations* nil
      *test-contributions* nil
      *nets-info* nil
      *hintons* nil)
  (when *use-cache*
    (dotimes1 (i *max-cases*)
              (setq *values* (svref *values-cache* i))
              (set-up-inputs (svref *training-inputs* i))))
  (dotimes1 (r rounds  :lose)
            (case (train-outputs outlimit last-epoch)
              (:stop
               (format t "Stop after last epoch ~S . ~S units, ~S hidden, Error ~S. ~% ~%"
                 (1- *epoch*) *nunits* (- *nunits* *ninputs* 1) *true-error*)
               (return nil))
              (:win
               (list-parameters)
               (format t "Victory at ~S epochs, ~S units, ~S hidden, Error ~S.~%"
                 *epoch* *nunits* (- *nunits* *ninputs* 1) *true-error*)
               (return nil))
              (:timeout
               (format t "Epoch ~D: Out Timeout  ~D bits wrong, error ~S.~2%"
                 *epoch* *error-bits* *true-error*))
              (:stagnant
               (format t "Epoch ~D: Out Stagnant ~D bits wrong, error ~S.~2%"
                 *epoch* *error-bits* *true-error*)))
            (if *mark-hiddens-errors* (mark-hiddens-errors))
            (if *test-last-output-phase-epoch* (test-epoch *score-threshold*))
            (case (train-inputs inlimit last-epoch)
              (:stop
               (return (format t "~% Stop after last epoch. ~% ~%")))
              (:timeout
               (format t "Epoch ~D: In Timeout.  Cor: ~D~%"
                 *epoch* *best-candidate-score*))
              (:stagnant
               (format t "Epoch ~D: In Stagnant.  Cor: ~D~%"
                 *epoch* *best-candidate-score*)))
            (install-new-unit)))

(defun test-epoch (&optional (*score-threshold* 0.40))

  (let ((*use-cache* nil)
        (*values* *extra-values*)
        (*errors* *extra-errors*)
        (*sum-errors* *dummy-sum-errors*)
        (train-err-bits 0)
        (test-err-bits 0)
        (train-true-err 0.0)
        (test-true-err 0.0)
        (sum-sq-err 0.0)
        (train-pats-wrong 0)
        (previous-train-err-bits 0)
        (test-pats-wrong 0)
        (previous-test-err-bits 0))
    (declare (fixnum train-err-bits test-err-bits train-pats-wrong)
             (fixnum previous-train-err-bits test-pats-wrong previous-test-err-bits)
             (short-float train-true-err test-true-err sum-sq-err))
    ;; Run all training patterns, count errors, & maybe record contributions.
    (do ((i 0 (1+ i))
         (contribs nil (if *record-train-contributions*
                           (cons (get-contributions (svref *training-outputs* i) *values*)
                                 contribs)))
         (activations nil (if *record-train-activations*
                              (cons (append 
                                     (cdr (partvector->list *nunits* *values*))
                                     (vector->list *outputs*))
                                    activations))))
        ((= i (length *training-inputs*)) (progn
                                            (if *record-train-contributions*
                                                (setq *train-contributions* 
                                                      (cons (reverse contribs) 
                                                            *train-contributions*)))
                                            (if *record-train-activations*
                                                (setq *train-activations*
                                                      (cons (reverse activations)
                                                            *train-activations*)))))
      (full-forward-pass (svref *training-inputs* i))
      (compute-errors (svref *training-outputs* i)
                      train-err-bits train-true-err sum-sq-err nil)
      
      ;;If train-err-bits increases, then increment train-pats-wrong.
      ;;Reset previous-train-err-bits.
      (if *record-proportion-train-correct*
          (progn
            (unless (= previous-train-err-bits train-err-bits)
              (incf train-pats-wrong))
            (setq previous-train-err-bits train-err-bits))))
    
    (if *record-train-errors* 
        (setq *train-errors* (cons (float->decimals train-true-err 3) *train-errors*)))
    
    ;;Record *proportion-train-correct*
    (if *record-proportion-train-correct*
        (let* ((train-pats (length *training-inputs*))
               (train-pats-correct (- train-pats train-pats-wrong)))
          (setq *proportion-train-correct* (cons (float->decimals (/ train-pats-correct
                                                                     train-pats)
                                                                  3)
                                                 *proportion-train-correct*))))
    
    (if *display-error*
        (format t "~%Epoch ~S ~%Training: ~D of ~D wrong, error ~S. ~%"
          *epoch* train-err-bits (length *training-inputs*) train-true-err))
    
    ;; Now run all test patterns and report the results.
    (when *test-inputs*
      (do ((i 0 (1+ i))
           (contribs nil (if *record-test-contributions*
                             (cons (get-contributions (svref *test-outputs* i) *values*)
                                   contribs)))
           (activations nil (if *record-test-activations*
                                (cons (append 
                                       (cdr (partvector->list *nunits* *values*))
                                       (vector->list *outputs*))
                                      activations))))
          ((= i (length *test-inputs*)) (progn
                                          (if *record-test-contributions*
                                              (setq *test-contributions* 
                                                    (cons (reverse contribs) 
                                                          *test-contributions*)))
                                          (if *record-test-activations*
                                              (setq *test-activations*
                                                    (cons (reverse activations)
                                                          *test-activations*)))))
        (full-forward-pass (svref *test-inputs* i))
        (compute-errors (svref *test-outputs* i)
                        test-err-bits test-true-err sum-sq-err nil)
        
        ;;If test-err-bits increases, then increment test-pats-wrong.
        ;;Reset previous-test-err-bits.
        (if *record-proportion-test-correct*
            (progn
              (unless (= previous-test-err-bits test-err-bits)
                (incf test-pats-wrong))
              (setq previous-test-err-bits test-err-bits)))))
    
    (if *record-test-errors* 
        (setq *test-errors* (cons (float->decimals test-true-err 3) *test-errors*)))
    (if *record-proportion-test-correct*
        (let* ((test-pats (length *test-inputs*))
               (test-pats-correct (- test-pats test-pats-wrong)))
          (setq *proportion-test-correct* (cons (float->decimals (/ test-pats-correct
                                                                    test-pats)
                                                                 3)
                                                *proportion-test-correct*))))
    (if *save-nets-for-test-setups*
        (setq *nets-info* (cons 
                           (list *nunits* 
                                 *structure*
                                 (vectors->lists *weights*)
                                 (vectors->lists *output-weights*)
                                 (vectors->lists *connections*)
                                 (vector->list *nconnections*))
                           *nets-info*)))
    (if (and *display-error*
             *test-inputs*
             (not *record-multi-test-activations*))
        (format t "Test: ~D of ~D wrong, error ~S.~%"
          test-err-bits (length *test-inputs*) test-true-err))
    (if *record-hintons*
        (setq *hintons* (cons (hintonize *weights* *output-weights*) *hintons*))
      nil)))

(defun tests-epoch (&optional (*score-threshold* 0.40))

  (do ((*use-cache* nil)
       (*values* *extra-values*)
       (*errors* *extra-errors*)
       (*sum-errors* *dummy-sum-errors*)
       (test-err-bits 0)
       (test-true-err 0.0)
       (sum-sq-err 0.0)
       (test-pats-wrong 0)
       (previous-test-err-bits 0)
       (patterns *multi-test-patterns* (cdr patterns))
       (test-inputs *test-inputs*)
       (test-outputs *test-outputs*)
       (errors nil)
       (proportions-correct nil)
       (multi-test-activations nil)
       (p 1 (1+ p)))
      ((null patterns) (progn
                         (if *record-multi-errors*
                             (setq *multi-errors*
                                   (cons (reverse errors)
                                         *multi-errors*)))
                         (if *record-proportion-multi-correct*
                             (setq *proportion-multi-correct*
                                   (cons (reverse proportions-correct)
                                         *proportion-multi-correct*)))
                         (if *record-multi-test-activations* 
                             (setq *multi-test-activations*
                                   (cons (reverse multi-test-activations)
                                         *multi-test-activations*)))
                         (setq *test-inputs* test-inputs
                             *test-outputs* test-outputs)))
    (declare (fixnum test-err-bits previous-test-err-bits test-pats-wrong)
             (short-float test-true-err sum-sq-err))
    (setq test-err-bits 0
        test-true-err 0.0
        sum-sq-err 0.0
        previous-test-err-bits 0
        test-pats-wrong 0)
    (set-patterns (car patterns) 'test)
    
    ;; Now run all test patterns and report the results.
    (do ((i 0 (1+ i))
         (activations nil (if *record-multi-test-activations*
                              (cons (append 
                                     (cdr (partvector->list *nunits* *values*))
                                     (vector->list *outputs*))
                                    activations))))
        ((= i (length *test-inputs*)) (if *record-multi-test-activations*
                                          (setq multi-test-activations
                                                (cons (reverse activations)
                                                      multi-test-activations))))
      (full-forward-pass (svref *test-inputs* i))
      (compute-errors (svref *test-outputs* i)
                      test-err-bits test-true-err sum-sq-err nil)
      
      ;;If test-err-bits increases, then increment test-pats-wrong.
      ;;Reset previous-test-err-bits.
      (if *record-proportion-multi-correct*
          (progn
            (unless (= previous-test-err-bits test-err-bits)
              (incf test-pats-wrong))
            (setq previous-test-err-bits test-err-bits))))
    
    (if *record-multi-errors* 
        (setq errors (cons (float->decimals test-true-err 3) errors)))
    (if *record-proportion-multi-correct*
        (let* ((test-pats (length *test-inputs*))
               (test-pats-correct (- test-pats test-pats-wrong)))
          (setq proportions-correct (cons (float->decimals (/ test-pats-correct
                                                              test-pats)
                                                           3)
                                          proportions-correct))))
    (if *display-error*
        (format t "  Test ~a: ~D of ~D wrong, error ~S.~%"
          p test-err-bits (length *test-inputs*) test-true-err))))

(defun test-setup (nunits weights output-weights connections nconnections)
 
  (init-net)
  (setq *weights* weights
      *output-weights* output-weights
      *nunits* nunits
      *connections* connections
      *nconnections* nconnections))

;;;;;;;;;;;;Additional interface procedures

;;;;;use as encoder network

(defun zero-input-output-weights ()

  (do ((j 0 (1+ j)))
      ((= j *noutputs*))
    (let ((w (svref *output-weights* j)))
      (do ((i 1 (1+ i)))
          ((= i (1+ *ninputs*)))
        (setf (fvref w i) 0.0)))))

;;;;;use in sdcc

(defun zero-sibling-weights ()

 (dotimes1 (i (/ *ncandidates* 2))                                    ;;sdcc
           (let ((cw (svref *cand-weights* i))                        ;;sdcc
                 (nsiblings (car *structure*)))                       ;;sdcc
             (dotimes1 (j nsiblings)                                  ;;sdcc
                       (setf (fvref cw (- *nunits* (+ 1 j))) 0.0))))) ;;sdcc 

;;;;;sign of number

(defun sign (x)

  (if (< x 0)
      -1
    1))

;;;;;random numbers

(defun call-random (m n)

  (do ((i 0 (1+ i)))
      ((= i m) nil)
    (if (zerop n) 
        (random 1)
      (random n))))

(defun seed-random ()

  (let* ((time (get-internal-real-time))
         (n (multiple-value-bind (x y) (floor time 100) (cadr (list x y))))
         (hundreds (multiple-value-bind (x y) (floor time 100) (car (list x y))))
         (m (multiple-value-bind (x y) (floor hundreds 100) (cadr (list x y)))))
    (call-random m n)))

(defun integers (x y)

  (do ((i x (1+ i))
       (result nil (cons i result)))
      ((= i (1+ y)) (reverse result))))

(defun random-range (n x y)

  (do ((i (1+ (- y x)) (1- i))
       (result nil)
       (pending (integers x y)))
      ((= (length result) n) result)
    (let* ((vec (make-array (length pending) :initial-contents pending))
           (index (random i))
           (selection (aref vec index)))
      (setq pending (remove selection pending))
      (setq result (cons selection result)))))

(defun random-range-replace (n x y)

  (let ((candidates (integers x y)))
    (do ((vec (make-array (length candidates) :initial-contents candidates))
         (result nil)
         (size (length candidates)))
        ((= (length result) n) result)
      (let ((selection (aref vec (random size))))
        (setq result (cons selection result))))))

(defun select-random (n lst)

  (let ((size (length lst)))
    (do ((selections (random-range n 0 (1- size)) (cdr selections))
         (result nil))
        ((null selections) result)
      (push (car (nthcdr (car selections) lst)) result))))

;;;;;vectors & lists

(defun list->vector (lst)

  (make-array (list (length lst))
              :initial-contents
              lst))

(defun vector->list (vector)

  (do ((i 0 (1+ i))
       (lst nil (cons (aref vector i) lst)))
      ((= i (car (array-dimensions vector))) (reverse lst))))

(defun partvector->list (n v)

  (nextn n (vector->list v)))

(defun lists->vectors (lists)

  (make-array (list (length lists))
              :initial-contents
              (mapcar #'list->vector lists)))

(defun vectors->lists (vectors)
  
  (do ((i 0 (1+ i))
       (lists nil (if (null (aref vectors i))
                      (cons nil lists)
                    (cons (vector->list (aref vectors i)) lists))))
      ((= i (length vectors)) (reverse lists))))

(defun partvectors->lists (n vectors)

  (do ((i 0 (1+ i))
       (lst nil (cons (nextn n (vector->list (aref vectors i))) lst)))
      ((= i (car (array-dimensions vectors))) (reverse lst))))

;;;;;setting up the training and testing patterns

(defun set-patterns (patterns type)

  (let* ((pats (if (stringp patterns)
                   (with-open-file 
                       (pattern-stream patterns :direction :input)
                     (read pattern-stream))
                 patterns))
         (npatterns (length pats)))
    (setq *ninputs* (length (caar pats)))
    (setq *noutputs* (length (cadar pats)))
    (do ((pats pats (cdr pats))
         (i 0 (1+ i))
         (input (make-array npatterns))
         (output (make-array npatterns)))
        ((= i npatterns) (cond ((or (eq type 'train) 
                                    (eq type 'change-train))
                                (setf *training-inputs* input)
                                (setf *training-outputs* output))
                               ((eq type 'test)
                                (setf *test-inputs* input)
                                (setf *test-outputs* output))
                               (t nil)))
      (setf (svref input i) 
        (make-array *ninputs* :initial-contents (caar pats)))
      (setf (svref output i) 
        (make-array *noutputs* :initial-contents (cadar pats)))))
  (cond ((eq type 'train)
         (progn
           (build-net *ninputs* *noutputs*)
           (init-net)))
        ((eq type 'change-train)
         (changed-training-set))
        (t nil))
  (if *random-inputs*
      (setq *ninputs* (+ *ninputs* *n-random-inputs*)))
  (if (or *random-inputs*
          *uniform-input-noise*
          *some-uniform-input-noise*
          *normal-input-noise*
          *some-normal-input-noise*)
      (progn (setf *standard-training-inputs* *training-inputs*)
        (setf *standard-test-inputs* *test-inputs*)))
  (if (or *uniform-output-noise*
          *normal-output-noise*)
      (progn 
        (setf *standard-training-outputs* *training-outputs*)
        (setf *standard-test-outputs* *test-outputs*))))

(defun modify-patterns ()

  (if *select-patterns* (select-patterns))
  (if *random-inputs*
      (progn
        (setq *training-inputs* 
              (random-vectors *standard-training-inputs* *n-random-inputs* *high-random-input*))
        (setq *test-inputs* 
              (random-vectors *standard-test-inputs* *n-random-inputs* *high-random-input*))))
  (if *uniform-input-noise*
      (progn
        (setq *training-inputs* 
              (noise-vectors *uniform-input-noise* 'uniform *standard-training-inputs*))
        (setq *test-inputs* 
              (noise-vectors *uniform-input-noise* 'uniform *standard-test-inputs*))))
  (if *some-uniform-input-noise*
      (progn
        (setq *training-inputs* (some-noise-vectors 
                                 (car *some-uniform-input-noise*) 
                                 'uniform
                                 (cadr *some-uniform-input-noise*)
                                 *standard-training-inputs*))
        (setq *test-inputs* (some-noise-vectors 
                             (car *some-uniform-input-noise*) 
                             'uniform
                             (cadr *some-uniform-input-noise*)
                             *standard-test-inputs*))))
  (if *uniform-output-noise*
      (progn
        (setq *training-outputs* 
              (noise-vectors *uniform-output-noise* 'uniform *standard-training-outputs*))
        (setq *test-outputs* 
              (noise-vectors *uniform-output-noise* 'uniform *standard-test-outputs*))))
  (if *normal-input-noise*
      (progn
        (setq *training-inputs* 
              (noise-vectors *normal-input-noise* 'normal *standard-training-inputs*))
        (setq *test-inputs* 
              (noise-vectors *normal-input-noise* 'normal *standard-test-inputs*))))
  (if *some-normal-input-noise*
      (progn
        (setq *training-inputs* (some-noise-vectors 
                                 (car *some-normal-input-noise*) 
                                 'normal
                                 (cadr *some-normal-input-noise*)
                                 *standard-training-inputs*))
        (setq *test-inputs* (some-noise-vectors 
                             (car *some-normal-input-noise*) 
                             'normal
                             (cadr *some-normal-input-noise*)
                             *standard-test-inputs*))))
  (if *normal-output-noise*
      (progn
        (setq *training-outputs* 
              (noise-vectors *normal-output-noise* 'normal *standard-training-outputs*))
        (setq *test-outputs* 
              (noise-vectors *normal-output-noise* 'normal *standard-test-outputs*))))
  (if (or *random-inputs*
          *uniform-input-noise*
          *some-uniform-input-noise*
          *uniform-output-noise*
          *normal-input-noise*
          *some-normal-input-noise*
          *normal-output-noise*)
      (changed-training-set)))

;;;;;;;;;;;recording test errors

(defun mark-hiddens-errors ()
  "Mark hidden unit installations in error files."
  (if *record-train-errors*
      (setq *train-errors* (cons 'h *train-errors*)))
  (if *record-test-errors*
      (setq *test-errors* (cons 'h *test-errors*)))
  (if *record-multi-errors*
      (setq *multi-errors* (cons 'h *multi-errors*)))
  nil)

;;;;;;;;;;;convert float to decimals

(defun float->decimals (f d)

  (float (/ (round (* (expt 10 d) f)) (expt 10 d))))

;;;;;saving lists in file

(defun lists->file (lst file &optional (separator " "))

  (with-open-file
      (output-stream file :direction :output)
    (do ((items (reverse lst) (cdr items)))
        ((null items) 'done)
      (let ((sub-item (car items)))
        (if (listp sub-item)
            (print-line sub-item separator output-stream)
          (format output-stream "~a ~%"
            sub-item))))))

(defun print-line (lst &optional (separator " ") output-stream)

  (do ((lst lst (cdr lst)))
      ((null lst) (terpri output-stream))
    (princ (car lst) output-stream)
    (princ separator output-stream)))

;;;;;recording & saving contributions

(defun get-contributions (targets activations)

  (do ((targets (vector->list targets) (cdr targets))
       (activations (nextn *nunits* (vector->list activations)))
       (outwts (partvectors->lists *nunits* *output-weights*) (cdr outwts))
       (contributions nil))
      ((null outwts) contributions)
    (do ((ow (car outwts) (cdr ow))
         (act activations (cdr act))
         (2t (*sf 2.0 (car targets)))
         (contribs nil))
        ((null ow) (setf contributions (append contributions (reverse contribs))))
      (let ((product (if *scale-contributions-by-target*
                         (float->decimals (*sf (car ow) (car act) 2t) 3)
                       (float->decimals (*sf (car ow) (car act)) 3))))
        (setf contribs (cons product contribs))))))

(defun nextn (n lst)

  (do ((lst lst (cdr lst))
       (i 0 (1+ i))
       (result nil))
      ((= i n) (reverse result))
    (setf result (cons (car lst) result))))

(defun save-contributions (lst path type net)

  (do ((contribs (reverse lst) (cdr contribs))
       (i 1 (1+ i)))
      ((null contribs))
    (cond ((eq type 'train)
           (lists->file (reverse (car contribs)) 
                        (concatenate 'string 
                          path
                          "net" 
                          (princ-to-string net) 
                          " train contribs" 
                          (princ-to-string i))))
          ((eq type 'test)
           (lists->file (reverse (car contribs)) 
                        (concatenate 'string 
                          path
                          "net" 
                          (princ-to-string net) 
                          " test contribs" 
                          (princ-to-string i))))
          (t nil))))

;;;;;normal noise

(defun unit-circle (v1 v2)

  (let ((r (+ (expt v1 2) (expt v2 2))))
    (if (>= r 1)
        (let ((v3 (1- (* 2 (random 1.0))))
              (v4 (1- (* 2 (random 1.0)))))
          (unit-circle v3 v4))
      r)))

(defun normal (&optional (d 3) (mean 0) (sd 1))
  
  (let* ((v1 (1- (* 2 (random 1.0))))
         (v2 (1- (* 2 (random 1.0))))
         (r (unit-circle v1 v2))
         (fac (sqrt (/ (* -2 (log r)) r))))
    (float->decimals (+ (* sd (* fac v1)) mean) d)))

;;;;;adding noise to input or target patterns

(defun noise-vectors (noise type vector)

  (let* ((size (car (array-dimensions vector)))
         (noisy-vector (make-array (list size)
                                   :initial-element
                                   nil)))
    (dotimes (i size noisy-vector)
      (setf (aref noisy-vector i) (noise-vector noise type (aref vector i))))))

(defun noise-vector (noise type vector)

  (let* ((size (length vector))
         (noisy-vector (make-array (list size) 
                                   :initial-element
                                   nil)))
    (dotimes (j size noisy-vector)
      (let* ((value (aref vector j))
             (sign (if (eq type 'uniform)
                       (random-sign)
                     0))
             (addend (if (eq type 'uniform)
                         (if (integerp noise)
                             (* sign (random (1+ noise)))
                           (* sign (random noise)))
                       0))
             (noisy-value (if (eq type 'uniform)
                              (float->decimals (+ addend value) 3)
                            (+ (normal 3 0 noise) value))))
        (setf (aref noisy-vector j) noisy-value)))))

(defun some-noise-vectors (noise type positions vector)
 
  (let* ((size (length vector))
         (noisy-vector (make-array (list size)
                                   :initial-element
                                   nil)))
    (dotimes (i size noisy-vector)
      (setf (aref noisy-vector i) (some-noise-vector noise type positions (aref vector i))))))

(defun some-noise-vector (noise type positions vector)

  (do ((noisy-vector (make-array (list (length vector))
                                 :initial-contents
                                 (vector->list vector)))
       (positions positions (cdr positions)))
      ((null positions) noisy-vector)
    (let* ((position (car positions))
           (value (aref vector position))
           (sign (if (eq type 'uniform)
                     (random-sign)
                   0))
           (addend (if (eq type 'uniform)
                       (if (integerp noise)
                           (* sign (random (1+ noise)))
                         (* sign (random noise)))
                     0))
           (noisy-value (if (eq type 'uniform)
                            (float->decimals (+ addend value) 3)
                          (+ (normal 3 0 noise) value))))
      (setf (aref noisy-vector position) noisy-value))))

(defun random-sign ()
  "Return +1 or -1 randomly."
  (if (= (random 2) 0)
      1
    -1))

;;;;;random inputs: Net must learn to ignore them. Call random-inputs.

(defun random-inputs (n &optional (high 1))

  (setq *random-inputs* t)
  (if (integerp high)
      (setq *high-random-input* (1+ high))
    (setq *high-random-input* high))
  (setq *n-random-inputs* n))

(defun random-vectors (vector n high)

  (let* ((size (car (array-dimensions vector)))
         (big-vector (make-array (list size)
                                 :initial-element
                                 nil)))
    (dotimes (i size big-vector)
      (setf (aref big-vector i) (random-vector (aref vector i) n high)))))

(defun random-vector (vector n high)

  (let* ((size (car (array-dimensions vector)))
         (long-size (+ n size))
         (long-vector (make-array (list long-size)
                                  :initial-element
                                  nil)))
    (dotimes (j size nil)
      (setf (aref long-vector j) (aref vector j)))
    (do ((j (- long-size n) (1+ j))
         (value (random high) (random high)))
        ((= j long-size) long-vector)
      (setf (aref long-vector j) value))))

;;;;;;;;;;;producing values for Hinton diagrams

(defun max-abs (x y)

  (max-absolute (max-abs-vectors x)
                (max-abs-vectors y)))

(defun max-absolute (x y)

  (if (> (abs x) (abs y))
      (abs x)
    (abs y)))

(defun max-abs-vector (vector)

  (do ((n 0 (1+ n))
       (size (car (array-dimensions vector)))
       (max 0))
      ((= n size) max)
    (setq max (max-absolute max (aref vector n)))))

(defun max-abs-vectors (vectors)
 
  (do ((n 0 (1+ n))
       (size (car (array-dimensions vectors)))
       (max 0))
      ((= n size) max)
    (let ((vector (aref vectors n)))
      (if (eq vector nil)
          nil
        (setq max (max-absolute max (max-abs-vector (aref vectors n))))))))

(defun vector->square (vector max squares)
 
  (do ((n 0 (1+ n))
       (size (car (array-dimensions vector)))
       (a-list nil))
      ((= n size) (reverse a-list))
    (let ((value (aref vector n)))
      (if (= value 0)
          nil
        (setq a-list (cons (list n
                                 (transform value max squares))
                           a-list))))))

(defun transform (value max squares)
 
  (let* ((transformed (log (1+ (abs value)) 10))
         (trans (if (< value 0)
                    (* -1 transformed)
                  transformed))
         (tr (* squares (/ trans (log (1+ max) 10)))))
    (cond ((> (abs tr) .5) (round tr))
          ((> tr 0) 1)
          (t -1))))

(defun vectors->square (vectors max s type)

  (do ((n 0 (1+ n))
       (size (car (array-dimensions vectors)))
       (a-lists nil))
      ((= n size) a-lists)
    (let ((vector (aref vectors n)))
      (if (eq vector nil)
          nil 
        (if (eq type 'weight)
            (setq a-lists (cons (cons n (list (vector->square (aref vectors n) max s)))
                                a-lists))
          (setq a-lists (cons (cons (+ *nunits* n) 
                                    (list (vector->square (aref vectors n) max s)))
                              a-lists)))))))

(defun hintonize (weights output-weights &optional (s 7))
 
  (let* ((max-weight (max-abs weights output-weights))
         (weight-list (vectors->square weights max-weight s 'weight))
         (output-weight-list (vectors->square output-weights max-weight s 'output-weight)))
    (append output-weight-list weight-list)))

;;;;;use test-setup via files

(defun save-for-test-setup (filename)
 
  (with-open-file
      (out-stream filename :direction :output)
    (pprint *nunits* out-stream)
    (pprint *weights* out-stream)
    (pprint *output-weights* out-stream)
    (pprint *connections* out-stream)
    (pprint *nconnections* out-stream)))

(defun use-test-setup (filename)
  
  (with-open-file
      (in-stream filename :direction :input)
    (test-setup (read in-stream)
                (read in-stream)
                (read in-stream)
                (read in-stream)
                (read in-stream))))

(defun save-list-for-test-setup (lst file)
 
  (with-open-file
      (out-stream file :direction :output)
    (pprint (first lst) out-stream)
    (pprint (lists->vectors (second lst)) out-stream)
    (pprint (lists->vectors (third lst)) out-stream)
    (pprint (lists->vectors (fourth lst)) out-stream)
    (pprint (list->vector (fifth lst)) out-stream)))

(defun read-nth (n file)
 
  (with-open-file
      (in file :direction :input)
    (do ((i 0 (1+ i)))
        ((= i n) (read in))
      (read in))))

(defun get&save-net-info (positions net)

  (do ((positions positions (cdr positions))
       (info (read-nth 2 (concatenate 'string 
                           "net" 
                           (princ-to-string net) 
                           "results"))))
      ((null positions))
    (save-list-for-test-setup 
     (nth (car positions) info) 
     (concatenate 'string 
       "net" 
       (princ-to-string net)
       "pos"
       (princ-to-string (1+ (car positions)))))))

(defun save-contributions1 (net epoch)

  (lists->file (reverse (car *test-contributions*))
               (concatenate 
                   'string "net" (princ-to-string net) "test contribs" (princ-to-string epoch))))

(defun fetch-for-contributions (positions net)

  (do ((positions positions (cdr positions))
       (patterns (with-open-file
                     (problems-in 
                      (concatenate 'string 
                        "net" 
                        (princ-to-string net) 
                        "results") 
                      :direction :input)
                   (read problems-in))))
      ((null positions))
    (set-patterns patterns 'train)
    (set-patterns patterns 'test)
    (use-test-setup (concatenate 'string 
                      "net" 
                      (princ-to-string net) 
                      "pos" 
                      (princ-to-string (1+ (car positions)))))
    (test-epoch)
    (save-contributions1 net (1+ (car positions)))))

;;;;;help for pca of contributions

(defun sd (numbers)

  (do ((numbers numbers (cdr numbers))
       (sum 0 (+sf sum (car numbers)))
       (sum^2 0 (+sf sum^2 (expt (car numbers) 2)))
       (n (length numbers)))
      ((null numbers) 
       (let ((variance (/sf (-sf (*sf n sum^2)
                                 (expt sum 2))
                            (*sf n (1- n)))))
         (if (< variance 0)
             0
           (sqrt variance))))))

(defun file->list (filename)

  (with-open-file 
      (in-stream filename :direction :input)
    (do ((lst nil (cons item lst))
         (item (read in-stream nil) (read in-stream nil)))
        ((null item) (reverse lst)))))

(defun filerows->lists (file r)

  (let* ((lst (file->list file))
         (cols (/ (length lst) r)))
    (do ((flatlist lst (nthcdr cols flatlist))
         (results nil)
         (i 0 (1+ i)))
        ((= i r) (reverse results))
      (setq results (cons (nextn cols flatlist) results)))))

(defun transpose (lists)

  (let* ((rows (length lists))
         (cols (length (car lists)))
         (start-array (make-array  (list rows cols)
                                  :initial-contents
                                  lists))
         (result-array (make-array (list cols rows))))
    (do ((i 0 (1+ i)))
        ((= i rows) (2d-array->lists result-array))
      (do ((j 0 (1+ j)))
          ((= j cols))
        (setf (aref result-array j i) (aref start-array i j))))))

(defun 2d-array->lists (array)
 
  (let* ((dimensions (array-dimensions array))
         (rows (car dimensions))
         (cols (cadr dimensions)))
    (do ((result nil)
         (i 0 (1+ i)))
        ((= i rows) (reverse result))
      (do ((sublist nil)
           (j 0 (1+ j)))
          ((= j cols) (setf result (cons (reverse sublist) result)))
        (setf sublist (cons (aref array i j) sublist))))))

(defun sds-of-contributions (file n)
 
  (do ((lst (transpose (filerows->lists file n)) (cdr lst))
       (sds nil (cons (sd (car lst)) sds)))
      ((null lst) (reverse sds))))

(defun divide-each-score (scores divisor)

  (do ((lst (cdr scores) (cdr lst))
       (head (car scores))
       (results nil))
      ((null lst) (cons head (reverse results)))
    (let ((item (car lst)))
      (setq results (cons 
                     (if (and (numberp divisor)
                              (not (zerop divisor)))
                         (/ item divisor)
                       item)
                     results)))))

(defun floats-lists->file (lst file &optional (separator " "))
  
  (with-open-file
      (output-stream file :direction :output)
    (do ((lists (reverse lst) (cdr lists)))
        ((null lists) 'done)
      (print-floats-line (car lists) separator output-stream))))

(defun print-floats-line (lst &optional (separator " ") output-stream)

  (do ((lst lst (cdr lst)))
      ((null lst) (terpri output-stream))
    (format output-stream "~2,2F~a"
      (car lst) separator)))

(defun standardize-loadings (path contributions-file n loadings-file)

  (let* ((sds (sds-of-contributions (concatenate 'string
                                      path
                                      contributions-file) 
                                    n))
         (number-of-loadings (length sds)))
    (do ((sds sds (cdr sds))
         (loadings (filerows->lists (concatenate 'string
                                      path
                                      loadings-file) 
                                    number-of-loadings) 
                   (cdr loadings))
         (results nil))
        ((null sds) (floats-lists->file results (concatenate 'string
                                                  path
                                                  "standard loadings")))
      (setq results (cons (divide-each-score (car loadings) (car sds))
                          results)))))

;;; The End.

