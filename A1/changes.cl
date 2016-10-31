
 
 
(defun initialize-array (nu nx ny ns1 ns2 nb)
   "(nu nx ny nb)"
  (setf *array-size* (+ nu nx ny ns1 ns2 nb)
     *agents* (make-array *array-size* :initial-element nil)))
 
 ;;;(initialize-array 4 1 0)
 
 (defun make-1-agent (i s)
   "(i s)"
   (setf (aref *agents* i) 
     (make-agent :id i
                 :state s)))
 
 ;;;(make-1-agent 2 'u)
 
(defun make-agent-array (nu nx ny nb ns1 ns2)
   "(nu nx ny nb)"
   (do ((i 0 (1+ i)))
       ((= i nu))
     (make-1-agent i 'u))
     (make-1-agent i 'y))
   ; added this
   (do ((i (+ nu nx ny) (1+ i)))
    ((= i (+ nu nx ny ns1)))
    (make-1-agent i 's1))
  (do ((i (+ nu nx ny ns1) (1+ i)))
    ((= i (+ nu nx ny ns1 ns2)))
    (make-1-agent i 's2))
  (do ((i (+ nu nx ny ns1 ns2) (1+ i)))
      ((= i (+ nu nx ny ns1 ns2 nb)))
     (make-1-agent i 'b)))
        
 ;;;(initialize-array 4 2 0)
 
 (defun get-agent-state (a)
   "(a)"
   (let ((agent (aref *agents* a)))
     (agent-state agent)))
 
 
 (defun count-state (state)
   "(state)"
   (do ((i 0 (1+ i))
        (n 0 (if (eq (get-agent-state i) state)
                 (1+ n)
 
 (defun count-states (states)
   "(states)"
   (do ((stts states (cdr stts))
        (counts nil (cons (count-state (car stts))
                          counts)))
 
 (defun count-state-section (state start end)
   "(state start end)"
   (do ((i start (1+ i))
        (n 0 (if (eq (get-agent-state i) state)
                 (1+ n)
 
 ;;;interact
 
(defun interact-am (agents p p2 p3)
   "(agents p)"
   (let* ((i (first agents))
          (r (second agents))
          (r-agent (aref *agents* r))
      ((and (eq r-state 'u)
            (not (eq i-state 'u))
            (not (eq i-state 'b))
           (not (eq i-state 's1))
           (not (eq i-state 's2))
           (not (eq r-state 'b))
            (< (random 1.0) p))
       (setf (agent-state r-agent) i-state))
      ((and (not (eq i-state 'u))
            (not (eq i-state 'b))
            (not (eq r-state 'u))
            (not (eq r-state 'b))
           (not (eq i-state 's2))
           (not (eq r-state 's1))
           (not (eq r-state 's2))
            (not (eq i-state r-state))
            (< (random 1.0) p))
       (setf (agent-state r-agent) 'u))
     ; if r's state is slow and i is not undecided/byzantine, p2 p3 chance of changing
     ((and 
        (not (eq i-state 's2))
        (not (eq i-state 's1))
        (not (eq i-state 'u))
        (not (eq i-state 'b))
        (not (eq r-state 's2))
        (not (eq r-state 'b))
        (< (random 1.0) p2))
     (setf (agent-state r-agent) i-state))
      ((and 
        (not (eq i-state 's2))
        (not (eq i-state 's1))
         (not (eq i-state 'u))
         (not (eq i-state 'b))
        (not (eq r-state 's1))
        (not (eq r-state 'b))
        (< (random 1.0) p3))
     (setf (agent-state r-agent) i-state))
      (t nil))
 
     ))
     
 
(defun interact-ac (agents p p2 p3)
   "(agents p)"
   (let* ((i (first agents))
          (r (second agents))
          (r-agent (aref *agents* r))
          (i-state (get-agent-state i))
          (r-state (get-agent-state r)))
    (cond 
        ((and 
             (not (eq i-state 'u))
              (not (eq i-state 'b))
              (not (eq r-state 'b))
             (not (eq r-state 's1))
             (not (eq r-state 's2))
              (not (eq i-state r-state))
             (not (eq i-state 's1))
             (not (eq i-state 's2))
              (< (random 1.0) p))

        (setf (agent-state r-agent) i-state))
        ((and 
            (not (eq i-state 'u))
            (not (eq i-state 'b))
            (not (eq i-state r-state))
            (not (eq r-state 'b))
            (not (eq r-state 's2))
            (not (eq i-state 's1))
            (not (eq i-state 's2))
            (< (random 1.0) p2))
        (setf (agent-state r-agent) i-state))
        ((and 
            (not (eq i-state 'u))
            (not (eq i-state 'b))
            (not (eq i-state r-state))
            (not (eq r-state 's1))
            (not (eq r-state 'b))
            (not (eq i-state 's1))
            (not (eq i-state 's2))
            (< (random 1.0) p3))
        (setf (agent-state r-agent) i-state)))))
 
 ;;;(interact-ac '(4 0) 1.0)
 
(defun interact-ciu (agents p p2 p3)
   "(agents p)"
   (let* ((i (first agents))
          (r (second agents))
          (r-agent (aref *agents* r))
          (i-state (get-agent-state i))
          (r-state (get-agent-state r)))
    (cond 
        ((and (not (eq i-state 'u))
              (not (eq i-state 'b))
             (not (eq i-state 's1))
             (not (eq i-state 's2))
              (eq r-state 'u)
              (< (random 1.0) p))
        (setf (agent-state r-agent) i-state))

        ((and (not (eq i-state 'u))
             (not (eq i-state 'b))
             (not (eq i-state 's1))
             (not (eq i-state 's2))
             (eq r-state 's1)
             (< (random 1.0) p2))
        (setf (agent-state r-agent) i-state))
        ((and (not (eq i-state 'u))
             (not (eq i-state 'b))
             (not (eq i-state 's1))
             (not (eq i-state 's2))
             (eq r-state 's2)
             (< (random 1.0) p3))
        (setf (agent-state r-agent) i-state)))))

 

 
 
(defun call-algorithm (algo p p2 p3)
   "(algo p)"
   (case algo
    (am (interact-am (get-2-random-agents) p p2 p3))
    (ac (interact-ac (get-2-random-agents) p p2 p3))
    (ciu (interact-ciu (get-2-random-agents) p p2 p3))
     (otherwise (error "Unknown algorithm"))))
 
 ;;;run simulation
 
(defun run1-saturation (nu nx ny nb ns1 ns2 p p2 p3 replication algo)
   "(nu nx ny nb p replication algo)"
  (initialize-array nu nx ny nb ns1 ns2)
  (make-agent-array nu nx ny nb ns1 ns2)
   (do ((cycles 0 (1+ cycles))
        (ucount (list nu) (cons (count-state 'u) 
                                ucount))
                                ycount)))
       ((consensusp (list (first ucount) (first xcount) (first ycount)))
        (progn 
         ; (lists->file ucount 
         ;              (concatenate 'string *path* (princ-to-string replication) "ucount"))
         ; (lists->file xcount 
         ;              (concatenate 'string *path* (princ-to-string replication) "xcount"))
         ; (lists->file ycount 
         ;              (concatenate 'string *path* (princ-to-string replication) "ycount"))
          (list (reverse ucount) 
                (reverse xcount) 
                (reverse ycount) 
                cycles
                (list (first xcount) (first ycount)))))
    (call-algorithm algo p p2 p3)))
 
(defun run-saturation (n nu nx ny nb ns1 ns2 p p2 p3 algorithm)
   "(n nu nx ny nb p algorithm)"
   (seed-random)
  (setq *path* "/Users/Elias/Documents/models/saturation/"); m/copy/results/")
   (do ((i 0 (1+ i))
        (cycles nil)
        (ucounts nil)
                                       :direction :output)
                      (format output-stream "~a x  ~a y"
                        (first winners) (second winners))))))
    (let ((counts (run1-saturation nu nx ny nb ns1 ns2 p p2 p3 i algorithm)))
       (setf 
        ucounts (cons (first counts)
                      ucounts)
; (run-saturation 20 73 2 0 15 4 6 1.0 0.25 0.75 'am)
 
(defun run1-to-cycle (nu nx ny nb ns1 ns2 max p p2 p3 replication algo)
   "(nu nx ny nb max p replication algo)"
  (initialize-array nu nx ny nb ns1 ns2)
  (make-agent-array nu nx ny nb ns1 ns2)
   (do ((i 0 (1+ i))
        (ucount (list nu) (cons (count-state 'u) 
                                ucount))
        (xcount (list nx) (cons (count-state 'x) 
                                xcount))
        (ycount (list ny) (cons (count-state 'y) 
                               ycount))
       (s1count (list ns1) (cons (count-state 's1)
                               s1count))
       (s2count (list ns2) (cons (count-state 's2)
                               s2count)))
       ((= i max) (progn 
                    (lists->file ucount 
                                 (concatenate 'string *path* (princ-to-string replication) "ucount"))
                                 (concatenate 'string *path* (princ-to-string replication) "xcount"))
                    (lists->file ycount 
                                 (concatenate 'string *path* (princ-to-string replication) "ycount"))
                   (lists->file s1count
                                (concatenate 'string *path* (princ-to-string replication) "s1count"))
                   (lists->file s2count
                                (concatenate 'string *path* (princ-to-string replication) "s2count"))
                   (list (reverse ucount) ;first
                         (reverse xcount) ;second
                         (reverse ycount) ;third
                         (reverse s1count) ;fourth
                         (reverse s2count) ;fifth
                         (list (first xcount) (first ycount))))) ;sixth
    (call-algorithm algo p p2 p3)))

(defun run-to-cycle (n nu nx ny nb ns1 ns2 max p p2 p3 algorithm)
   "(n nu nx ny nb max p algorithm)"
   (seed-random)
  (setq *path* "/Users/Elias/Documents/models/cycle/"); m/copy/results/")
   (do ((i 0 (1+ i))
        (ucounts nil)
        (xcounts nil)
        (ycounts nil)
       (s1counts nil)
       (s2counts nil)
        (finishes nil))
       ((= i n) (progn
                  (lists->file
                   (reverse (rotate (reverse ycounts)))
                   (concatenate 'string *path* "ycounts"))
                  (lists->file finishes (concatenate 'string *path* "finishes"))
                 (lists->file (reverse (rotate (reverse s1counts)))
                    (concatenate 'string *path* "s1counts"))
                 (lists->file (reverse (rotate (reverse s2counts)))
                    (concatenate 'string *path* "s2counts"))
                  (let ((winners (winners finishes)))
                    (with-open-file
                        (output-stream (concatenate 'string *path* "winners") 
                                       :direction :output)
                      (format output-stream "~a x  ~a y"
                        (first winners) (second winners))))))
    (let ((counts (run1-to-cycle nu nx ny nb ns1 ns2 max p p2 p3 i algorithm)))
       (setf 
        ucounts (cons (first counts)
                      ucounts)
                      xcounts)
        ycounts (cons (third counts)
                      ycounts)
       s1counts (cons (fourth counts)
                     s1counts)
       s2counts (cons (fifth counts)
                     s2counts)
       finishes (cons (sixth counts)
                       finishes)))))
 

;;;(run-to-cycle 20 73 2 0 15 4 6 1100 1.0 0.25 0.75 'am)
 
;;; (run-to-cycle 20 74 1 0 15 4 6 770 1.0 0.25 0.75 'am)
 

 
(defun run1-to-cycle-switch (nu nx ny ns1 ns2 nb max p p2 p3 algo)
   "(nu nx ny nb max p algo)"
       ((= i max) (list (count-state-section 'u 75 89)
                        (count-state-section 'x 75 89)
                        (count-state-section 'y 75 89)
                       (count-state-section 's1 75 89)
                       (count-state-section 's2 75 89)
                        (count-state-section 'b 75 89)))
    (call-algorithm algo p p2 p3)))
 
(defun run-to-cycle-switch (n nu nx ny nb ns1 ns2 max p p2 p3 algorithm)
   "(n nu nx ny nb max p algorithm)"
   (seed-random)
  (setq *path* "/Users/Elias/Documents/models/")
   (do ((i 0 (1+ i))
       (counts nil (cons (run1-to-cycle-switch nu nx ny ns1 ns2 nb max p p2 p3 algorithm)
                          counts)))
       ((= i n) (lists->file 
                 counts

; (run-to-cycle-switch 20 0 75 14 15 4 6 1100 1.0 0.25 0.75 'am)
 

 
(defun run1-to-cycle-persist (nu nx ny ns1 ns2 nb max p p2 p3 replication algo)
   "(nu nx ny nb max p replication algo)"
  (initialize-array nu nx ny nb ns1 ns2)
  (make-agent-array nu nx ny nb ns1 ns2)
   (do ((i 0 (1+ i))
        (countx-newbies nil (cons (count-state-section 'x 0 45)
                                  countx-newbies))
        (countx-vets nil (cons (count-state-section 'x 45 75) 
                               countx-vets)))
       ((= i max) (progn 
                   ; (lists->file 
                   ;  countx-newbies
                   ;  (concatenate 'string *path* (princ-to-string replication) "countx-newbies"))
                   ; (lists->file 
                   ;  countx-vets
                   ;  (concatenate 'string *path* (princ-to-string replication) "countx-vets"))
                    (list (reverse countx-newbies) (reverse countx-vets))))
    (call-algorithm algo p p2 p3)))
 
(defun run-to-cycle-persist (n nu nx ny nb ns1 ns2 max p p2 p3 algorithm)
   "(n nu nx ny nb max p algorithm)"
   (seed-random)

  (setq *path* "/Users/Elias/Documents/models/persist/") ;m/copy/results/")
   (do ((i 0 (1+ i))
        (countx-newbies nil)
        (countx-vets nil))
                   (concatenate 'string *path* "countx-newbies"))
                  (lists->file 
                   (reverse (rotate (reverse countx-vets)))
                  (concatenate 'string *path* "countx-vets")))) (let ((countx-both (run1-to-cycle-persist nu nx ns1 ns2 ny nb max p p2 p3 i algorithm)))
       (setf 
        countx-newbies (cons (first countx-both)
                             countx-newbies)
                          countx-vets)))))
                                  
;;;(run-to-cycle-persist 20 45 30 0 15 4 6 275 1.0 0.25 0.75 'am)
;;;(run-to-cycle-persist 20 45 30 0 15 4 6 275 1.0 0.25 0.75 'ac)
;;;(run-to-cycle-persist 20 45 30 0 15 4 6 275 1.0 0.25 0.75 'ciu)

