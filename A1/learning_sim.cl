;;;file: learning_sim.cl
;;;purpose: compare 3 copying algorithms: AM, AC, & CIU
;;;programmer: Tom Shultz
;;;modified by: Elias Stengel-Eskin (260609642)
;;;new in learning_sim: s1, s2 agents copy with probability p2, p3
;;;interactions adjusted to include this
;;;parameters refactored for these 4 extra values
;;;removed several docstrings as they were causing problems while compiling

;;;globals

(defstruct agent
  "Agent has id index & state."
  id
  state)

(defvar *agents* nil
  "Array of agents.")

(defvar *array-size* 100
  "Size of agent array.")

(defvar *path* nil
  "Path for saving files.")


(defun call-random (m n)
  "(m n)"
  (do ((i 0 (1+ i)))
      ((= i m) nil)
    (if (zerop n) 
        (random 1)
      (random n))))

(defun seed-random ()
  "Seed random from the last 4 digits of time." 
  (let* ((time (get-internal-real-time))
         (n (multiple-value-bind (x y) (floor time 100) (cadr (list x y))))
         (hundreds (multiple-value-bind (x y) (floor time 100) (car (list x y))))
         (m (multiple-value-bind (x y) (floor hundreds 100) (cadr (list x y)))))
    (call-random m n)))


; added number of s1 and s2 agents
(defun initialize-array (nu nx ny ns1 ns2 nb)
  "(nu nx ny nb). initializes an empty array with sum(args) agents"
  (setf *array-size* (+ nu nx ny ns1 ns2 nb)
    *agents* (make-array *array-size* :initial-element nil)))


(defun make-1-agent (i s)
  "(i s)"
  (setf (aref *agents* i) 
    (make-agent :id i
                :state s)))

; added number of s1 and s2 agents
(defun make-agent-array (nu nx ny nb ns1 ns2)
  "(nu nx ny nb). Fills the empty array with the specified number of each agent."
  (do ((i 0 (1+ i)))
      ((= i nu))
    (make-1-agent i 'u))
  (do ((i nu (1+ i)))
      ((= i (+ nu nx)))
    (make-1-agent i 'x))
  (do ((i (+ nu nx) (1+ i)))
      ((= i (+ nu nx ny)))
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
       

(defun get-agent-state (a)
  "(a)"
  (let ((agent (aref *agents* a)))
    (agent-state agent)))



(defun count-state (state)
  "(state)"
  (do ((i 0 (1+ i))
       (n 0 (if (eq (get-agent-state i) state)
                (1+ n)
              n)))
      ((= i *array-size*) n)))


(defun count-states (states)
  "(states)"
  (do ((stts states (cdr stts))
       (counts nil (cons (count-state (car stts))
                         counts)))
      ((null stts) (reverse counts))))



(defun count-state-section (state start end)
  "(state start end)"
  (do ((i start (1+ i))
       (n 0 (if (eq (get-agent-state i) state)
                (1+ n)
              n)))
      ((= i end) n)))



; added new probabilities and interactions
(defun interact-am (agents p p2 p3)
  "(agents p p2 p3). Have 2 randomly chosen agents interact according to AM."
  (let* ((i (first agents))
         (r (second agents))
         (r-agent (aref *agents* r))
         (i-state (get-agent-state i))
         (r-state (get-agent-state r)))
    (cond 
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
           (not (eq i-state 's1))
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
    
; added new probabilities and interactions
(defun interact-ac (agents p p2 p3)
  "(agents p p2 p3). Have 2 randomly chosen agents interact according to AC."
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

; added new probabilities and interactions
(defun interact-ciu (agents p p2 p3)
  "(agents p p2 p3). Have 2 randomly chosen agents interact according to AC."
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


(defun get-random-agent ()
  "()"
  (random *array-size*))


(defun get-2-random-agents ()
  (let* ((demonstrator (get-random-agent))
         (observer (get-random-agent)))
    (if (eq demonstrator observer)
        (get-2-random-agents)
      (list demonstrator observer))))


(defun consensusp (counts)
  "(counts)"
  (let ((nu (first counts))
        (nx (second counts))
        (ny (third counts)))
    (and (= nu 0)
         (or (zerop nx)
             (zerop ny))
         (not (and (zerop nx)
                   (zerop ny))))))

;;;store lists in file

(defun lists->file (lst file &optional (separator " "))
  "(lst file &optional (separator " 
  (with-open-file
      (output-stream file :direction :output :if-exists :supersede)
    (do ((items (reverse lst) (cdr items)))
        ((null items))
      (let ((sub-item (car items)))
        (if (listp sub-item)
            (print-line sub-item separator output-stream)
          (format output-stream "~a ~%"
            sub-item))))))

(defun print-line (lst &optional (separator " ") output-stream)
  "(lst &optional (separator " 
  (do ((lst lst (cdr lst)))
      ((null lst) (terpri output-stream))
    (princ (car lst) output-stream)
    (princ separator output-stream)))


(defun rotate (lists)
  "(lists)"
  (apply #'mapcar #'list lists))


(defun winners (finishes)
  "(finishes)"
  (do ((fnshs finishes (cdr fnshs))
       (xwins 0 (if (> (first (car fnshs)) (second (car fnshs)))
                    (1+ xwins)
                  xwins))
       (ywins 0 (if (< (first (car fnshs)) (second (car fnshs)))
                    (1+ ywins)
                  ywins)))
      ((null fnshs) (list xwins ywins))))



; added new probabilities and new agents
(defun call-algorithm (algo p p2 p3)
  "(algo p)"
  (case algo
    (am (interact-am (get-2-random-agents) p p2 p3))
    (ac (interact-ac (get-2-random-agents) p p2 p3))
    (ciu (interact-ciu (get-2-random-agents) p p2 p3))
    (otherwise (error "Unknown algorithm"))))

; added new probabilities and new agents, 
(defun run1-saturation (nu nx ny nb ns1 ns2 p p2 p3 replication algo)
  "(nu nx ny nb p replication algo) Run one simulation until consensus, recording counts at each cycle. p, p2, p3 are probabilites for u, s1, s2"
  (initialize-array nu nx ny nb ns1 ns2)
  (make-agent-array nu nx ny nb ns1 ns2)
  (do ((cycles 0 (1+ cycles))
       (ucount (list nu) (cons (count-state 'u) 
                               ucount))
       (xcount (list nx) (cons (count-state 'x) 
                               xcount))
       (ycount (list ny) (cons (count-state 'y) 
                               ycount)))
      ((consensusp (list (first ucount) (first xcount) (first ycount)))
       (progn 
         (lists->file ucount 
                      (concatenate 'string *path* (princ-to-string replication) "ucount"))
         (lists->file xcount 
                      (concatenate 'string *path* (princ-to-string replication) "xcount"))
         (lists->file ycount 
                      (concatenate 'string *path* (princ-to-string replication) "ycount"))
         (list (reverse ucount) 
               (reverse xcount) 
               (reverse ycount) 
               cycles
               (list (first xcount) (first ycount)))))
    (call-algorithm algo p p2 p3)))

; added new probabilities and new agents
(defun run-saturation (n nu nx ny nb ns1 ns2 p p2 p3 algorithm)
  "(n nu nx ny nb p algorithm). Run n simulations until consensus. p, p2, p3 are probabilites for u, s1, s2 "
  (seed-random)
  (setq *path* "/Users/Elias/Documents/models/saturation/"); m/copy/results/")
  (do ((i 0 (1+ i))
       (cycles nil)
       (ucounts nil)
       (xcounts nil)
       (ycounts nil)
       (finishes nil))
      ((= i n) (progn
                 (lists->file 
                  cycles
                  (concatenate 'string *path* "cycles"))
                 (lists->file
                  (reverse (rotate (reverse ucounts)))
                  (concatenate 'string *path* "ucounts"))
                 (lists->file
                  (reverse (rotate (reverse xcounts)))
                  (concatenate 'string *path* "xcounts"))
                 (lists->file
                  (reverse (rotate (reverse ycounts)))
                  (concatenate 'string *path* "ycounts"))
                 (lists->file finishes (concatenate 'string *path* "finishes"))
                 (let ((winners (winners finishes)))
                   (with-open-file
                       (output-stream (concatenate 'string *path* "winners") 
                                      :direction :output)
                     (format output-stream "~a x  ~a y"
                       (first winners) (second winners))))))
    (let ((counts (run1-saturation nu nx ny nb ns1 ns2 p p2 p3 i algorithm)))
      (setf 
       ucounts (cons (first counts)
                     ucounts)
       xcounts (cons (second counts)
                     xcounts)
       ycounts (cons (third counts)
                     ycounts)
       cycles (cons (fourth counts)
                    cycles)
       finishes (cons (fifth counts)
                      finishes)))))

; (run-saturation 20 73 2 0 15 4 6 1.0 0.25 0.75 'am)

; added new probabilities and new agents
(defun run1-to-cycle (nu nx ny nb ns1 ns2 max p p2 p3 replication algo)
  "(nu nx ny nb max p replication algo) Run 1 simulation to max cycle. p2, p3 new probabilities of copying for s1, s2"
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
                   (lists->file xcount 
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

; added new probabilities and new agents

(defun run-to-cycle (n nu nx ny nb ns1 ns2 max p p2 p3 algorithm)
  "(n nu nx ny nb max p algorithm) Run n simulations to max cycle. p2, p3 new probabilities of copying for s1, s2"
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
                  (reverse (rotate (reverse ucounts)))
                  (concatenate 'string *path* "ucounts"))
                 (lists->file
                  (reverse (rotate (reverse xcounts)))
                  (concatenate 'string *path* "xcounts"))
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
       xcounts (cons (second counts)
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

;;;(run-to-cycle 20 74 1 0 25 770 1.0 'ciu) 
;;; (run-to-cycle 20 74 1 0 15 4 6 770 1.0 0.25 0.75 'am)



; added new probabilities and new agents
; never used this function
(defun run1-to-cycle-switch (nu nx ny ns1 ns2 nb max p p2 p3 algo)
  "(nu nx ny nb max p algo)"
  (initialize-array nu nx ny ns1 ns2 nb)
  (make-agent-array nu nx ny ns1 ns2 nb)
  (do ((i 0 (1+ i)))
      ((= i max) (list (count-state-section 'u 75 89)
                       (count-state-section 'x 75 89)
                       (count-state-section 'y 75 89)
                       (count-state-section 's1 75 89)
                       (count-state-section 's2 75 89)
                       (count-state-section 'b 75 89)))
    (call-algorithm algo p p2 p3)))

; added new probabilities and new agents
; never used this function
(defun run-to-cycle-switch (n nu nx ny nb ns1 ns2 max p p2 p3 algorithm)
  "(n nu nx ny nb max p algorithm)"
  (seed-random)
  (setq *path* "/Users/Elias/Documents/models/")
  (do ((i 0 (1+ i))
       (counts nil (cons (run1-to-cycle-switch nu nx ny ns1 ns2 nb max p p2 p3 algorithm)
                         counts)))
      ((= i n) (lists->file 
                counts
                (concatenate 'string *path* "counts")))))


; (run-to-cycle-switch 20 0 75 14 15 4 6 1100 1.0 0.25 0.75 'am)

; added new probabilities and new agents
(defun run1-to-cycle-persist (nu nx ny ns1 ns2 nb max p p2 p3 replication algo)
  "(nu nx ny nb max p replication algo) Run simulation until max cycle, record state counts per cycle, & return state counts at max cycle. p2, p3 new probabilities of copying for s1, s2"
  (initialize-array nu nx ny nb ns1 ns2)
  (make-agent-array nu nx ny nb ns1 ns2)
  (do ((i 0 (1+ i))
       (countx-newbies nil (cons (count-state-section 'x 0 45)
                                 countx-newbies))
       (countx-vets nil (cons (count-state-section 'x 45 75) 
                              countx-vets)))
      ((= i max) (progn 
                   (lists->file 
                    countx-newbies
                    (concatenate 'string *path* (princ-to-string replication) "countx-newbies"))
                   (lists->file 
                    countx-vets
                    (concatenate 'string *path* (princ-to-string replication) "countx-vets"))
                   (list (reverse countx-newbies) (reverse countx-vets))))
    (call-algorithm algo p p2 p3)))

; added new probabilities and new agents
(defun run-to-cycle-persist (n nu nx ny nb ns1 ns2 max p p2 p3 algorithm)
  "(nu nx ny nb max p replication algo) Run n simulations until max cycle, record state counts per cycle, & return state counts at max cycle. p2, p3 new probabilities of copying for s1, s2"
  (seed-random)

  (setq *path* "/Users/Elias/Documents/models/persist/")
  (do ((i 0 (1+ i))
       (countx-newbies nil)
       (countx-vets nil))
      ((= i n) (progn
                 (lists->file 
                  (reverse (rotate (reverse countx-newbies)))
                  (concatenate 'string *path* "countx-newbies"))
                 (lists->file 
                  (reverse (rotate (reverse countx-vets)))
                  (concatenate 'string *path* "countx-vets")))) (let ((countx-both (run1-to-cycle-persist nu nx ns1 ns2 ny nb max p p2 p3 i algorithm)))
      (setf 
       countx-newbies (cons (first countx-both)
                            countx-newbies)
       countx-vets (cons (second countx-both)
                         countx-vets)))))
                                 

;;;(run-to-cycle-persist 20 45 30 0 15 4 6 275 1.0 0.25 0.75 'am)
;;;(run-to-cycle-persist 20 45 30 0 15 4 6 275 1.0 0.25 0.75 'ac)
;;;(run-to-cycle-persist 20 45 30 0 15 4 6 275 1.0 0.25 0.75 'ciu)



