;;;file: copy4.cl
;;;purpose: compare 3 copying algorithms: AM, AC, & CIU
;;;programmer: Tom Shultz
;;;started: 20 aug 2015
;;;current: 26 jan 2016
;;;new in copy4: byzantine agents who stay in state b & thus never copy
;;; add conditions to interact-am, ac, & ciu to keep b agents in state b
;;; include b agents in initialize-array & make-agent-array
;;; add parameter for nb to all top & 2nd-level functions
;;; use run-to-cycle for conformity-bias simulations

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

;;;random numbers

(defun call-random (m n)
  "(m n)"
"Call (random n) m times. Used by seed-random."
  (do ((i 0 (1+ i)))
      ((= i m) nil)
    (if (zerop n) 
        (random 1)
      (random n))))

(defun seed-random ()
  "Seed random from the last 4 digits of time." 
"Useful for generating unique random sequences."
  (let* ((time (get-internal-real-time))
         (n (multiple-value-bind (x y) (floor time 100) (cadr (list x y))))
         (hundreds (multiple-value-bind (x y) (floor time 100) (car (list x y))))
         (m (multiple-value-bind (x y) (floor hundreds 100) (cadr (list x y)))))
    (call-random m n)))

;;;make agents

(defun initialize-array (nu nx ny nb)
  "(nu nx ny nb)"
"Set *array-size* to sum of numbers of agents."
"Initialize *agents* array with nil elements."
  (setf *array-size* (+ nu nx ny nb)
    *agents* (make-array *array-size* :initial-element nil)))

;;;(initialize-array 4 1 0)

(defun make-1-agent (i s)
  "(i s)"
"Make or change state of agent i to s."
  (setf (aref *agents* i) 
    (make-agent :id i
                :state s)))

;;;(make-1-agent 2 'u)

(defun make-agent-array (nu nx ny ns nb)
  "(nu nx ny nb)"
"Make agent array."
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
    ((= i (+ nu nx ny ns)))
    (make-1-agent i 's))
  (do ((i (+ nu nx ny ns) (1+ i)))
      ((= i (+ nu nx ny ns nb)))
    (make-1-agent i 'b)))
       
;;;(initialize-array 4 2 0)
;;;(make-agent-array 4 2 0)
;;;*agents*

;;;count states

(defun get-agent-state (a)
  "(a)"
"Return state of agent at a."
  (let ((agent (aref *agents* a)))
    (agent-state agent)))

;;;(get-agent-state 3)

(defun count-state (state)
  "(state)"
"Count state across agents."
  (do ((i 0 (1+ i))
       (n 0 (if (eq (get-agent-state i) state)
                (1+ n)
              n)))
      ((= i *array-size*) n)))

;;;(count-state 'u)
;;;(count-state 'x)

(defun count-states (states)
  "(states)"
"Count each state across agents."
  (do ((stts states (cdr stts))
       (counts nil (cons (count-state (car stts))
                         counts)))
      ((null stts) (reverse counts))))

;;;(count-states '(u x y))

(defun count-state-section (state start end)
  "(state start end)"
"Count state for array section from start up to but not including end index."
  (do ((i start (1+ i))
       (n 0 (if (eq (get-agent-state i) state)
                (1+ n)
              n)))
      ((= i end) n)))

;;;(count-state-section 'x 4 5)
;;;(count-state-section 'x 4 6)

;;;interact

(defun interact-am (agents p p2)
  "(agents p)"
"Initiator i & recipient r interact in AM."
"Decided i converts undecided u."
"Decided i confuses decided r."
  (let* ((i (first agents))
         (r (second agents))
         (r-agent (aref *agents* r))
         (i-state (get-agent-state i))
         (r-state (get-agent-state r)))
    (cond 
     ((and (eq r-state 'u)
           (not (eq i-state 'u))
           (not (eq i-state 'b))
           (< (random 1.0) p))
      (setf (agent-state r-agent) i-state))
     ((and (not (eq i-state 'u))
           (not (eq i-state 'b))
           (not (eq r-state 'u))
           (not (eq r-state 'b))
           (not (eq i-state r-state))
           (< (random 1.0) p))
      (setf (agent-state r-agent) 'u))
     ; if r's state is slow and i is not undecided/byzantine, p2 chance of changing
     ((and 
        (eq r-state 's)
        (not (eq i-state 'u))
        (not (eq i-state 'b))
        (< (random 1.0) p2)))
     (t nil))

    ))
    

(defun interact-ac (agents p)
  "(agents p)"
"Recipient imitates decided initiator in AC."
  (let* ((i (first agents))
         (r (second agents))
         (r-agent (aref *agents* r))
         (i-state (get-agent-state i))
         (r-state (get-agent-state r)))
    (if (and (not (eq i-state 'u))
             (not (eq i-state 'b))
             (not (eq r-state 'b))
             (not (eq i-state r-state))
             (< (random 1.0) p))
        (setf (agent-state r-agent) i-state))))

;;;(interact-ac '(4 0) 1.0)

(defun interact-ciu (agents p)
  "(agents p)"
"Undecided recipient imitates decided initiator in CIU."
  (let* ((i (first agents))
         (r (second agents))
         (r-agent (aref *agents* r))
         (i-state (get-agent-state i))
         (r-state (get-agent-state r)))
    (if (and (not (eq i-state 'u))
             (not (eq i-state 'b))
             (eq r-state 'u)
             (< (random 1.0) p))
        (setf (agent-state r-agent) i-state))))

;;;(interact-ciu '(4 0) 1.0)

;;;get initiator & responder

(defun get-random-agent ()
  "()"
"Get 1 random agent."
  (random *array-size*))

;;;(get-random-agent)

(defun get-2-random-agents ()
  (let* ((demonstrator (get-random-agent))
         (observer (get-random-agent)))
    (if (eq demonstrator observer)
        (get-2-random-agents)
      (list demonstrator observer))))

;;;(get-2-random-agents)

;;;Check for consensus

(defun consensusp (counts)
  "(counts)"
"Consensus on x or y?"
"Ignores b."
  (let ((nu (first counts))
        (nx (second counts))
        (ny (third counts)))
    (and (= nu 0)
         (or (zerop nx)
             (zerop ny))
         (not (and (zerop nx)
                   (zerop ny))))))

;;;(consensusp '(1 1 2))
;;;(consensusp '(0 0 0))
;;;(consensusp '(0 1 0))
;;;(consensusp '(0 0 1))

;;;store lists in file

(defun lists->file (lst file &optional (separator " "))
  "(lst file &optional (separator " "))"
"Save reverse lst in file. Items in flat lst are printed 1 per line."
"Separator is used to separate items on a line for embedded lst."
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
  "(lst &optional (separator " ") output-stream)"
"Print each item in list on a line separated by separator. "
"Then go to new line."
  (do ((lst lst (cdr lst)))
      ((null lst) (terpri output-stream))
    (princ (car lst) output-stream)
    (princ separator output-stream)))

;;; rotate list-of-lists

(defun rotate (lists)
  "(lists)"
"Rotate list of lists."
  (apply #'mapcar #'list lists))

;;;(rotate '((1 2 3)
;;;          (4 5 6)))

(defun winners (finishes)
  "(finishes)"
"Compute winners from finishing counts of x & y."
  (do ((fnshs finishes (cdr fnshs))
       (xwins 0 (if (> (first (car fnshs)) (second (car fnshs)))
                    (1+ xwins)
                  xwins))
       (ywins 0 (if (< (first (car fnshs)) (second (car fnshs)))
                    (1+ ywins)
                  ywins)))
      ((null fnshs) (list xwins ywins))))

;;;(winners '((0 20) (20 0) (20 0) (20 0) (20 0) (0 20) (20 0) (20 0) (20 0) (20 0) (20 0) (20 0) (20 0)
;;;           (20 0) (20 0) (20 0) (20 0) (20 0) (20 0) (20 0)))


;;;call algorithm

(defun call-algorithm (algo p)
  "(algo p)"
"Call a particular algorithm."
"p is probability of copying."
"Algorithm is 'am, 'ac, or 'ciu."
  (case algo
    (am (interact-am (get-2-random-agents) p))
    (ac (interact-ac (get-2-random-agents) p))
    (ciu (interact-ciu (get-2-random-agents) p))
    (otherwise (error "Unknown algorithm"))))

;;;run simulation

(defun run1-saturation (nu nx ny nb p replication algo)
  "(nu nx ny nb p replication algo)"
"Run simulation until consensus, recording counts at each cycle."
"p is probability of copying."
"Algorithm is 'am, 'ac, or 'ciu."
  (initialize-array nu nx ny nb)
  (make-agent-array nu nx ny nb)
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
    (call-algorithm algo p)))

(defun run-saturation (n nu nx ny nb p algorithm)
  "(n nu nx ny nb p algorithm)"
"Run n simulations, returning list of cycles required for consensus."
"p is probability of copying."
"Algorithm is 'am, 'ac, or 'ciu."
  (seed-random)
  (setq *path* "/Users/tom1/Documents/models m/copy/results/")
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
    (let ((counts (run1-saturation nu nx ny nb p i algorithm)))
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

;;;(run-saturation 20 73 2 0 25 1.0 'am) run for 1100 cycles
;;;(run-saturation 20 73 2 0 25 1.0 'ac) run for 1100 cycles
;;;(run-saturation 20 73 2 0 25 1.0 'ciu) run for 1100 cycles

(defun run1-to-cycle (nu nx ny nb max p replication algo)
  "(nu nx ny nb max p replication algo)"
"Run simulation until max cycle, record state counts per cycle, & return state counts at max cycle."
"p is probability of copying."
"Algorithm is 'am, 'ac, or 'ciu."
  (initialize-array nu nx ny nb)
  (make-agent-array nu nx ny nb)
  (do ((i 0 (1+ i))
       (ucount (list nu) (cons (count-state 'u) 
                               ucount))
       (xcount (list nx) (cons (count-state 'x) 
                               xcount))
       (ycount (list ny) (cons (count-state 'y) 
                               ycount)))
      ((= i max) (progn 
                   (lists->file ucount 
                                (concatenate 'string *path* (princ-to-string replication) "ucount"))
                   (lists->file xcount 
                                (concatenate 'string *path* (princ-to-string replication) "xcount"))
                   (lists->file ycount 
                                (concatenate 'string *path* (princ-to-string replication) "ycount"))
                   (list (reverse ucount) 
                         (reverse xcount) 
                         (reverse ycount) 
                         (list (first xcount) (first ycount)))))
    (call-algorithm algo p)))

(defun run-to-cycle (n nu nx ny nb max p algorithm)
  "(n nu nx ny nb max p algorithm)"
"Run n simulations to max cycle, recording cycle lists of state counts for each simulation "
"& simulation x cycle table of state counts at end."
"p is probability of copying."
"Algorithm is 'am, 'ac, or 'ciu."
  (seed-random)
  (setq *path* "/Users/tom1/Documents/models m/copy/results/")
  (do ((i 0 (1+ i))
       (ucounts nil)
       (xcounts nil)
       (ycounts nil)
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
                 (let ((winners (winners finishes)))
                   (with-open-file
                       (output-stream (concatenate 'string *path* "winners") 
                                      :direction :output)
                     (format output-stream "~a x  ~a y"
                       (first winners) (second winners))))))
    (let ((counts (run1-to-cycle nu nx ny nb max p i algorithm)))
      (setf 
       ucounts (cons (first counts)
                     ucounts)
       xcounts (cons (second counts)
                     xcounts)
       ycounts (cons (third counts)
                     ycounts)
       finishes (cons (fourth counts)
                      finishes)))))

;;;treatment conditions
;;;(run-to-cycle 20 73 2 0 25 1100 1.0 'am)
;;;(run-to-cycle 20 73 2 0 25 1100 1.0 'ac)
;;;(run-to-cycle 20 73 2 0 25 1100 1.0 'ciu)

;;;control conditions
;;;(run-to-cycle 20 74 1 0 25 770 1.0 'am) 
;;;(run-to-cycle 20 74 1 0 25 770 1.0 'ac) 
;;;(run-to-cycle 20 74 1 0 25 770 1.0 'ciu) 

;;;conformity conditions 14 or 12 x
;;;(run-to-cycle 20 60 14 1 25 825 1.0 'am)
;;;(run-to-cycle 20 62 12 1 25 825 1.0 'ac)
;;;(run-to-cycle 20 60 14 1 25 825 1.0 'ciu)

;;;conformity conditions 10 x
;;;(run-to-cycle 20 64 10 1 25 825 1.0 'am)
;;;(run-to-cycle 20 64 10 1 25 825 1.0 'ac)
;;;(run-to-cycle 20 64 10 1 25 825 1.0 'ciu)

(defun run1-to-cycle-switch (nu nx ny nb max p algo)
  "(nu nx ny nb max p algo)"
"Run n simulations to max cycle, recording final xcounts for newbie y section."
"p is probability of copying."
"Algorithm is 'am, 'ac, or 'ciu."
  (initialize-array nu nx ny nb)
  (make-agent-array nu nx ny nb)
  (do ((i 0 (1+ i)))
      ((= i max) (list (count-state-section 'u 75 89)
                       (count-state-section 'x 75 89)
                       (count-state-section 'y 75 89)
                       (count-state-section 'b 75 89)))
    (call-algorithm algo p)))

(defun run-to-cycle-switch (n nu nx ny nb max p algorithm)
  "(n nu nx ny nb max p algorithm)"
"Run n simulations to max cycle, recording xcounts at ends for newbie y section."
"p is probability of copying."
"Algorithm is 'am, 'ac, or 'ciu."
  (seed-random)
  (setq *path* "/Users/tom15/Documents/models m/copy/results/")
  (do ((i 0 (1+ i))
       (counts nil (cons (run1-to-cycle-switch nu nx ny nb max p algorithm)
                         counts)))
      ((= i n) (lists->file 
                counts
                (concatenate 'string *path* "counts")))))

;;;(run-to-cycle-switch 20 0 75 14 25 1100 1.0 'am)
;;;(run-to-cycle-switch 20 0 75 14 25 1100 1.0 'ac)
;;;(run-to-cycle-switch 20 0 75 14 25 1100 1.0 'ciu)

;;;(run-to-cycle-switch 20 0 75 14 25 550 1.0 'am)
;;;(run-to-cycle-switch 20 0 75 14 25 550 1.0 'ac)
;;;(run-to-cycle-switch 20 0 75 14 25 550 1.0 'ciu)

;;;persist over years

(defun run1-to-cycle-persist (nu nx ny nb max p replication algo)
  "(nu nx ny nb max p replication algo)"
"Run simulation until max cycle, record state counts per cycle, & return state counts at max cycle."
"p is probability of copying."
"Algorithm is 'am, 'ac, or 'ciu."
  (initialize-array nu nx ny nb)
  (make-agent-array nu nx ny nb)
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
    (call-algorithm algo p)))

(defun run-to-cycle-persist (n nu nx ny nb max p algorithm)
  "(n nu nx ny nb max p algorithm)"
"Run n simulations to max cycle, recording cycle lists of nx for newbies & vets "
"for each simulation & simulation x cycle table of nx at end for newbies & vets."
"p is probability of copying. Algorithm is 'am, 'ac, or 'ciu."
  (seed-random)
  (setq *path* "/Users/tom1/Documents/models m/copy/results/")
  (do ((i 0 (1+ i))
       (countx-newbies nil)
       (countx-vets nil))
      ((= i n) (progn
                 (lists->file 
                  (reverse (rotate (reverse countx-newbies)))
                  (concatenate 'string *path* "countx-newbies"))
                 (lists->file 
                  (reverse (rotate (reverse countx-vets)))
                  (concatenate 'string *path* "countx-vets")))) (let ((countx-both (run1-to-cycle-persist nu nx ny nb max p i algorithm)))
      (setf 
       countx-newbies (cons (first countx-both)
                            countx-newbies)
       countx-vets (cons (second countx-both)
                         countx-vets)))))
                                 
;;;(run-to-cycle-persist 20 45 30 0 25 275 1.0 'am)
;;;(run-to-cycle-persist 20 45 30 0 25 275 1.0 'ac)
;;;(run-to-cycle-persist 20 45 30 0 25 275 1.0 'ciu)


