;;;; mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10
;;;; File name    : truth-bias.lisp

;; Proportions:
;; 20% = 8, 30% = 12, 40% = 16, 50% = 20, 60% = 24, 70% = 28 80% = 32

;; load file containing statistics functions
(actr-load "ACT-R:models;lie-detection;package.lisp")
(actr-load "ACT-R:models;lie-detection;lhstats.lisp")

(defparameter *window* nil)
(defparameter *nsubjects* 0)
(defparameter *cues* '("voice" "face" "silence" "self"))
(defparameter *percentages* '((20 50 60 70) (30 40 50 80)))
(defparameter *condition* nil)
(defparameter *t1* nil)
(defparameter *t2* nil)
(defparameter *trial-counter* 1)
(defparameter *truth-response-counter* 0)
(defparameter *experiment-phase* 0 "0 for training, 1 for test")
(defparameter *current-trial* nil)
(defparameter *trials-per-training-block* 40 "Number of trials in a training block")
(defparameter *trials-per-test-block* 20 "Number of trials in a test block")
(defparameter *trial-rt* nil)
(defparameter *cue-percentages* nil)
(defparameter *training-phase* 0)
(defparameter *test-phase* 1)

;; Human data: mean percentage diagnosticity of 'truth, statements for the "easy"
;; and "hard" conditions. 20% 30% 40% 50% 60% 70% 80%

(defparameter *data-easy* "0.219,0.341,0.524,0.670,0.634,0.858,0.754")
(defparameter *data-hard* "0.159,0.091,0.231,0.407,0.494,0.662,0.816")

;; Test phase

(defparameter *test-trials* nil
  "Single list containing 80 (4 x 20) test trials")
(defparameter *test-proportion-counts* nil
  "To hold the array of test proportion counts")
(defparameter *test-truth-response-counts* nil
  "To hold the array of test proportion counts")
(defparameter *final-proportion-of-test-phase-truth-counts* nil
  "To hold the array of test responses")

;; Training phase

(defparameter *training-trials* nil "List of all 4 x 40 training trials")

;; The training proportion responses are the number of times that a particular
;; level of the IV is given the 'truth' response across all participants in the
;; experiment

(defparameter *iv-level-truth-response-counts* nil
  "To hold the array of training responses")
(defparameter *final-proportion-of-iv-level-truth-responses* nil
  "To hold the array of training responses")

;; The training proportion counts is the number of times a particular level of
;; the IV has been seen across all participants in the experiment

(defparameter *training-proportion-counts* nil
  "To hold the array of training proportion counts")

;; The proportion of trials of how often people told the truth when that cue was
;; present.  Note.  Because 't' has a special meaning in Lisp, 'truth' is
;; indicated by the letter 'v' (for 'veracity') instead.

(defparameter *all-test-response-counts* nil)

(defparameter *training-trial-proportions*
  '(("v" "v" "v" "v" "v" "v" "v" "v" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l")
    ("v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l")
    ("v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l")
    ("v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l")
    ("v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l")
    ("v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l" "l")
    ("v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "v" "l" "l" "l" "l" "l" "l" "l" "l")))

;; Functions that shuffle the experiment elements for each participant and
;; create the list of trials.

(defun get-cues ()
  "Return a shuffled list of experiment cues"
  (permute-list *cues*))

(defun get-percentages ()
  "Return a shuffled list of experiment percentages"
  (permute-list (elt *percentages* (random (length *percentages*)))))

(defun get-block (percentage)
  "Given an integer percentage, return a shuffled block of trials"
  (case percentage
    (20 (permute-list (first *training-trial-proportions*)))
    (30 (permute-list (second *training-trial-proportions*)))
    (40 (permute-list (third *training-trial-proportions*)))
    (50 (permute-list (fourth *training-trial-proportions*)))
    (60 (permute-list (fifth *training-trial-proportions*)))
    (70 (permute-list (sixth *training-trial-proportions*)))
    (80 (permute-list (seventh *training-trial-proportions*)))))

(defun get-proportion-array-index (proportion)
  "Return the index for the array collecting the response proportions."
  (case proportion (20 0) (30 1) (40 2) (50 3) (60 4) (70 5) (80 6)))

;; The global variable *training-proportion-counts* contains an array containing
;; the number of times each percentage is seen.  The counts in this array are
;; used to create the mean number of "truth" responses for each percentage at the
;; end of the run.  This function updates the array.

(defun count-training-percentages (percentage-list)
  (loop for per in percentage-list
     do (incf (aref *training-proportion-counts* (get-proportion-array-index per)))))

;; Each training trial is a list of two elements: 1. A cue ("voice" "face"
;; "silence" "self")) 2. A value indicating whether the cue is diagnostic of
;; telling the truth (veridical: "v") or lying ("l").

(defun create-training-trials ()
  "Return a list of 4 x 40 lists of 4 cues with their 40 diagnosticity values"
  (let* ((cues (get-cues))
         (block-percentages (get-percentages))
         (block1 (get-block (first block-percentages)))
         (block2 (get-block (second block-percentages)))
         (block3 (get-block (third block-percentages)))
         (block4 (get-block (fourth block-percentages)))
         (all-blocks (append block1 block2 block3 block4))
         (all-trials nil)
         (counter 0))
    (setf *cue-percentages* (loop
                               for cu in cues
                               for bp in block-percentages
                               collect (list cu bp)))
    (format t "~A~%" *cue-percentages*)
    (count-training-percentages block-percentages)
    (loop for trl in all-blocks do
         (push (list (car cues) trl) all-trials)
         (incf counter)
         (when (= counter *trials-per-training-block*)
           (setf cues (rest cues))
           (setf block-percentages (rest block-percentages))
           (setf counter 0)))
    (reverse all-trials)))

(defun create-test-trials ()
  "Each test trial is a list of two elements: 1. A cue (`voice',
  `face', `silence', `self').  2. The letter `t' indicating that the
  cue is a test.  Returns a shuffled list of 80 cues (2 x 40 trial
  blocks)."
  (let ((test-cues (permute-list
                    (flatten (loop for x from 1 to *trials-per-test-block* collect
                                  (loop for cue in *cues* collect cue)))))
        (final-list nil))
    (loop for cue in test-cues do
         (push (list cue "t") final-list))
    (permute-list final-list)))

(defun get-cue-percentage (cue)
  "Given a cue as input, return the percentage of that cue in the
  current trial."
  (let ((percentage nil))
    (loop for pair in *cue-percentages*
       do (cond ((string= cue (first pair))
                 (setf percentage (second pair)))))
    percentage))

(defun check-response (key)
  "Compare the key response (either `v' or `l') with the response stored with
  the cue.  Return `Correct' if matching, `Wrong' if not."
  (cond ((string= "t" (second *current-trial*))
         (list "Test"))
        ((string= key (second *current-trial*))
         (list "Correct"))
        (t (list "Wrong"))))

(defun present-cue ()
  "Document here."
  (clear-exp-window)
  (add-text-to-exp-window nil (first *current-trial*) :x 125 :y 150)
  (setf *t1* (get-time)))

(defun run-test-phase ()
  "Create a list of cues for the testing phase and then add the context
  information which is a trial fact with the game-difficulty slot being either
  `easy' or `hard'."
  (setf *trial-counter* 1)
  (setf *experiment-phase* 1)
  (setf *test-trials* (create-test-trials))
  (setf *current-trial* (first *test-trials*))
  (setf *test-trials* (rest *test-trials*))
  (goal-focus g2)
  (mod-focus-fct `(test-condition  ,*condition*))
  (present-cue))

;; At the end of a block of 40 trials, print out the total number of 'truth'
;; responses and add the total to the array that collects the totals for each
;; proportion condition.  Reset the counter to zero.

(defun output-block-truth-response ()
  (when (= (mod *trial-counter* *trials-per-training-block*) 0)
    (format t "~d " *truth-response-counter*)
    (setf (aref *iv-level-truth-response-counts*
                (get-proportion-array-index (get-cue-percentage (first *current-trial*))))
          (+ (aref *iv-level-truth-response-counts*
                   (get-proportion-array-index (get-cue-percentage (first *current-trial*))))
             *truth-response-counter*))
    (setf *truth-response-counter* 0)))


(defun present-feedback (feedback)
  "Record the number of 'truth' responses."
  (clear-exp-window)
  (add-text-to-exp-window nil (first feedback) :x 125 :y 150))

;; For each percentage condition, divide the number of truth responses by the
;; number of participants who've seen that percentage.  Check to avoid 'division
;; by zero' error.

(defun compute-training-averages ()
  (dotimes (arr (length *iv-level-truth-response-counts*))
    (if (not (eql (aref *training-proportion-counts* arr) 0))
        (setf (aref *final-proportion-of-iv-level-truth-responses* arr)
              (/ (aref *iv-level-truth-response-counts* arr)
                 (float (* (aref *training-proportion-counts* arr) *trials-per-training-block*)))))))

;; For each percentage condition, divide the number of truth responses by the
;; number of participants who've seen that percentage.  Check to avoid 'division
;; by zero' error.

(defun compute-test-averages ()
  (dotimes (arr (length *test-proportion-counts*))
    (if (not (eql (aref *test-proportion-counts* arr) 0))
        (setf (aref *final-proportion-of-test-phase-truth-counts* arr)
              (/ (aref *test-truth-response-counts* arr)
                 (float (aref *test-proportion-counts* arr)))))))

;; ================================================================
;; ================================================================

(defun print-human-data ()
  (format t "~%  8  12  16  20  24  28  32")
  (cond ((string= "easy" *condition*)
         (format t "~%0.2 0.3 0.4 0.5 0.6 0.7 0.8~%Human-esy,~A" *data-easy*))
        ((string= "hard" *condition*)
         (format t "~%0.2 0.3 0.4 0.5 0.6 0.7 0.8~%Human-hrd,~A" *data-hard*))))

(defun human-data ()
  (cond ((string= "easy" *condition*)
         (list 0.219 0.341 0.524 0.670 0.634 0.858 0.754))
        ((string= "hard" *condition*)
         (list 0.159 0.091 0.231 0.407 0.494 0.662 0.816))))

;; ================================================================
;;
;;
;; ================================================================

(defun update-test-count ()
  (incf (aref *test-proportion-counts*
              (get-proportion-array-index (get-cue-percentage
                                           (first *current-trial*))))))

(defun update-test-truth-counts ()
  (incf (aref *test-truth-response-counts*
              (get-proportion-array-index (get-cue-percentage
                                           (first *current-trial*))))))

;; ================================================================
;;
;;
;; ================================================================

(defun print-array (stream arr)
  (loop for i from 0 to (- (length arr) 1) do
       (format stream ",~,3F" (aref arr i))))

(defun output-data-to-file (&optional (filename "data.csv"))
  "Print model results to file for sending to R plot function."
  (with-open-file (stream
                   (translate-logical-pathname
                    (format nil "ACT-R:models;lie-detection;~a" filename))
                   :direction :output
                   :if-exists :rename-and-delete)
    (format stream "Agent,c1,c2,c3,c4,c5,c6,c7~%")
    (cond ((string= "easy" *condition*)
           (format stream "Human (easy) test,0.219,0.341,0.524,0.67,0.634,0.858,0.754~%")
           (format stream "Model training")
           (print-array stream *final-proportion-of-iv-level-truth-responses*)
           (format stream "~%Model (easy) test")
           (print-array stream *final-proportion-of-test-phase-truth-counts*))
          ((string= "hard" *condition*)
           (format stream "Human (hard) test,0.159,0.091,0.231,0.407,0.494,0.662,0.816~%")
           (format stream "Model training")
           (print-array stream *final-proportion-of-iv-level-truth-responses*)
           (format stream "~%Model (hard) test")
           (print-array stream *final-proportion-of-test-phase-truth-counts*)))
    (force-output stream)))

;; =============================================================================
;; Update the training trials list.  If there are still training trials left
;; then present another stimulus.  When the training trials have come to an end,
;; run the test phase.
;; =============================================================================

(defun determine-next-step ()
  ""
  (when (eql *experiment-phase* *training-phase*)
    (setf *training-trials* (rest *training-trials*))
    (setf *current-trial* (first *training-trials*))
    (when (= (mod *trial-counter* (* *trials-per-training-block* 4)) 0)
      (compute-training-averages)
      (schedule-event-relative 2 'run-test-phase))
    (when *training-trials*
      (incf *trial-counter*)
      (setf *experiment-phase* *training-phase*)
      (schedule-event-relative 2 'present-cue)))
  (when (eql *experiment-phase* *test-phase*)
    (update-test-count)
    (when (not *test-trials*)
      (compute-test-averages)
      (print-human-data)
      (format t "~%Model-trn")
      (print-array t *final-proportion-of-iv-level-truth-responses*)
      (format t "~%Model-tst")
      (print-array t *final-proportion-of-test-phase-truth-counts*)
      (correlation (human-data) *final-proportion-of-test-phase-truth-counts*)
      (mean-deviation (human-data) *final-proportion-of-test-phase-truth-counts*))
    (when *test-trials*
      (setf *current-trial* (first *test-trials*))
      (setf *test-trials* (rest *test-trials*))
      (incf *trial-counter*)
      (setf *experiment-phase* *test-phase*)
      (schedule-event-relative 2 'present-cue))))

;; When the model presses a key response, determine whether the response is
;; correct or not, record the RT, count if it's a 'truth' (veridical "v")
;; response.

(defmethod device-handle-keypress (model key)
  (let ((feedback nil))
    (if (string= (string key) "v")
        (progn
          (incf *truth-response-counter*)
          (when (eql *experiment-phase* *test-phase*)
            (update-test-truth-counts))))
    (when (eql *experiment-phase* *training-phase*)
      (setf feedback (check-response (string key)))
      (present-feedback feedback)
      (output-block-truth-response))
    (setf *t2* (get-time))
    (setf *trial-rt* (- *t2* *t1*))
    (determine-next-step)))

;; This function is used by the production that processes a `Wrong' feedback by
;; placing the opposite response in memory.  As if the model is saying "I
;; responded 'truth' and that's incorrect so the correct response should be
;; 'lie', remember that".

(defun change-response (response)
  (cond ((string= response "l") "v")
        ((string= response "v") "l")))

(defun initiate-experiment ()
  "Run the experiment for one subject."
  (setf *experiment-phase* 0)
  (setf *truth-response-counter* 0)
  (setf *training-trials* (create-training-trials))
  (setf *current-trial* (first *training-trials*))
  (setf *window* (open-exp-window "Experiment" :visible nil))
  (install-device *window*)
  (add-act-r-command "key-press" 'device-handle-keypress)
  (monitor-act-r-command "output-key" "key-press")
  (present-cue)
  (run 2000000)
  (remove-act-r-command-monitor "output-key" "key-press")
  (remove-act-r-command "key-press"))

;; Functions to print out the chunk activations to a file.

(defun name-mapping (chunk slot1 slot2)
  (cond ((and (chunk-slot-value-fct chunk slot1)
              (chunk-slot-value-fct chunk slot2))
         (concatenate 'string (string (chunk-slot-value-fct chunk slot1)) "_"
                      (string (chunk-slot-value-fct chunk slot2))))
        (t chunk)))

(defun print-activation-table (&optional (stream t))
  (let ((names nil)
        (trace (saved-activation-history)))
    (dolist (x trace)
      (dolist (y (cdr x))
        (unless (find y names)
          (push y names))))
    (format stream "Time,~{~s~^,~}~%"
            (mapcar (lambda (x) (name-mapping x 'cue 'diagnosticity)) names))
    (dolist (x trace)
      (format stream "~d" (first x))
      (dolist (y names)
        (if (find y (cdr x))
            (format stream ",~f"
                    (no-output (print-chunk-activation-trace-fct y (first x) t)))
            (format stream ",")))
      (format stream "~%"))))

(defun output-activations-to-file (&optional (filename "acts.csv"))
  (with-open-file (stream
                   (translate-logical-pathname
                    (format nil "ACT-R:models;lie-detection;~a" filename))
                   :direction :output
                   :if-exists :rename-and-delete)
    (print-activation-table stream)))

;; The following functions are created to calculate the confidence intervals for
;; the model's output for each of the proportion conditions.  At the end of the
;; run, the subtract-array function takes the 2D array stored in the variable
;; *all-test-response-counts* (which contains the cumulative counts for each
;; simulated participant) and outputs a new array containing the counts for each
;; participant.  The extract-numbers-into-list function collects the counts for
;; each proportion condition and returns them in a list.

(defun store-array (store index arr)
  ""
  (loop for i from 0 to (1- (length arr)) do
       (setf (aref store index i)
             (aref arr i))))

(defun print-2d-array-as-table (array)
  (loop for i from 0 below (array-dimension array 0)
     do (loop for j from 0 below (array-dimension array 1)
           do (princ (aref array i j))
             (if (= j (1- (array-dimension array 1)))
                 (terpri)
                 (princ #\Space)))))

(defun subtract-array (array)
  (let* ((subjs (array-dimension array 0))
         (conds (array-dimension array 1))
         (new-array (make-array `(,subjs ,conds) :initial-element 0)))
    (loop for i from 0 below subjs
       do (loop for j from 0 below conds
             do (cond ((eq i 0)
                       (setf (aref new-array i j)
                             (aref array i j)))
                      (t
                       (setf (aref new-array i j)
                             (- (aref array i j)
                                (aref array (1- i) j)))))))
    new-array))

(defun extract-numbers-into-list (array loc)
  (let ((val-list nil))
    (loop for arr from 0 to (1- (first (array-dimensions array))) do
         (push (aref array arr loc) val-list))
    (reverse val-list)))

(defun calculate-confidence-intervals (accum-counts-array)
  (let ((counts-array (subtract-array accum-counts-array))
        (counts-list nil)
        (ci nil)
        (mns nil))
    (print-2d-array-as-table counts-array)
    (loop for condition from 0 to (1- (second (array-dimensions counts-array))) do
         (setf counts-list (remove 0 (extract-numbers-into-list counts-array condition)))
         (setf ci (multiple-value-bind (x y)
                      (statistics:normal-mean-ci-on-sequence
                       (mapcar #'(lambda (x) (/ x 20.0)) counts-list) .95)
                    (list x y)))
         (setf mns (statistics:mean (mapcar #'(lambda (x) (/ x 20.0)) counts-list)))
         (loop for cons on counts-list
            do (format t "~a" (car cons))
            when (cdr cons) do (format t " "))
         (format t " ~A ~A~%" mns ci)
         (setf ci nil)
         (setf mns nil))))

(defun runsim (nsubjects test-cond)
  "Run the experiment for n subjects for a particular condition (\"easy\" or \"hard\")."
  (let ((npercentages 7))
    (setf *nsubjects* nsubjects)
    (setf *condition* test-cond)
    (setf *iv-level-truth-response-counts* (make-array npercentages :initial-element 0))
    (setf *final-proportion-of-iv-level-truth-responses*
          (make-array npercentages :initial-element 0))
    (setf *training-proportion-counts*
          (make-array npercentages :initial-element 0))
    (setf *final-proportion-of-test-phase-truth-counts*
          (make-array npercentages :initial-element 0))
    (setf *test-proportion-counts* (make-array npercentages :initial-element 0))
    (setf *test-truth-response-counts* (make-array npercentages :initial-element 0))
    (setf *all-test-response-counts*  (make-array `(,nsubjects ,npercentages) :initial-element 0))
    (dotimes (subj nsubjects)
      (reset)
      (setf *trial-counter* 1)
      (format t "S: ~A (~A) " subj *condition*)
      (initiate-experiment)
      (format t "~%")
      (store-array *all-test-response-counts* subj *test-truth-response-counts*))
;;      (print-2d-array-as-table *all-test-response-counts*))
    (calculate-confidence-intervals *all-test-response-counts*)
    (output-activations-to-file)
   (output-data-to-file)
   (sb-ext:run-program
    "/usr/bin/Rscript"
    (list "/home/david/Dropbox/act-r/models/lie-detection/data-plots.R") :input t)
    ))

;; Return a value somewhere between -1 and 0 where 0 refers to a perfect match
;; (no penalty) and -1 is the maximum penalty (the :mp value) set to 0 for
;; initial testing to verify that test phase results are consistent with
;; training phase

(defun context-similarity (a b)
  (when (and (stringp a) (stringp b)
             (or (and (string-equal a "l") (string-equal b "v"))
                 (and (string-equal b "l") (string-equal a "v"))))
    -0.32))

;;; The ACT-R model code begins here

(clear-all)
(define-model truth-bias

;; Set ACT-R parameters

    (sgp :v nil
         :esc t            ;; enable subsymbolic computation
         :bll 0.5          ;; base level learning
         :mp 0.65          ;; increasing partial matching improves accuracy
         :act nil          ;; activation trace
         :sact t           ;; save activation trace
         :ans 0.21         ;; activation noise (usually between 0.2 and 0.5)
         :rt .4            ;; retrieval threshold
         :ol t             ;; optimised learning
         :trace-detail medium
         :er t             ;; break ties randomly instead of deterministically
         :sim-hook context-similarity)

  (set-visloc-default)

;; Define chunk types.  Note: In ACT-R 7 ISA and chunk-types are not used by the
;; system itself but are only used in the preprocessing of the chunks and
;; productions so generally, their removal does not change the underlying
;; behaviour of the system.  This makes the two
;; 'successful-retrieval->make-response' productions identical.  To avoid the
;; problems this causes, specify a default slot value for each chunk-type.
;; These slots need not be explicitly provided in a production or chunk
;; definition which includes an ISA but the default slot values are
;; automatically added.  Adding (training-trial-goal t) and (test-trial-goal t)
;; solves this problem for my specific productions.

  (chunk-type training-trial-goal state (training-trial-goal t))
  (chunk-type test-trial-goal state test-condition diagnosticity-bias (test-trial-goal t))
  (chunk-type trial "Knowledge of each trial" cue diagnosticity)
  (chunk-type game-fact "Knowledge of context bias" game-difficulty diagnosticity-bias)

;; Note: The declare-buffer-usage command is used to indicate a chunk-type and
;; any slots from that chunk-type that are going to be used with a particular
;; buffer but which are not created by the productions nor set initially in the
;; model definition.  By declaring slots used it will prevent style warnings
;; from the production definitions for the slots provided.

  (declare-buffer-usage goal test-trial-goal test-trial-goal test-condition)

  (define-chunks (wait isa chunk) (attend isa chunk) (retrieve isa chunk) (respond isa chunk)
                 (done isa chunk) (process-test-condition isa chunk) (get-feedback isa chunk)
                 (retrieve-context-information isa chunk) (g1 isa training-trial-goal state wait)
                 (g2 isa test-trial-goal state process-test-condition))

  ;; Add facts about context bias to declarative memory.  If the game is easy,
  ;; players are more likely to not cheat and therefore tell the truth when they
  ;; said they hadn't cheated, whereas if the game is hard, players were more
  ;; likely to cheat and subsequently lie when they said they hadn't cheated.

  (add-dm (easy-fact isa game-fact game-difficulty "easy" diagnosticity-bias "v")
          (hard-fact isa game-fact game-difficulty "hard" diagnosticity-bias "l"))

  (goal-focus g1)

;; Note: In order to ensure these are retrieved, set the number of references to
;; a high value (1000) and the time that they were created to 100s before the
;; model started so that they can have time to be referenced.  This keeps their
;; base level activations high.  This method is required because bll is on.

  (set-base-levels (easy-fact 1000 -100) (hard-fact 1000 -100))

;;; Production rules.  The first element of each production rule name indicates
;;; whether it is specific to a particular experiment phase or applies to both.

(p [both]===find-unattended-text
  "If there's something on the screen, attend to it."
 =goal>
  state wait
 =visual-location>
 ?visual>
  state free
==>
 +visual>
  cmd move-attention
  screen-pos =visual-location
 =goal>
  state attend)

;; Once a cue has been attended to, try to retrieve its associated diagnosticity
;; information.  Note that in the test phase, the information about the cue is
;; not placed into the imaginal buffer.  This is to ensure that the model does
;; not keep learning during the test phase.  Also, in the training production
;; the :mp-value (the mismatch penalty) for the retrieval is set to nil.  This
;; is because partial matching is enabled but we need to ensure that the model
;; does not retrieve the wrong cue in the training phase.

(p [train]===encode-cue->attempt-to-retrieve-diagnosticity
 =goal>
  state attend
  diagnosticity-bias nil
 =visual>
  value =cue
 ?imaginal>
  state free
  buffer empty
==>
 =goal>
  state retrieve
 +imaginal>
  isa trial
  cue =cue
 +retrieval>
  isa trial
 :mp-value nil
  cue =cue)

(p [test]===encode-cue->attempt-to-retrieve-diagnosticity
 =goal>
  state attend
  diagnosticity-bias =diag
 =visual>
  value =cue
==>
 =goal>
  state retrieve
 +retrieval>
  isa trial
  diagnosticity =diag
  cue =cue)

;; If the cue and its diagnosticity have been retrieved, respond with the
;; retrieved diagnosticity.  The only difference between the training and test
;; productions is that the imaginal buffer is used in the training phase.

(p [train]===successful-retrieval->make-response
  =goal>
   test-trial-goal nil
   state retrieve
  =imaginal>
   cue =cue
  =retrieval>
   cue =cue
   diagnosticity =diag
  ?manual>
   state free
==>
  @retrieval>
  =goal>
   state wait
  =imaginal>
   diagnosticity =diag
  +manual>
   cmd press-key
   key =diag)

(p [test]===successful-retrieval->make-response
  =goal>
   test-trial-goal t
   state retrieve
  =retrieval>
   cue =cue
   diagnosticity =diag
  ?manual>
   state free
==>
  @retrieval>
  =goal>
   state wait
  +manual>
   cmd press-key
   key =diag)

;; IF you can't recall a previous diagnosticity THEN just respond "truth"

;; One way of dealing with runaway activation is to not reinforce a chunk when
;; it is retrieved inappropriately.  There are a couple of ways to do that, but
;; the easiest in ACT-R 7 is to use the overwrite action to just remove the
;; chunk from the buffer without merging it back into DM:

(p [train]===incorrect-retrieval->make-response
  =goal>
   test-trial-goal nil
   state retrieve
  =imaginal>
   cue =cue
  =retrieval>
 - cue =cue
   diagnosticity =diag
  ?manual>
   state free
   ==>
  =goal>
   state wait
  =imaginal>
   diagnosticity =diag
  +manual>
   cmd press-key
   key =diag
  @retrieval>)

(p [test]===incorrect-retrieval->make-response
  =goal>
   test-trial-goal t
   state retrieve
  =retrieval>
   cue =cue
   diagnosticity =diag
  ?manual>
   state free
==>
  =goal>
   state wait
  +manual>
   cmd press-key
   key =diag
  @retrieval>)

;; If the retrieval request is unsuccessful then just guess either "truth" or
;; "lie".  The only difference between the training and test productions is that
;; the imaginal buffer is used in the training phase.

(p [train]===fail-to-retrieve->guess-lie
  =goal>
   state retrieve
   test-trial-goal nil
  =imaginal>
   cue =cue
  ?retrieval>
   state error
  ?manual>
   state free
==>
  =goal>
   state wait
  +manual>
   cmd press-key
   key "l"
  =imaginal>
   diagnosticity "l")

(p [train]===fail-to-retrieve->guess-truth
  =goal>
   state retrieve
   test-trial-goal nil
  =imaginal>
   cue =cue
  ?retrieval>
   state error
  ?manual>
   state free
==>
  =goal>
   state wait
  +manual>
   cmd press-key
   key "v"
  =imaginal>
   diagnosticity "v")

(p [test]===fail-to-retrieve->guess-lie
  =goal>
   state retrieve
   test-trial-goal t
  ?retrieval>
   state error
  ?manual>
   state free
==>
  =goal>
   state wait
  +manual>
   cmd press-key
   key "l")

(p [test]===fail-to-retrieve->guess-truth
  =goal>
   state retrieve
   test-trial-goal t
  ?retrieval>
   state error
  ?manual>
   state free
==>
  =goal>
   state wait
  +manual>
   cmd press-key
   key "v")

;; The following two productions process the feedback.  If the feedback is that
;; the response was correct, then the cue and the response are remembered and
;; the trial ends.  If the feedback is that the response was wrong then the
;; production associates the alternative correct response to the cue and
;; remembers that.

(p [train]===receive-negative-feedback
  =goal>
   state attend
  =visual>
   value "Wrong"
  =imaginal>
   cue =cue
   diagnosticity =response
==>
  !bind! =correct-response (change-response =response)
  =imaginal>
   diagnosticity =correct-response
  =goal>
  state wait
  -imaginal>)

(p [train]===receive-positive-feedback
  =goal>
   state attend
  =visual>
   value "Correct"
  =imaginal>
   cue =cue
   diagnosticity =response
==>
  =goal>
   state wait)

;; IF   you're starting the test phase
;; AND  you've been given a test condition in the
;;      form of a game "easy" or game "hard" fact
;; THEN retrieve what that "easy" or "hard" fact
;;      means for the diagnosticity of the cue

;; IF   you're doing the test phase
;; AND  you've retrieved the diagnosticity information
;; THEN keep the cue in mind
;; AND  try to remember if you've experienced the
;;      same cue before

(p [test]===start-test-phase->retrieve-context-information
 =goal>
  state process-test-condition
  test-condition =cond
==>
 =goal>
  state retrieve-context-information
 +retrieval>
  isa game-fact
  game-difficulty =cond)

(p [test]===context-information-retrieved->start-test-trials
 =goal>
  state retrieve-context-information
 =retrieval>
  isa game-fact
  diagnosticity-bias =diag
 ?imaginal>
  state free
  buffer empty
==>
 =goal>
  diagnosticity-bias =diag
  state wait)

)
