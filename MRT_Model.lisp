;; Metronome Response Task (MRT) experiment
;; Created for Cognitive Modelling Practicals, 2023


;;===================;;
;;  Experiment code  ;;
;;===================;;

(defvar *response*)

;; Variables for the MRT experiment
(defvar *beat-frequency* 440)
(defvar *beat-time* 0.2)
(defvar *beat-interval* 0.4)
(defvar *time-to-respond* 2)
(defvar *beats-before-probe* 5)
(defvar *onset-time* 0.2)
(defvar *number-of-ticks 26)

;; Number of probes that there must be in the experiment
(defvar *number-of-probes* 10)

;; Update the variables to the updated values
(setq *beat-frequency* 440)

;; How long the beats persist
(setq *beat-time* 0.075)

;; How long the beats are apart
(setq *beat-interval* 1.3)

;; How long the participant has to respond
(setq *time-to-respond* 7)

;; How many beats before the probe
(setq *beats-before-probe* 5)

;; Start of the first beat
(setq *onset-time* 0.650)

;; Set the number of probes
(setq *number-of-probes* 10)

;; Threshold for the model to press the key
(setq *number-of-ticks* 27)

(defvar *output-directory* "C:/Dev Projects/RUG BSC AI 2022/Cognitive Modelling Practical/MRT Model/output") ; location where output files are stored
(defvar *trace-to-file-only* nil) ; whether the model trace should only be saved to file and not appear in terminal
(defvar *trace-file-name* "sart-trace") ; name of file in which the trace is stored

(defvar *terminal-stream* *standard-output*) ; necessary for stream management

(defvar *visible* nil) ; visibility of the experiment window

;; Global variables for data storage
(defvar *stimuli* nil)
(defvar *trial-response* nil)
(defvar *trial-start* nil)
(defvar *trial-rt* nil)
(defvar *trial-done* nil)
(defvar *all-responses* nil)
(defvar *all-rts* nil)
(defvar *beat-times* nil)
(defvar *beat* nil)

;; Parallellism management
(defvar *lock* (bt:make-lock))

;; Do MRT experiment n times, save trace to output file
(defun mrt-n (n)
  (with-open-file
    (*file-stream*
      (ensure-directories-exist
        (merge-pathnames
          (make-pathname :name *trace-file-name* :type "txt")
          *output-directory*))
      :direction :output :if-does-not-exist :create :if-exists :supersede)

  (if *trace-to-file-only*
    ; If true, direct standard output only file
    (setf *standard-output* *file-stream*)
    ; Else, direct standard output to terminal and file
    (setf *standard-output*
      (make-broadcast-stream *terminal-stream* *file-stream*)))

  ; Direct ACT-R output to the stream contained within *standard-output*
  (echo-act-r-output)

  (setf *visible* nil)
  (format t "Running ~a model participants~%" n)
  (dotimes (i n)
    (let ((participant (1+ i)))
      (format t "Run ~a...~%" participant)

      ;; Reset the global variables
      (setf *all-responses* nil)
      (setf *all-rts* nil)
      (setf *beat-times* nil)

      (mrt-trial)
      (write-results-to-file
        (concatenate 'string "dat" (write-to-string participant))
        participant
        (reverse *all-responses*)
        (reverse *all-rts*)
        (reverse *beat-times*))))
  (format t "Done~%")

  ; We will close *file-stream* now, so make sure *standard-output*
  ; no longer points to it
  (setf *standard-output* *terminal-stream*)
  ; We also have to make sure ACT-R knows about the new value of
  ; *standard-output*
  (echo-act-r-output)
  )
)

(defun beat-trial (onset)

    ;; Start beat at the specified time with a delay
    (new-tone-sound *beat-frequency* *beat-time* onset)

)

(defun multi-beat-trial (current-time)

  (let ()

  ;; Get the response to the 5 beats using key-event-handler
  (add-act-r-command
    "sart-key-press"
    'key-event-handler
    "SART task key press monitor")
  (monitor-act-r-command "output-key" "sart-key-press")

  (setf *trial-response* nil)
  (setf *trial-start* (get-time))
  (setf *trial-rt* nil)
  (setf *trial-done* nil)

  ;; Schedule the beats
  (dotimes (i *beats-before-probe*)

    ;; Create a variable for the beat times
    (setf *beat* (+ (* *beat-interval* i) *onset-time*))

    (beat-trial (+ *beat* current-time))
    (push *beat* *beat-times*)
  )

  ;; This creates an interface for the model to interact with
  (install-device '("motor" "keyboard"))
  (run-full-time *time-to-respond* *visible*)
  (remove-device '("motor" "keyboard"))

  (remove-act-r-command-monitor "output-key" "sart-key-press")
  (remove-act-r-command "sart-key-press")
  
  )
    
)

(defun mrt-trial ()

  ;; Repeat n times
  (dotimes (i *number-of-probes*)

    ;; Run the trial
    (multi-beat-trial (/ (get-time) 1000.0))

  )

)

;; Register the model's key presses (ignore the model parameter)
(defun key-event-handler (model key)
  (declare (ignore model))

  ; Prevent race conditions
  (bt:with-lock-held
  (*lock*)
    (setf *trial-rt* (/ (- (get-time) *trial-start*) 1000.0))
    (setf *trial-response* (string key))
    (setf *trial-done* t)

    ;; Add the response and rt to the list for saving
    (push *trial-response* *all-responses*)
    (push *trial-rt* *all-rts*)
  )

)

;; Write the behavioural results to a file
(defun write-results-to-file (name participant responses rts beat-times)

  (with-open-file
    (out
      (ensure-directories-exist
        (merge-pathnames
          (make-pathname :name name :type "csv")
          *output-directory*))
      :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format out "participant, trial, beat, response, rt, beat-time~%")
    (loop
      for trial from 0
      for response in responses
      for rt in rts
      for beat in beat-times
      do (format out "~a, ~a, ~a, ~a, ~a, ~a~%" participant (+ (floor trial 5) 1) (+ (mod trial 5) 1) response rt beat)))
)



;;===================;;
;;    Model code     ;;
;;===================;;

(clear-all)

(define-model sart

;; Model parameters
(sgp :v t ; main trace detail
  :act low ; activation trace detail
  :sact t ; include activation trace in main trace

  :show-focus t ; show where the model is looking
  :esc t ; enable sub-symbolic level
  :rt -5 ; retrieval threshold
  :bll 0.5 ; base-level learning
  :ans 0.2 ;activation noise

  ; Spreading activation. Marieke's model utilizes this to simulate how real memories work
  :mas 1

  ; number of finsts for declarative memory
  ; Based on research
  :declarative-num-finsts 4
)

(chunk-type beginning label)
(chunk-type goal state)
(chunk-type subgoal step)
(chunk-type srmapping stimulus hand)

; New chunk type for mind wandering
(chunk-type memory type)

(add-dm
  (start isa chunk)

  ; Add some chunks for mind wandering

  (press-on-O isa srmapping stimulus "O" hand left)
  (withhold-on-Q isa srmapping stimulus "Q" hand nil)
  (startgoal isa beginning label start)
  (attend isa goal state attend)

  ; New chunk for implementing mind wandering
  ;; Let's focus on the attend state first
  ;; (wander isa goal state wander)

  (identify isa subgoal step identify)
  (get-response isa subgoal step get-response)
  (make-response isa subgoal step make-response)
  (remember isa subgoal step remember)

  ; Create chunks for mind wandering
  (dattend isa memory type dattend)
  (d1 isa memory type d1)
  (d2 isa memory type d2)
  (d3 isa memory type d3)
  (d4 isa memory type d4)
  (d5 isa memory type d5)
  (d6 isa memory type d6)
  (d7 isa memory type d7)
  (d8 isa memory type d8)
  (d9 isa memory type d4)
  (d10 isa memory type d10)
  (d11 isa memory type d11)
  (d12 isa memory type d12)
  (d13 isa memory type d13)
  (d14 isa memory type d14)
  (d15 isa memory type d15)
  (d16 isa memory type d16)
  (d17 isa memory type d17)
  (d18 isa memory type d18)
  (d19 isa memory type d19)
  (d20 isa memory type d20)
  (d21 isa memory type d21)
  (d22 isa memory type d22)
  (d23 isa memory type d23)
  (d24 isa memory type d24)
  (d25 isa memory type d25)
  (d26 isa memory type d26)
  (d27 isa memory type d27)
  (d28 isa memory type d28)
  (d29 isa memory type d29)
  (d30 isa memory type d30)

)

(set-base-levels

; Attend and wander have equal base activation
  (attend      10000  -10000)
  ;; (wander      10000  -10000)

  (press-on-O    10000  -10000)
  (withhold-on-Q  10000  -10000)
)

(p start-task
  =goal>
    isa      beginning
    label    start
  ?retrieval>
    buffer   empty
    state    free
  - state    error
==>
  +retrieval>
    isa      goal
    state    attend
  -goal>
)

(p check-current-goal
  =retrieval>
    isa           goal
    state         attend
  ?retrieval>
    state         free
  - state         error
  ?goal>
    buffer        empty
  ?visual>
  - scene-change  T
  =temporal>
    isa time
    ticks =ticks
==>
  !output! (Current tick counter is =ticks)
  =retrieval>
    state         nil ; clear retrieval buffer without strengthening chunk
  -retrieval>
  +retrieval>
    isa           goal
  - state         nil
)

;; Production that is fired when the threshold is reached
;; Currently executed all the time
(p on-rhythm
  =temporal>
    isa time
    >= ticks 26
    ticks =ticks
  ==>
  !output! (the button was pressed at =ticks)
  ?manual>
    state     free
  +manual>
    isa       punch
    hand      left
    finger    index
  
  ;; Resets threshold
  +temporal>
    isa time
)

;; Production used to reset the threshold which allows the model to correct itself
(p hear-sound
  =aural-location>
    isa        audio-event
    location   external
  =temporal>
    isa time
    ticks =ticks
  ==>
  !output! (the tick counter was =ticks when model heard sound)
  ;; Reset ticks
  +temporal>
    isa time
)

;; Production that is fired when the model first hears a sound
(p start-counting
  =aural-location>
    isa        audio-event
    location   external
  ==>
  !output! (Model Started Counting)
  +temporal>
    isa time
  +manual>
    isa       punch
    hand      left
    finger    index
)

;; Production used to reset the threshold when the model is too late
(p hear-sound-and-press
  =aural-location>
    isa        audio-event
    location   external
  =temporal>
    isa time
    ticks =ticks
    >= ticks 18
  ==>
  !output! (the tick counter was =ticks when the model was late)
  +manual>
    isa       punch
    hand      left
    finger    index
  ;; Reset ticks
  +temporal>
    isa time
)

(p identify-stimulus
  ?goal>
    buffer      empty
  =retrieval>
    isa         goal
    state       attend
  =visual-location>
  ?visual>
    state       free
==>
  +visual>
    isa         move-attention
    screen-pos  =visual-location
  +goal>
    isa         subgoal
    step        get-response
  =retrieval>
    state       nil ; clear retrieval buffer without strengthening chunk
  -retrieval>
)

(p retrieve-response
  =goal>
    isa       subgoal
    step      get-response
  =visual>
    isa       text
    value     =letter
  ?visual>
    state     free
  ?retrieval>
    state     free
==>
  +retrieval>
    isa       srmapping
    stimulus  =letter
  +goal>
    isa       subgoal
    step      make-response
  +visual>
    isa       clear-scene-change
)

(p respond-if-O
  =goal>
    isa       subgoal
    step      make-response
  =retrieval>
    isa       srmapping
    stimulus  =letter
    hand      =hand
  ?manual>
    state     free
==>
  +manual>
    isa       punch
    hand      =hand
    finger    index
  -goal>
  -visual-location>
  -visual>
  +retrieval>
    isa       goal
  - state     nil
)

(p do-not-respond-if-Q
  =goal>
    isa       subgoal
    step      make-response
  =retrieval>
    isa       srmapping
    stimulus  =letter
    hand      nil
  ?manual>
    state     free
==>
  -goal>
  -visual-location>
  -visual>
  +retrieval>
    isa       goal
  - state     nil
)

(goal-focus startgoal)

; Productions for mind wandering are located here
(p begin-wander
  =retrieval>
    isa           goal
    state         wander
  ?retrieval>
    state         free
  - state         error
  ?goal>
    buffer        empty
==>
  +goal>
		isa			subgoal
		step		remember
  =retrieval>
    state         nil ; clear retrieval buffer without strengthening chunk
  -retrieval>
  +retrieval>
    isa 		memory
	-	type			nil

    ; This enables the model to only think of a memory that it has not recently remembered
    ; It is limited by the number of declarative finsts
    :recently-retrieved nil
)

; Production that reminds the model to go back to the task
(p back-to-task
  =goal>
    isa       subgoal
    step      remember
  =retrieval>
    isa       memory
    type      dattend
  ?retrieval>
    state     free
  - state     error
==>
  -retrieval>
  +retrieval>
    isa       goal
    state     attend

    ; Resets the declarative finsts
    ;; :recently-retrieved reset

  -goal>
)

; Production that is fired when the model retrieves a random memory
(p wander-loop
  =goal>
    isa       subgoal
    step      remember
  =retrieval>
    isa           memory
  - type          dattend
  ?retrieval>
    state         free
  - state         error
==>
  =retrieval>
    state         nil ; clear retrieval buffer without strengthening chunk
  -retrieval>
  +retrieval>
    isa 		memory
	-	type			nil
    :recently-retrieved nil
)

; Default response that is executed when the model is mind wandering
(p lazy-response	
	=goal>
		isa			subgoal
		step		remember
	?visual>
		scene-change T
    state free
	=visual-location>
	?manual>
		state 		free
  ?retrieval>
    state         free
  - state         error
==>
  +manual>
    isa       punch
    hand      left
    finger    index
	-visual-location>
	-visual>
  -retrieval>
  +retrieval>
    isa 		memory
	-	type			nil
    :recently-retrieved nil
  
)

)
