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
(defvar *number-of-ticks* 26)
(defvar *beats-per-block* 50)

;; Number of probes that there must be in the experiment
(defvar *number-of-probes* 18)

(setq *beats-per-block* 50)

;; Update the variables to the updated values
(setq *beat-frequency* 440)

;; How long the beats persist
(setq *beat-time* 0.075)

;; How long the beats are apart
(setq *beat-interval* 1.3)

;; How long the participant has to respond
(setq *time-to-respond* 65)

;; How many beats before the probe
(setq *beats-before-probe* 5)

;; Start of the first beat
(setq *onset-time* 0.650)

;; Set the number of probes
(setq *number-of-probes* 18)

;; Threshold for the model to press the key
(setq *number-of-ticks* 27)

(defvar *output-directory* "C:/Dev Projects/RUG BSC AI 2022/Cognitive Modelling Practical/MRT Model/output-wander/") ; location where output files are stored
(setq *output-directory* "C:/Dev Projects/RUG BSC AI 2022/Cognitive Modelling Practical/MRT Model/output-wander/")
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
(defvar *remaining-beats* nil)
(defvar *probe-location* nil)
(defvar *probe-response* nil)
(defvar *probe-list* nil)

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
      (setf *probe-list* nil)

      (mrt-trial)
      (write-results-to-file
        (concatenate 'string "dat" (write-to-string participant))
        participant
        (reverse *all-responses*)
        (reverse *all-rts*)
        (reverse *beat-times*)
        (reverse *probe-list*)))
    
    (reset)
  )
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

  (setf *probe-response* nil)

  ;; Get the response to the 50 beats using key-event-handler
  (add-act-r-command
    "sart-key-press"
    'key-event-handler
    "SART task key press monitor")
  (monitor-act-r-command "output-key" "sart-key-press")

  (setf *trial-response* nil)
  (setf *trial-start* (get-time))
  (setf *trial-rt* nil)
  (setf *trial-done* nil)

  ;; Generate probe's position
  (setf *probe-location* (+ 5 (random (- *beats-per-block* 5)))) ;; 5 to 50?
  (setf *remaining-beats* (- *beats-per-block* *probe-location*))

  (print (concatenate 'string "Probe location: " (write-to-string *probe-location*) " Remaining beats: " (write-to-string *remaining-beats*)))

  ;; Add the probe to the list probe location - 5 times
  (dotimes (i (- *probe-location* 5))
    (push "F" *probe-list*))
  
  ;; Put the required beats for the probe
  (dotimes (i 5)
    (push "T" *probe-list*))
  
  ;; Mark all the remaining beats as false
  (dotimes (i *remaining-beats*)
    (push "F" *probe-list*))

  ;; Schedule the beats
  (dotimes (i *probe-location*)

    ;; Create a variable for the beat times
    (setf *beat* (+ (+ (* *beat-interval* i) *onset-time*) current-time))

    (beat-trial *beat*)
    (push *beat* *beat-times*)
  )

  ;; Create a handler to detect the probe
  (add-act-r-command
    "probe-key-press"
    'probe-handler
    "probe monitor")
  (monitor-act-r-command "output-key" "probe-key-press")

  ;; This creates an interface for the model to interact with
  (install-device '("motor" "keyboard"))

  ;; First, get the responses for the initial beats
  (run-full-time (* *beat-interval* *probe-location*) *visible*)

  ;; Create a window for the probe
  (let ((window (open-exp-window "MRT Experiment"
                               :visible *visible*
                               :width 300
                               :height 300
                               :x 300
                               :y 300)))

    (add-text-to-exp-window window
                            "It's Probe Time!"
                            :width 30
                            :height 30
                            :x 145
                            :y 150)
  
    (install-device window)

    ;; Make the window visible for 1 second
    ;; Change it to last until the probe is pressed later
    (run-full-time 1 *visible*)
    (clear-exp-window)
  
  )

  (run-full-time (* *beat-interval* *remaining-beats*) *visible*)

  (remove-device '("motor" "keyboard"))

  (remove-act-r-command-monitor "output-key" "sart-key-press")
  (remove-act-r-command "sart-key-press")
  (remove-act-r-command-monitor "output-key" "probe-key-press")
  (remove-act-r-command "probe-key-press")
  
  )
    
)

(defun mrt-trial ()

  ;; Repeat n times
  (dotimes (i *number-of-probes*)



    ;; Run the trial
    (multi-beat-trial (/ (get-time) 1000.0))

  )

)

;; Create a handler to detect the probe
;; Schedules the remaining beats
(defun probe-handler (model key)

  (declare (ignore model))
  
  ;; If the probe was not detected and the key pressed was "k"
  (if (and(string= key "k") (not *probe-response*))
      (progn ;; Required to evaluate multiple expressions
        (print "Probe detected")
        (setf *probe-response* "k") ;; Mark that a probe was detected
        (dotimes (i *remaining-beats*)
          (setf *beat* (+ (+ (* *beat-interval* i) *onset-time*) (/ (get-time) 1000)))
          (beat-trial *beat*)
          (push *beat* *beat-times*)))
      nil))

;; Register the model's key presses (ignore the model parameter)
(defun key-event-handler (model key)
  (declare (ignore model))

  (if (string= key "f")
    (progn ; Execute multiple forms
      ;; Prevent race conditions
      (bt:with-lock-held
        (*lock*)
        (setf *trial-rt* (/ (get-time) 1000.0))
        (setf *trial-response* (string key))
        (setf *trial-done* t)

        ;; Add the response and rt to the list for saving
        (push *trial-response* *all-responses*)
        (push *trial-rt* *all-rts*)
      )
    ))

)

;; Write the behavioural results to a file
(defun write-results-to-file (name participant responses rts beat-times probe-locations)

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
    for probe-location in probe-locations
    do (when (string= probe-location "T")
        (format out "~a, ~a, ~a, ~a, ~a, ~a~%" participant (+ (floor trial *beats-per-block*) 1) (+ (mod trial *beats-per-block*) 1) response (* rt 1000) (* beat 1000)))))
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

  ;; Prervent the trace log from being filled with tick increments
  :record-ticks nil

  ;; Affects noisiness of the ticks
  :time-noise 0.004

)

(chunk-type beginning label)
(chunk-type goal state)
(chunk-type subgoal step pressed threshold)
(chunk-type number num1 num2)

; New chunk type for mind wandering
(chunk-type memory type)

(add-dm
  (start isa chunk)

  ; Add some chunks for mind wandering

  (startgoal isa beginning label start)
  (attend isa goal state attend)

  ; New chunk for implementing mind wandering
  (wander isa goal state wander)

  (counting isa subgoal step counting pressed nil threshold 27)
  (increase-tick isa subgoal step increase-tick pressed nil threshold 27)
  (decrease-tick isa subgoal step decrease-tick pressed nil threshold 27)
  (remember isa subgoal step remember pressed nil threshold 27)

  ; Create chunks for mind wandering
  (dattend isa memory type dattend)
  (d1 isa memory type d1)
  (d2 isa memory type d2)
  (d3 isa memory type d3)
  (d4 isa memory type d4)

  ;; Chunks for numbers
  (one isa number num1 1 num2 2)
  (two isa number num1 2 num2 3)
  (three isa number num1 3 num2 4)
  (four isa number num1 4 num2 5)
  (five isa number num1 5 num2 6)
  (six isa number num1 6 num2 7)
  (seven isa number num1 7 num2 8)
  (eight isa number num1 8 num2 9)
  (nine isa number num1 9 num2 10)
  (ten isa number num1 10 num2 11)
  (eleven isa number num1 11 num2 12)
  (twelve isa number num1 12 num2 13)
  (thirteen isa number num1 13 num2 14)
  (fourteen isa number num1 14 num2 15)
  (fifteen isa number num1 15 num2 16)
  (sixteen isa number num1 16 num2 17)
  (seventeen isa number num1 17 num2 18)
  (eighteen isa number num1 18 num2 19)
  (nineteen isa number num1 19 num2 20)
  (twenty isa number num1 20 num2 21)
  (twentyone isa number num1 21 num2 22)
  (twentytwo isa number num1 22 num2 23)
  (twentythree isa number num1 23 num2 24)
  (twentyfour isa number num1 24 num2 25)
  (twentyfive isa number num1 25 num2 26)
  (twentysix isa number num1 26 num2 27)
  (twentyseven isa number num1 27 num2 28)
  (twentyeight isa number num1 28 num2 29)
  (twentynine isa number num1 29 num2 30)
  (thirty isa number num1 30 num2 31)
  (thirtyone isa number num1 31 num2 32)
  (thirtytwo isa number num1 32 num2 33)
  (thirtythree isa number num1 33 num2 34)
  (thirtyfour isa number num1 34 num2 35)

)

(set-base-levels

; Attend and wander have equal base activation
  (attend      10000  -10000)
  (wander      10000  -10000)

  ; Mind wandering chunks have equal base activation
  ; They should not take too much time to be retrieved
  (dattend    1000  1000)
  (d1         1000  1000)
  (d2         1000  1000)
  (d3         1000  1000)
  (d4         1000  1000)

  ;; We expect these to be well practiced so they should be retrieved quickly
  ;; I wish arrays and loops existed in act r
  (one        10000  10000)
  (two        10000  10000)
  (three        10000  10000)
  (four        10000  10000)
  (five        10000  10000)
  (six        10000  10000)
  (seven        10000  10000)
  (eight        10000  10000)
  (nine        10000  10000)
  (ten        10000  10000)
  (eleven        10000  10000)
  (twelve        10000  10000)
  (thirteen        10000  10000)
  (fourteen        10000  10000)
  (fifteen        10000  10000)
  (sixteen        10000  10000)
  (seventeen        10000  10000)
  (eighteen        10000  10000)
  (nineteen        10000  10000)
  (twenty        10000  10000)
  (twentyone        10000  10000)
  (twentytwo        10000  10000)
  (twentythree      10000  10000)
  (twentyfour       10000  10000)
  (twentyfive       10000  10000)
  (twentysix        10000  10000)
  (twentyseven      10000  10000)
  (twentyeight      10000  10000)
  (twentynine      10000  10000)
  (thirty      10000  10000)
  (thirtyone      10000  10000)
  (thirtytwo      10000  10000)
  (thirtythree      10000  10000) 
  (thirtyfour      10000  10000)
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
  +goal>
    isa     subgoal
    step    counting
    pressed nil
    threshold 27 ;; Default value based on theoretical estimate
  +temporal>
    isa time
)

(p check-current-goal
  =goal>
    isa     subgoal
    step    counting
    threshold =threshold
  ?aural-location>
    buffer  empty  
  ?retrieval>
    state         free
  - state         error
  =temporal>
    isa time
    ticks =ticks
  < ticks =threshold
==>
  !output! (The tick counter was =ticks when the model checked the current goal)
  +retrieval>
    isa           goal
  - state         nil
)

;; Production that is fired when the threshold is reached
;; Executed all the time
(p on-rhythm-response
  =temporal>
    isa time
    >= ticks =threshold
    ticks =ticks
  ?manual>
    state     free
  =goal>
    threshold =threshold
  ==>
  !output! (the button was pressed at =ticks according to the model's rhythm and the threshold was =threshold)
  +manual>
    isa       punch
    hand      left
    finger    index
  
  ;; Resets threshold
  +temporal>
    isa time
  =goal>
    pressed t
)

;; Production used to reset the threshold which allows the model to correct itself
;; Only used when attending
(p hear-sound-early-attend
  =goal>
    isa     subgoal
    step    counting
    pressed t
    threshold =threshold
  =aural-location>
    isa        audio-event
    location   external
  =temporal>
    isa time
    ticks =ticks
  ?retrieval>
    state         free
  ==>
  !output! (the tick counter was =ticks when model heard sound and already pressed the button during attend)
  ;; Reset ticks
  +temporal>
    isa time
  =goal>
    isa     subgoal
    step    increase-tick
    pressed nil
  +retrieval>
    isa           number
    num1          =threshold
  
)

;; Same production as above but the model does not learn the ticks
(p hear-sound-early-wander
  =goal>
    isa     subgoal
    step    remember
    pressed t
    threshold =threshold
  =aural-location>
    isa        audio-event
    location   external
  =temporal>
    isa time
    ticks =ticks
  ==>
  !output! (the tick counter was =ticks when model heard sound and already pressed the button during wander)
  ;; Reset ticks
  +temporal>
    isa time
  =goal>
    pressed nil
  
)

;; Production used to reset the threshold when the model is too late
(p hear-sound-and-press-late-attend
  =goal>
    isa     subgoal
    step    counting
    pressed nil
    threshold =threshold
  =aural-location>
    isa        audio-event
    location   external
  =temporal>
    isa time
    ticks =ticks
  ?retrieval>
    state         free
  ?manual>
    state         free
  ==>
  !output! (the tick counter was =ticks when the model was late and did not press the button during attend and the threshold was =threshold)
  +manual>
    isa       punch
    hand      left
    finger    index
  ;; Reset ticks
  +temporal>
    isa time
  =goal>
    isa     subgoal
    step    decrease-tick
    pressed nil
  +retrieval>
    isa           number
    num2          =threshold
)

;; Same as above but the model does not learn the ticks
(p hear-sound-and-press-late-wander
  =goal>
    isa     subgoal
    step    remember
    pressed nil
    threshold =threshold
  =aural-location>
    isa        audio-event
    location   external
  =temporal>
    isa time
    ticks =ticks
  ?manual>
    state         free
  ==>
  !output! (the tick counter was =ticks when the model was late and did not press the button during wander and the threshold was =threshold)
  +manual>
    isa       punch
    hand      left
    finger    index
  ;; Reset ticks
  +temporal>
    isa time
)

(p increase-threshold
  =goal>
    isa     subgoal
    step    increase-tick
    pressed nil
  =retrieval>
    isa           number
    num2          =threshold
==>
  !output! (The model increased the threshold to =threshold ticks)

  ;; Increase the threshold
  =goal>
    isa     subgoal
    step    counting
    pressed nil
    threshold =threshold
)

(p decrease-threshold
  =goal>
    isa     subgoal
    step    decrease-tick
    pressed nil
  =retrieval>
    isa           number
    num1          =threshold
==>
    !output! (The model decreased the threshold to =threshold ticks)
    ;; Decrease the threshold
    =goal>
      isa     subgoal
      step    counting
      pressed nil
      threshold =threshold
)

(goal-focus startgoal)

; Productions for mind wandering are located here
; Mind wandering can only start from the counting state
(p begin-wander
  =goal>
    pressed =pressed
  ?goal>
    state     free
  =retrieval>
    isa           goal
    state         wander
  ?retrieval>
    state         free
  - state         error
==>

  ;; We assume that the model still has knowledge of whether or not it has pressed the button
  =goal>
		isa			subgoal
		step		remember
    pressed =pressed
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
  ?goal>
    state     free
  =retrieval>
    isa       memory
    type      dattend

==>
  ;; Strengthen the dattend chunk so the model is less likely to mind wander
  -retrieval>
  =goal>
		isa			subgoal
		step		counting
)

; Production that is fired when the model retrieves a random memory
(p wander-loop
  =goal>
    isa    subgoal
    step   remember
    threshold =threshold
  ?aural-location>
    buffer  empty 
  =temporal>
    isa time
  < ticks =threshold 
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

;; Productions for reacting to the probe
(p react-to-probe
	?visual>
		scene-change T
    state free
	=visual-location>
	?manual>
		state 		free
  ?goal>
    state         free
  =goal>
  ?temporal>
    state        free
==>
  +manual>
    isa       punch
    hand      right
    finger    middle
	-visual-location>
	-visual>
  
  !output! (The model reacted to the probe)

  ;; Force the model to pay attention after the probe
  =goal>
    isa     subgoal
    step    counting
    pressed nil
  
  ;; Reset internal clock
  +temporal>
    isa time

)

)
