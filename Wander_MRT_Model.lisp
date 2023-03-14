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
  ;; :time-noise 0.005

)

(chunk-type beginning label)
(chunk-type goal state)
(chunk-type subgoal step pressed)

; New chunk type for mind wandering
(chunk-type memory type)

(add-dm
  (start isa chunk)

  ; Add some chunks for mind wandering

  (startgoal isa beginning label start)
  (attend isa goal state attend)

  ; New chunk for implementing mind wandering
  (wander isa goal state wander)

  (counting isa subgoal step counting pressed nil)
  (remember isa subgoal step remember)

  ; Create chunks for mind wandering
  (dattend isa memory type dattend)
  (d1 isa memory type d1)
  (d2 isa memory type d2)
  (d3 isa memory type d3)
  (d4 isa memory type d4)
  (d5 isa memory type d5)

)

(set-base-levels

; Attend and wander have equal base activation
  (attend      10000  -10000)
  (wander      10000  -10000)
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
  +temporal>
    isa time
)

(p check-current-goal
  =goal>
    isa     subgoal
    step    counting
  ?retrieval>
    state         free
  - state         error
  =temporal>
    isa time
    ticks =ticks
==>
  !output! (the tick counter was =ticks when model checked current goal)
  +retrieval>
    isa           goal
  - state         nil
)

;; Production that is fired when the threshold is reached
;; Executed all the time
(p on-rhythm-response
  =temporal>
    isa time
    >= ticks 27
    ticks =ticks
  ?manual>
    state     free
  =goal>
  ==>
  !output! (the button was pressed at =ticks according to the model's rhythm)
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
(p hear-sound-early
  =goal>
    pressed t
  =aural-location>
    isa        audio-event
    location   external
  =temporal>
    isa time
    ticks =ticks
  ==>
  !output! (the tick counter was =ticks when model heard sound and already pressed the button)
  ;; Reset ticks
  +temporal>
    isa time
  =goal>
    pressed nil
)

;; Production used to reset the threshold when the model is too late
(p hear-sound-and-press
  =goal>
    pressed nil
  =aural-location>
    isa        audio-event
    location   external
  =temporal>
    isa time
    ticks =ticks
  ==>
  !output! (the tick counter was =ticks when the model was late and did not press the button)
  +manual>
    isa       punch
    hand      left
    finger    index
  ;; Reset ticks
  +temporal>
    isa time
)

(goal-focus startgoal)

; Productions for mind wandering are located here
; Mind wandering can only start from the counting state
(p begin-wander
  =goal>
    isa    subgoal
    step   counting
    pressed =pressed
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
    isa       subgoal
    step      remember
  =retrieval>
    isa       memory
    type      dattend
  ?retrieval>
    state     free
  - state     error

  ;; Adding this production attends to the chunk in the aural location buffer
  ;; It ensures that it won't interfere with future productions
  ;; There should always be a chunk in the aural location buffer since the default decay value is 3 seconds
  =aural-location>
    isa        audio-event
    location   external
==>
  ;; Strengthen the dattend chunk so the model is less likely to mind wander
  -retrieval>

  ;; By setting the pressed variable to t, we force the model to wait for the beat
  =goal>
		isa			subgoal
		step		counting
    pressed t
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
