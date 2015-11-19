(ns livecoding.core
  (:use [overtone.live]
        [overtone.synth.stringed]
        [overtone.at-at :only [every]]
        ))


;;(def modsamp (sample (freesound-path 281867)))

(defonce buf-0 (buffer 16))
(defonce buf-1 (buffer 16))
(defonce buf-2 (buffer 16))
(defonce buf-3 (buffer 16))

;;For melody
(defonce buf-4 (buffer 32))

(defonce root-trg-bus (control-bus)) ;; global metronome pulse
(defonce root-cnt-bus (control-bus)) ;; global metronome count
(defonce beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
(defonce beat-cnt-bus (control-bus)) ;; beat count

(defonce meter-cnt-bus16 (control-bus)) ;; not used yet
(defonce meter-cnt-bus32 (control-bus)) ;; needed for a pattern that requires two measures

(defonce piano-note-bus (control-bus)) ;; connects to the 32 note bus

(def BEAT-FRACTION "Number of global pulses per beat" 30)

;; Now we get a little close to the sounds. Here's four nice sounding
;; ;; samples from Freesound.org
(def kick-s (sample (freesound-path 777)))
(def click-s (sample (freesound-path 406)))
(def tick-s (sample (freesound-path 254316)))
(def subby-s (sample (freesound-path 25649)))

;; Here we design synths that will drive our pulse busses.
;;inst is the wrapper around the synth... any consequences?
(defsynth root-trg [rate 140]
  (out:kr root-trg-bus (impulse:kr rate)))

(defsynth root-cnt []
  (out:kr root-cnt-bus (pulse-count:kr (in:kr root-trg-bus))))

(defsynth beat-trg [div BEAT-FRACTION]
  (out:kr beat-trg-bus (pulse-divider (in:kr root-trg-bus) div))  )

(defsynth beat-cnt []
  (out:kr beat-cnt-bus (pulse-count (in:kr beat-trg-bus))))

(defsynth meter-cnt [meter-cnt-bus 0 div 8]
  (out:kr meter-cnt-bus (mod (in:kr beat-cnt-bus) div)))

;; Here's a synth for playing back the samples with a bit of modulation
;; to keep things interesting.
(defsynth mono-sequencer
  "Plays a single channel audio buffer."
  [buf 0 rate 1 out-bus 0 beat-num 0 sequencer 0 amp 1]
  (let [cnt      (in:kr beat-cnt-bus)
        beat-trg (in:kr beat-trg-bus)
        bar-trg  (and (buf-rd:kr 1 sequencer cnt)
                      (= beat-num (mod cnt 16))
                      beat-trg)
        vol      (set-reset-ff bar-trg)]
    (out
     out-bus (* vol
                amp
                (pan2
                 (rlpf
                  (scaled-play-buf 1 buf rate bar-trg)
                  (demand bar-trg 0 (dbrown 200 20000 50 INF))
                  (lin-lin:kr (lf-tri:kr 0.01) -1 1 0.1 0.9)))))))

(defsynth note-sequencer
  "Plays a sequence of notes to a bus"
  [buf 0 meter-count-bus 0 out-bus 1]
  (out out-bus (buf-rd:kr 1 buf (in:kr meter-count-bus) 1 0)))

(stop)
;; Now, let's start up all the synths:
(do
  (def r-cnt (root-cnt))
  (def b-cnt (beat-cnt))
  (def b-trg (beat-trg))
  (def r-trg (root-trg))

  (def kicks (doall
              (for [x (range 16)]
                (mono-sequencer :buf kick-s :beat-num x :sequencer buf-0))))

  (def clicks (doall
               (for [x (range 16)]
                 (mono-sequencer :buf click-s :beat-num x :sequencer buf-1))))

  (def ticks (doall
              (for [x (range 16)]
                (mono-sequencer :buf tick-s :beat-num x :sequencer buf-2))))

  (def subbies (doall
                (for [x (range 16)]
                  (mono-sequencer :buf subby-s :beat-num x :sequencer buf-3))))

  ;;note sequences
  (def dub-note-seq (note-sequencer buf-4 meter-cnt-bus32 piano-note-bus))
  )


;; An empty palatte to play with:
(do
  (buffer-write! buf-0 [0 1 0 0 1 0 0 1 0 0 1 0 0 0 0 0])  ;; kick
  (buffer-write! buf-1 [0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0])  ;; click
  (buffer-write! buf-2 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])  ;; tick
  (buffer-write! buf-3 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])) ;; subby


;;instruments and little demos

;;low end
(definst low [freq 30 numharm 10 length 10 volume 0.8]
  (* volume
     (line:kr 0.9 1 length FREE)
     (blip freq numharm)))

(low 30 :length 60)
(ctl low :length 10)
(ctl low :freq 30)
(ctl low :freq 40)
(ctl low :freq 48)
(ctl low :freq 32)
(ctl low :freq 30)


;; string... very orbital
(definst plucked-string [note 60 amp 0.8 dur 2 decay 30 coef 0.3 gate 1]
  (let [freq (midicps note)
        noize (* 0.8 (white-noise))
        dly (/ 1.0 freq)
        plk (pluck noize gate dly dly decay coef)
        dist (distort plk)
        filt (rlpf dist (* 12 freq) 0.6)
        clp (clip2 filt 0.8)
        reverb (free-verb clp 0.4 0.8 0.2)]
    (* amp (env-gen (perc 0.0001 dur) :action FREE) reverb)))

(plucked-string)

;;Beep... very telepop musik
(definst beep [note 60]
  (let [src (sin-osc (midicps note))
        env (env-gen (perc 0.1 1.5) :action FREE)]
    (* src env)))

(beep)

(beep (note "F#4"))
(beep (note "A#4"))
(beep (note "C#5"))
(beep (note "D#5"))


;; Controls for firing sounds

;;Chords
(defn play-chord [instrument a-chord ]
  (doseq [note a-chord] (instrument note)))

(chord :C3 :minor)


(play-chord plucked-string (chord :C3 :minor))
(play-chord beep (chord :C4 :minor))

;;careful, sounds hideous
(play-chord low (chord :C4 :minor))

;;Melodies
;;currently doesn't take the instrument as an argument...
(defn play-beep
  [time notes sep]
  (let [note (first notes)]
    (when note
      (at time (beep note)))

    (let [next-time (+ time sep)]
      (apply-at next-time play-beep [next-time (rest notes) sep]))))

(defn play-low
  [time notes sep]
  (let [note (first notes)]
    (when note
      (at time (low note)))

    (let [next-time (+ time sep)]
      (apply-at next-time play-low [next-time (rest notes) sep]))))

(defn play-string
  [time notes sep]
  (let [note (first notes)]
    (when note
      (at time (plucked-string note)))

    (let [next-time (+ time sep)]
      (apply-at next-time play-string [next-time (rest notes) sep]))))


(def wurlitzer1 (map note ["F#5" "A#4" "C#5" "D#5" "F#5" "A#4" "C#5" "D#5" "F#5" "A#4" "C#5" "D#5" "A#4" "G#4" "F#4" "D#4"]))
(def wurlitzer2 (map note ["F#4" "G#4" "A#4" "C#5" 0 "A#4" 0 "C#5" "D#5" "C#5" "A#4" "G#4" 0 "D#4"]))

(play-beep (now) wurlitzer2 364)

;;careful, sounds hideous
(play-low (now) wurlitzer2 364)

(play-string (now) wurlitzer2 364)

(stop)

;;what's map doing? taking in a collection of strings representing notes, and returning of collection of integers that represent those notes in MIDI

(map note ["F#5" "A#4" "C#5" "D#5" "F#5" "A#4" "C#5" "D#5" "F#5" "A#4" "C#5" "D#5" "C#5" "A#4" "G#4" "F#4"])


;;Play melody once
(play-beep (now) wurlitzer2 364)

;;Repeat melody infinitely
(play-beep (now) (cycle wurlitzer1) 374)

;;Repeat melody n times
(play-beep (now) (flatten (repeat 5 wurlitzer1)) 374)

;;Play two melodies simultenously
(let [t (+ (now))]
  (play-beep t (cycle wurlitzer1) 374)
  (play-beep t (cycle wurlitzer2) 374))

;;Play two meoldies with a delay
(let [t (+ 2000 (now))]
  (play-beep t (cycle wurlitzer1) 374)
  (play-beep t (cycle wurlitzer1) 374))

;;Play two meoldies at different tempos
(let [t (+ 2000 (now))]
  (play-string t (cycle wurlitzer1) 374)
  (play-string t (cycle wurlitzer1) 380))

(stop)
;;great way to get really interesting syncopated rhythms
;;I wonder if there's a way to lock in a repeat any particaluar phased measure


;;Live controls
;;Now trying to get a melody
;;Doesn't work with the buffer sequencer yet...
(buffer-write! buf-4 [60 60 0 65 65 67 70 67])
(buffer-write! buf-4 (repeatedly 8 #(choose (map (partial + 24) [60 60 0 65 65 67 70 67]))))

;;Playing wurlitzer

(def wurlitzer1 (map note ["F#5" "A#4" "C#5" "D#5" "F#5" "A#4" "C#5" "D#5" "F#5" "A#4" "C#5" "D#5" "A#4" "G#4" "F#4" "D#4"]))

(def wurlitzer2 (map note ["F#4" "G#4" "A#4" "C#5" 0 "A#4" 0 "C#5" "D#5" "C#5" "A#4" "G#4" 0 "D#4" 0 0]))

(def wurlitzer4 (map note ["F#4" "G#4" "A#4" "C#5" 0 0 0 "C#5" "D#5" "C#5" "A#4" "G#4" 0 "D#4" 0 0]))

(def wurlitzer5 (map note ["F#5" "G#5" "A#5" "C#6" 0 "A#5" 0 "D#6" "C#6" "A#5" "G#5" "F#5" 0 "D#5" 0 0]))

;; F A C D G

(def wurlitzer3 (map note ["A#5" "D#5" "F#5" "G#5" "A#5" "D#5" "F#5" "G#5" "A#5" "D#5" "F#5" "G#5" "F#5" "D#5" "C#5" "A#4"]))

(play-beep (now) wurlitzer3 364)


(beep (note "D#6"))

;;how to play a sequence of rifs. Riff 4, 2, 4, 4
(play-beep (now) (lazy-cat wurlitzer4 wurlitzer2 wurlitzer4 wurlitzer4) 374)
(play-beep (now) wurlitzer2 374)



;;now the drums

;;Every time we stop all sounds, we have to restart the drum track synths

;; Now, let's start up all the synths:
(do
  (def r-cnt (root-cnt))
  (def b-cnt (beat-cnt))
  (def b-trg (beat-trg))
  (def r-trg (root-trg))

  (def kicks (doall
              (for [x (range 16)]
                (mono-sequencer :buf kick-s :beat-num x :sequencer buf-0))))

  (def clicks (doall
               (for [x (range 16)]
                 (mono-sequencer :buf click-s :beat-num x :sequencer buf-1))))

  (def ticks (doall
              (for [x (range 16)]
                (mono-sequencer :buf tick-s :beat-num x :sequencer buf-2))))

  (def subbies (doall
                (for [x (range 16)]
                  (mono-sequencer :buf subby-s :beat-num x :sequencer buf-3))))

  ;;note sequences
  (def dub-note-seq (note-sequencer buf-4 meter-cnt-bus32 piano-note-bus))
  )


;;Drum tracks
(buffer-write! buf-0 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])
(buffer-write! buf-0 [0 1 0 0 1 0 0 1 0 0 1 0 0 0 0 0])
(buffer-write! buf-0 [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])

(buffer-write! buf-1 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])

(buffer-write! buf-2 [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1])
(buffer-write! buf-2 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])

(buffer-write! buf-3 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])
(buffer-write! buf-3 [1 2 3 4 5 6 0 0 0 0 0 0 0 0 0 0])


;;play some low over it
(low 30 :length 60)
(ctl low :length 10)
(ctl low :freq 30)
(ctl low :freq 40)
(ctl low :freq 48)
(ctl low :freq 32)
(ctl low :freq 30)



(stop)

;;start urlitzer
(play-beep (now) wurlitzer5 364)

(defsynth root-trg [rate 160.5]
  (out:kr root-trg-bus (impulse:kr rate)))


;;intro

(do
  (volume 0.7)
  ;;(recording-start "~/Desktop/wurlitzer.wav") comment this back in to record
  (buffer-write! buf-0 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])  ;; kick
  (buffer-write! buf-1 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])  ;; click
  (buffer-write! buf-2 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])  ;; tick
  (buffer-write! buf-3 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])  ;; subby

  (let [t (+ (now))]
    (play-beep t (flatten (repeat 8 wurlitzer1 )) 374)
    (play-beep t (lazy-cat wurlitzer1 wurlitzer1 wurlitzer3 wurlitzer3 wurlitzer3 wurlitzer3 wurlitzer1 wurlitzer1) 374))

  (def r-cnt (root-cnt))
  (def b-cnt (beat-cnt))
  (def b-trg (beat-trg))
  (def r-trg (root-trg))

  (def kicks (doall
              (for [x (range 16)]
                (mono-sequencer :buf kick-s :beat-num x :sequencer buf-0))))

  (def clicks (doall
               (for [x (range 16)]
                 (mono-sequencer :buf click-s :beat-num x :sequencer buf-1))))

  (def ticks (doall
              (for [x (range 16)]
                (mono-sequencer :buf tick-s :beat-num x :sequencer buf-2))))

  (def subbies (doall
                (for [x (range 16)]
                  (mono-sequencer :buf subby-s :beat-num x :sequencer buf-3))))


  ;;note sequences
  (def dub-note-seq (note-sequencer buf-4 meter-cnt-bus32 piano-note-bus)))



;;Intro drums; eval after 2 measures
(buffer-write! buf-0 [0 0 1 0 0 1 0 0 1 0 0 0 0 0 0 1])  ;; kick
(buffer-write! buf-1 [0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0])  ;; click

;;SILENCE; eval after 6 measures
(buffer-write! buf-0 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])  ;; kick
(buffer-write! buf-1 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])
(buffer-write! buf-3 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])


;; eval after 8 measures
(do
  ;;second
  (buffer-write! buf-0 [0 1 1 0 0 0 0 1 1 0 1 1 0 0 0 1])  ;; kick
  (buffer-write! buf-1 [0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0])  ;; click
  (buffer-write! buf-3 [0 1 1 0 1 1 0 0 0 1 1 0 1 1 0 0])

  (let [t (+ (now))]
    (play-beep t (lazy-cat wurlitzer4 wurlitzer2 wurlitzer4 wurlitzer4) 374)
    (play-beep t (lazy-cat wurlitzer5 wurlitzer5 wurlitzer5 wurlitzer5) 374)))

;;third
(do
  (buffer-write! buf-0 [0 1 1 0 0 0 0 1 1 0 1 1 0 0 0 1])  ;; kick
  (buffer-write! buf-1 [0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0])  ;; click
  (buffer-write! buf-3 [0 1 1 0 1 1 0 0 0 1 1 0 1 1 0 0])
  (buffer-write! buf-2 [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1])

  (let [t (+ (now))]
    (play-beep t (flatten (repeat 8 wurlitzer1 )) 374)
    (play-beep t (lazy-cat wurlitzer1 wurlitzer3 wurlitzer3 wurlitzer3 wurlitzer3 wurlitzer3 wurlitzer3 wurlitzer3 wurlitzer3) 374)))

;;ends with clicks
(buffer-write! buf-0 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])  ;; kick
(buffer-write! buf-1 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])  ;; click
(buffer-write! buf-3 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])


;;todo figure out the thread pool problem in at-at
;;so I can use at-at abstractions for sequencing

(defn volume-down [step]
  (if
    (< (- (volume) step) 0.0001)
    (volume 0)
    (volume (- (volume) step))))

(defn volume-up [step]
  (volume (+ (volume) step)))

(volume-down 0.05)
(volume-up 0.05)
(volume 0.8)


;;end clicks, end recording
(do
  (buffer-write! buf-2 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])
  (recording-stop)
  (stop)
  )


(do
  (buffer-write! buf-0 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])  ;; kick
  (buffer-write! buf-1 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])  ;; click
  (buffer-write! buf-2 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])  ;; tick
  (buffer-write! buf-3 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))

;start
(buffer-write! buf-0 [0 0 1 0 0 1 0 0 1 0 0 0 0 0 0 1])  ;; kick
(buffer-write! buf-1 [0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0])  ;; click

;;SILENCE
(buffer-write! buf-0 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])  ;; kick
(buffer-write! buf-1 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])  ;; click

;;later
(buffer-write! buf-2 [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1])
(buffer-write! buf-3 [0 0 0 1 1 1 0 0 0 1 1 1 1 1 0 0])


(do
  (buffer-write! buf-0 [0 0 1 0 0 1 0 0 1 0 0 0 0 0 0 1])  ;; kick
  (buffer-write! buf-1 [0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0])  ;; click
  (buffer-write! buf-2 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])  ;; tick
  (buffer-write! buf-3 [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])) ;; subby

(recording-stop)








(defsynth monotron
  "Korg Monotron from website diagram:
   http://korg.com/services/products/monotron/monotron_Block_diagram.jpg."
  [note     60            ; midi note value
   volume   0.7           ; gain of the output
   mod_pitch_not_cutoff 1 ; use 0 or 1 only to select LFO pitch or cutoff modification
   pitch    0.0           ; frequency of the VCO
   rate     4.0           ; frequency of the LFO
   int      1.0           ; intensity of the LFO
   cutoff   1000.0        ; cutoff frequency of the VCF
   peak     0.5           ; VCF peak control (resonance)
   pan      0             ; stereo panning
   ]
  (let [note_freq       (midicps note)
        pitch_mod_coef  mod_pitch_not_cutoff
        cutoff_mod_coef (- 1 mod_pitch_not_cutoff)
        LFO             (* int (saw rate))
        VCO             (saw (+ note_freq pitch (* pitch_mod_coef LFO)))
        vcf_freq        (+ cutoff (* cutoff_mod_coef LFO) note_freq)
        VCF             (moog-ff VCO vcf_freq peak)
        ]
    (out 0 (pan2 (* volume VCF) pan))))

;; ======================================================================
;; create an instance of the synth
(def N0 (monotron 40 0.8 1 0.0 2.5 350.0 800.0 3.0))

(ctl N0 :note   50)               ;; midi note value: 0 to 127
(ctl N0 :note   60)
(ctl N0 :volume 0.7)              ;; gain of the output: 0.0 to 1.0
(ctl N0 :mod_pitch_not_cutoff 0)  ;; use 0 or 1 only to select LFO pitch or cutoff modification
(ctl N0 :mod_pitch_not_cutoff 1)
(ctl N0 :pitch  10.0)             ;; this + note is frequency of the VCO
(ctl N0 :pitch  0.0)
(ctl N0 :rate   1.5)              ;; frequency of the LFO
(ctl N0 :rate   16.0)              ;; so dirty
(ctl N0 :rate   2.5)
(ctl N0 :int    800.0)           ;; intensity of the LFO
(ctl N0 :int    350.0)
(ctl N0 :cutoff 600.0)           ;; cutoff frequency of the VCF
(ctl N0 :cutoff 800.0)
(ctl N0 :peak   0.5)              ;; VCF peak control (resonance) 0.0 to 4.0
(ctl N0 :peak   0)

(stop)





;;thx emulator
(defsynth thx [gate 1 amp 1 out-bus 0]
  (let [target-pitches (map midi->hz [77 74 72 70 65 62 60 58 53 50 46 34 26 22 14 10])
        r-freq         (env-gen:kr (envelope [1 1 0.007 10] [8 4 2] [0 -4 1] 2) gate)
        amp-env        (env-gen:kr (envelope [0 0.07 0.21 0] [8 4 2] [0 1 1] 2) gate :action FREE)
        mk-noise       (fn [ug-osc]
                         (mix (map #(pan2 (ug-osc (+ (* r-freq (+ 230 (* 100 (lf-noise2:kr 1.3))))
                                                     (env-gen:kr (envelope [0 0 %] [8 6] [0 -3]))))
                                          (lf-noise2:kr 5))
                                   target-pitches)))
        saws           (mk-noise saw)
        sins           (mk-noise sin-osc)
        snd            (+ (* saws amp-env) (* sins amp-env))]
    (out out-bus
         (* amp (g-verb snd 9 0.7 0)))))

;; play the instrument:
(def t (thx :amp 2))
;; make it fly away
(ctl t :gate 0)



;; ======================================================================
;; make some dub step

(defn later [t]
    (+ (now) t))

(defcgen kick-drum
  "basic synthesised kick drum"
  [bpm {:default 120 :doc "tempo of kick in beats per minute"}
   pattern {:default [1 0] :doc "sequence pattern of beats"}]
  (:ar
   (let [kickenv (decay (t2a (demand (impulse:kr (/ bpm 30)) 0 (dseq pattern INF))) 0.7)
         kick (* (* kickenv 7) (sin-osc (+ 40 (* kickenv kickenv kickenv 200))))]
     (clip2 kick 3))))


(defcgen snare-drum
  "basic synthesised snare drum"
  [bpm {:default 120 :doc "tempo of snare in beats per minute"}]
  (:ar
   (let [snare (* 3 (pink-noise) (apply + (* (decay (impulse (/ bpm 240) 0.5) [0.4 2]) [1 0.05])))
         snare (+ snare (bpf (* 4 snare) 2000))]
     (clip2 snare 1))))


(defcgen wobble
  "wobble an input src"
  [src {:doc "input source"}
   wobble-factor {:doc "num wobbles per second"}]
  (:ar
   (let [sweep (lin-exp (lf-tri wobble-factor) -1 1 40 3000)
         wob   (lpf src sweep)
         wob   (* 0.8 (normalizer wob))
         wob   (+ wob (bpf wob 1500 2))]
     (+ wob (* 0.2 (g-verb wob 9 0.7 0.7))))))


(definst dubstep [bpm 120 wobble-factor 1 note 50]
 (let [freq (midicps (lag note 0.25))
       bass (apply + (saw (* freq [0.99 1.01])))
       bass (wobble bass wobble-factor)
       kick (kick-drum bpm :pattern [1 0 0 0 0 0 1 0 1 0 0 1 0 0 0 0])
       snare (snare-drum bpm)]

   (clip2 (+ bass kick snare) 1)))


(dubstep)


;; ======================================================================
;; make a guitar
(def g (guitar))
;; strum it on your own
(guitar-strum g :E :down 0.25)
(guitar-strum g :E :up 0.75)
(guitar-strum g :B :down 0.25)
(guitar-strum g :A :up 0.5)
;; bow down to the power chord!
(ctl g :pre-amp 4.0 :distort 0.99)
(guitar-strum g [0 2 2 -1 -1 -1])
(guitar-strum g [3 5 5 -1 -1 -1])
;; mute all strings
(guitar-strum g [-1 -1 -1 -1 -1 -1])

;; ======================================================================
;; try out a bit of rhythmic accompanyment
;; http://www.youtube.com/watch?v=DV1ANPOYuH8
;; http://www.guitar.gg/strumming.html
(defn pattern-to-beat-strum-seq
  "given a string describing a one-measure up/down strum pattern like
  'ud-udu-', return a sequence of vector [beats :up/:down] pairs"
  [cur-pattern]
  (let [strums-per-measure (count cur-pattern)
        beats-per-measure 4.0
        beats-per-strum (/ beats-per-measure strums-per-measure)
        ud-keywords {\u :up, \d :down}]
    (for [[i s] (map-indexed vector cur-pattern)]
      (when (contains? ud-keywords s)
        [(* i beats-per-strum) (ud-keywords s)]))))

(defn strum-pattern [the-guitar metro cur-measure cur-chord cur-pattern]
  (let [cur-beat (* 4 cur-measure)]
    (doall
     (doseq [[b d] (pattern-to-beat-strum-seq cur-pattern)]
       (when-not (= b nil)
         (guitar-strum the-guitar cur-chord d 0.07 (metro (+ b cur-beat))))))))

;; play a variety of different rhythm patterns.
(ctl g :pre-amp 10.0 :amp 1.0 :distort 0.0)

(do ;; strumming practice
  (let [metro (metronome 100)]
    (doall
     (doseq [[i c] (map-indexed vector [:Gadd5 :Gadd5 :Cadd9 :Cadd9
                                        :Dsus4 :Dsus4 :Gadd5 :Cadd9
                                        :Gadd5 :Cadd9])]
       (strum-pattern g metro i c "d-du-ud-")))))

(do ;; knocking on heaven's door
  (let [metro (metronome 100)]
    (doall
     (doseq [[i c] (map-indexed vector [:Gadd5 :Dsus4 :Am :Am
                                        :Gadd5 :Dsus4 :Am :Am
                                        :Gadd5 :Dsus4 :Cadd9 :Cadd9])]
       (strum-pattern g metro i c "d-du-udu")))))

(do ;; 16th notes.
  (let [metro (metronome 90)]
    (doall
     (doseq [[i c] (map-indexed vector [:Gadd5 :Cadd9 :Gadd5 :Cadd9])]
       (strum-pattern g metro i c "d---d---dudu-ud-")))))

(stop)
;; ======================================================================
;; ac/dc's highway to hell intro.  turn it up!
(defn ddd0 []
  (let [t (now) dt 250]
    (guitar-strum g [-1  0  2  2  2 -1] :down 0.01 (+ t (* 0 dt)))
    (guitar-strum g [-1  0  2  2  2 -1] :up   0.01 (+ t (* 1 dt)))
    (guitar-strum g [-1  0  2  2  2 -1] :down 0.01 (+ t (* 2 dt) 50))
    (guitar-strum g [-1 -1 -1 -1 -1 -1] :down 0.01 (+ t (* 3.5 dt)))))

(defn ddd1 []
  (let [t (now) dt 250]
    (guitar-strum g [ 2 -1  0  2  3 -1] :down 0.01 (+ t (* 0 dt)))
    (guitar-strum g [ 2 -1  0  2  3 -1] :up   0.01 (+ t (* 1 dt)))
    (guitar-strum g [ 3 -1  0  0  3 -1] :down 0.01 (+ t (* 2 dt) 50))
    (guitar-strum g [-1 -1 -1 -1 -1 -1] :down 0.01 (+ t (* 3.5 dt)))))

(defn ddd2 []
  (let [t (now) dt 250]
    (guitar-strum g [ 2 -1  0  2  3 -1] :down 0.01 (+ t (* 0 dt)))
    (guitar-strum g [-1 -1 -1 -1 -1 -1] :down 0.01 (+ t (* 1.5 dt)))
    (guitar-strum g [-1  0  2  2  2 -1] :down 0.01 (+ t (* 2 dt)))
    (guitar-strum g [-1  0  2  2  2 -1] :up   0.01 (+ t (* 3 dt)))
    (guitar-strum g [-1 -1 -1 -1 -1 -1] :down 0.01 (+ t (* 4.5 dt)))))

;; give us a good, crunchy sound
(ctl g :pre-amp 5.0 :distort 0.96
     :lp-freq 5000 :lp-rq 0.25
     :rvb-mix 0.5 :rvb-room 0.7 :rvb-damp 0.4)
(ddd0) ;; play once
(ddd1) ;; repeat 3 times
(ddd2) ;; play once

;; ======================================================================
;; play with the one chord progression to rule them all
;; The I - V - vi - IV
;; (or C - G - Am - F)
(ctl g :pre-amp 4.0 :distort 0.5 :noise-amp 1.0
     :lp-freq 4000 :lp-rq 2.0
     :rvb-mix 0.45 :rvb-room 0.4 :rvb-damp 0.9)

(defn play1 [metro k N chord-list]
   (dotimes [n N]
     (doseq [[i cur-chord] (map-indexed vector chord-list)]
       (let [cur-dir (choose [:up :down])
             cur-pattern (choose ["d-du-ud-"
                                  "d-du-udu"
                                  "d-d--udu"])]
         (strum-pattern g metro (+ k (* 4 n) i) cur-chord cur-pattern)))))

;; every pop song ever written.  :^)
(doall
 (let [metro (metronome 100)]
   (play1 metro 0 4 [:C :G :Am :F])))

;; okay, change it up a bit
(doall
 (let [metro (metronome 132)]
   (play1 metro 0 1 [:C :G :Am :F])
   (play1 metro 4 1 [:Am :F :C :G])
   (play1 metro 8 1 [:C :G :Am :F])
   (play1 metro 12 1 [:C :G :Em :C])
   ))






;;TODO
;;Make a gamilan
;;http://www.berkeleynoise.com/celesteh/code/tutorials/supercollider05%20synthesis%20techniques.pdf
;;Do a find for "ring modulation"
;;Go to this youtube video to see how its made https://www.youtube.com/watch?v=vKHZD3CDQ0M


;;TODO
;;Set up an IDM sequence that uses ideas from these videos
;;https://www.youtube.com/watch?v=bk5JLX5mxhE&feature=youtu.be&t=2m49s (swing)
;;https://www.youtube.com/watch?v=z1CK8AM4cfE (interference sequencing)
;;https://www.youtube.com/watch?v=wxPUrH2Z6MI (intentions)
;;https://www.youtube.com/watch?v=hVmeG8kYtgk (tempo)
(stop)

(demo (sin-osc [440 0 1 0]))
(volume 0.5)
