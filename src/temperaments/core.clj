(ns temperaments.core
  (:use seesaw.core
        seesaw.graphics
        ;seesaw.color
        ;seesaw.keystroke
        seesaw.chooser
        clojure.math.numeric-tower)
  (:require [overtone.core :as o]
            [clojure.string :as s])
  (:gen-class))

(native!)

(def tonic (atom :a3))

(def tuning (atom :equalTemperament))

(def tempo (atom 120))

(def repetition (atom false))

(def twelfth6 (sqrt 2))
(def twelfth3 (sqrt twelfth6))
(def twelfth1  (expt twelfth3 1/3))
(def twelfth4 (expt 2 1/3))
(def twelfth5 (* twelfth1 twelfth4))
(def twelfth2 (* twelfth1 twelfth1))
(def twelfth7 (* twelfth3 twelfth4))

(def tuning-map
  (atom {:equalTemperament (map #(Math/pow 2 (/ % 12)) (range 0 12)),
         :pythagorean '(1 256/243 9/8 32/27 81/64 4/3 729/512 3/2 128/81 27/16 16/9 243/128),
         :justIntonation '(1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8)
         :odonnel (list 1,(* 8/9 twelfth3),
                        (* 8/9 twelfth4),(* 8/9 twelfth5),
                        (* 8/9 twelfth6),
                        4/3,(* 8/9 twelfth4 twelfth4),
                        (* 4/3 twelfth2),(* 8/9 twelfth6 twelfth4),
                        (* 32/27 twelfth6),(* twelfth6 twelfth4),
                        (* 32/27 twelfth4 twelfth4))
         :neidhardt (list 1,(* 8/9 twelfth3),
                        (* 8/9 twelfth4),(* 8/9 twelfth5),
                        (* 8/9 twelfth6),
                        4/3,(* 8/9 twelfth4 twelfth4),
                        (* 4/3 twelfth2),(* 8/9 twelfth6 twelfth4),
                        (* 32/27 twelfth6),(* 9/8 twelfth4 twelfth4),
                        (* 32/27 twelfth4 twelfth4))
         }))

(def counter (atom 0))

(def melody-map (atom {}))

(defn midi->freq [n]
  (let [tonic-note (o/note @tonic)
        tonic-freq (o/midi->hz tonic-note)
        diff (- n tonic-note)
        octave (quot diff 12)
        octave (if (< diff 0) (- octave 1) octave)
        interval (mod diff 12)]
    (* tonic-freq (Math/pow 2 octave) (nth (get @tuning-map @tuning) interval))))

(if (not (o/server-connected?))
  (o/boot-external-server))

(require '[overtone.inst.synth :as syn])

;(def flute syn/simple-flute)
(def cs80  syn/cs80lead)
(def ssaw  syn/supersaw)
(def tick  syn/ticker)
(def ping  syn/ping)

(o/definst flute [freq 880
                       amp 0.5
                       attack 0.4
                       decay 0.5
                       sustain 0.8
                       release 1
                       gate 1
                       out 0]
  (let [env  (o/env-gen (o/adsr attack decay sustain release) gate :action o/FREE)
        mod2 (o/lin-lin:kr (o/lf-noise2:kr 1) -1 1 0.2 1)
        mod3 (o/lin-lin:kr (o/sin-osc:kr (o/ranged-rand 4 6)) -1 1 0.5 1)
        sig (o/distort (* env (o/sin-osc freq)))
        sig (* amp sig mod2 mod3)]
    sig))


;; IMPORTANT: requires the mda-piano ugen to be available on your system

;; modified from repo to take freq rather than note
(o/definst piano [freq 220
                  gate 1
                  vel 100
                  decay 0.8
                  release 0.8
                  hard 0.8
                  velhard 0.8
                  muffle 0.8
                  velmuff 0.8
                  velcurve 0.8
                  stereo 0.2
                  tune 0.5
                  random 0.1
                  stretch 0
                  sustain 0]
  (let [snd (o/mda-piano {:freq freq
                          :gate gate
                          :vel vel
                          :decay decay
                          :release release
                          :hard hard
                          :velhard velhard
                          :muffle muffle
                          :velmuff velmuff
                          :velcurve velcurve
                          :stereo stereo
                          :tune tune
                          :random random
                          :stretch stretch
                          :sustain sustain})]
                                        ;(o/detect-silence snd 0.05 :action o/FREE)
    snd))

;; modified from repo to take freq rather than note
(o/definst mooger
  "Choose 0, 1, or 2 for saw, sin, or pulse"
  [freq 220
   amp  {:default 0.5 :min 0 :max 1 :step 0.01}
   osc1 {:default 0 :min 0 :max 2 :step 1}
   osc2 {:default 1 :min 0 :max 2 :step 1}
   osc1-level {:default 0.5 :min 0 :max 1 :step 0.01}
   osc2-level {:default 0.3 :min 0 :max 1 :step 0.01}
   cutoff {:default 500 :min 0 :max 20000 :step 1}
   attack {:default 0.0001 :min 0.0001 :max 5 :step 0.001}
   decay {:default 0.3 :min 0.0001 :max 5 :step 0.001}
   sustain {:default 0.99 :min 0.0001 :max 1 :step 0.001}
   release {:default 0.0001 :min 0.0001 :max 6 :step 0.001}
   fattack {:default 0.0001 :min 0.0001 :max 6 :step 0.001}
   fdecay {:default 0.3 :min 0.0001 :max 6 :step 0.001}
   fsustain {:default 0.999 :min 0.0001 :max 1 :step 0.001}
   frelease {:default 0.0001 :min 0.0001 :max 6 :step 0.001}
   gate 1]
  (let [osc-bank-1 [(o/saw freq) (o/sin-osc freq) (o/pulse freq)]
        osc-bank-2 [(o/saw freq) (o/sin-osc freq) (o/pulse freq)]
        amp-env    (o/env-gen (o/adsr attack decay sustain release)
                              gate :action o/FREE)
        f-env      (o/env-gen (o/adsr fattack fdecay fsustain frelease) gate)
        s1         (* osc1-level (o/select osc1 osc-bank-1))
        s2         (* osc2-level (o/select osc2 osc-bank-2))
        filt       (o/moog-ff (+ s1 s2) (* cutoff f-env) 3)]
    (* amp filt)))

(def instruments ["Flute" "Piano" "Mooger" "CS80" "SSaw" "Ticker" "Ping"])

(def inst-cur flute)

(defn kill-tones [e]
  (o/clear))

(def aof-subject '([:d4 2] [:a4 2] [:f4 2] [:d4 2] [:c#4 2] [:d4 1]
                   [:e4 1] [:f4 2.5] [:g4 0.5] [:f4 0.5] [:e4 0.5] [:d4 1]))
(def major
  (let [half '([:c4 1] [:d4 1] [:e4 1] [:f4 1] [:g4 1] [:a4 1] [:b4 1] [:c5 1])
        rev (drop 1 (reverse half))]
    (concat half rev)))
(def minor
  (let [half '([:c4 1] [:d4 1] [:d#4 1] [:f4 1] [:g4 1] [:a4 1] [:b4 1] [:c5 1])
        rev (concat '([:a#4 1] [:g#4 1]) (drop 3 (reverse half)))]
    (concat half rev)))

(defn str->instr [str]
  (case str
    "Flute"   flute,
    "Piano"   piano,
    "Mooger"  mooger,
    "CS80"    cs80,
    "SSaw"    ssaw,
    "Ticker"  tick,
    "Ping"    ping,
    "Default" inst-cur))

(defn inst-chooser [e]
  (let [val (value (.getSource e))
        instr (str->instr val)]
    (def inst-cur instr)))

(defn inst-chooser-part [id e]
  (let [val (value (.getSource e))
        oldmap (get @melody-map id)
        newmap (assoc oldmap :instr val)]
    (swap! melody-map assoc id newmap)))    
  
(defn tonic-chooser [e]
  (let [val (value (.getSource e))]
    (case val
      "A"       (reset! tonic :a3)
      "B\u266D" (reset! tonic :a#3)
      "B"       (reset! tonic :b3)
      "C"       (reset! tonic :c4)
      "C\u266F" (reset! tonic :c#4)
      "D"       (reset! tonic :d4)
      "E\u266D" (reset! tonic :d#4)
      "E"       (reset! tonic :e4)
      "F"       (reset! tonic :f4)
      "F\u266F" (reset! tonic :f#4)
      "G"       (reset! tonic :g4)
      "G\u266F" (reset! tonic :g#4))))

(defn str->midi [strnote]
  (let [oct (read-string (str (last strnote)))
        len (dec (count strnote))
        val (subs strnote 0 len)
        octch (* 12 (- oct 4))]
    (case val
      "C"       (+ 60 octch)
      "C\u266F" (+ 61 octch)
      "D"       (+ 62 octch)
      "E\u266D" (+ 63 octch)
      "E"       (+ 64 octch)
      "F"       (+ 65 octch)
      "F\u266F" (+ 66 octch)
      "G"       (+ 67 octch)
      "G\u266F" (+ 68 octch)
      "A"       (+ 69 octch)
      "B\u266D" (+ 70 octch)
      "B"       (+ 71 octch)
)))

(defn tuning-chooser [e]
  (let [val (value (.getSource e))]
    (case val
      "Equal"       (reset! tuning :equalTemperament)
      "Pythagorean" (reset! tuning :pythagorean)
      "Just"        (reset! tuning :justIntonation)
      "O'Donnel"        (reset! tuning :odonnel)
      "Neidhardt"        (reset! tuning :neidhardt)))
  ;;(println (midi->freq 60))
  )


; https://compuzzle.wordpress.com/2015/02/10/overtone-and-clojure/
; setup a sound for our metronome to use
(def kick syn/ticker)
 
; this function will play our sound at whatever tempo we've set our metronome to
(defn looper [nome sound]
    (let [beat (nome)]
        (o/at (nome beat) (sound))
        (o/apply-at (nome (inc beat)) looper nome sound [])))
 
; turn on the metronome
; (looper one-twenty-bpm kick)

(defn note-to-midi [[[n d :as v] & tail]]
  (if v
    (let [newnote (if n
                    [(o/note n) d]
                    [n d])]
      (concat (list newnote) (note-to-midi tail)))
    '()))
(defn string-to-midi [[[n d :as v] & tail]]
  (if v
    (let [newnote (if n
                    [(str->midi n) d]
                    [n d])]
      (concat (list newnote) (string-to-midi tail)))
    '()))

(def melody (atom [(cons "Default" (note-to-midi major))]))

(defn melody-chooser [e]
  (let [val (value (.getSource e))]
    (case val
      "Art of Fugue"  (reset! melody [(cons "Default" (note-to-midi aof-subject))])
      "Major Scale"   (reset! melody [(cons "Default" (note-to-midi major))])
      "Minor Scale"   (reset! melody [(cons "Default" (note-to-midi minor))])
      "Patterns"      (reset! melody false)
      )))

;Playing a Melody

;;(println aof-subject)

(defn play-one
  [metronome beat instrument [pitch dur] {midiq :midi}]
  (let [end (+ beat dur)]
    (if pitch
      (let [freq (if midiq
                   (midi->freq pitch)
                   pitch)
            id (o/at (metronome beat) (instrument freq))]
        (o/at (metronome end) (o/ctl id :gate 0))))
    end))

(defn play
  ([metronome inst score {midiq :midi}]
   (play metronome (metronome) inst score {:midi midiq}))
  ([metronome beat instrument score {midiq :midi}]
   (let [cur-note (first score)]
     (if cur-note
       (let [next-beat (play-one metronome beat instrument cur-note {:midi midiq})]
         (o/apply-at (metronome next-beat)
                     play metronome next-beat instrument
                     (next score) {:midi midiq} []))))))

(def a-scale ["A" "B\u266D" "B" "C" "C\u266F" "D"
              "E\u266D" "E" "F" "F\u266F" "G" "G\u266F"] )
(def c-scale ["C" "C\u266F" "D"
              "E\u266D" "E" "F" "F\u266F" "G" "G\u266F" "A" "B\u266D" "B"] )

(def make-melody-panel)
(defn new-panel [e]
  (swap! counter inc)
  (make-melody-panel @counter))

(def keyboard 
  (let [oct1 (map #(s/join [% "3"]) c-scale)
        oct2 (map #(s/join [% "4"]) c-scale)]
    (concat oct1 oct2)))

(defn make-note-box [id]
  (let [cb (combobox :id (keyword (s/join ["kb-" id]))
                     :model keyboard)]
    (.setMaximumRowCount cb 24)
    cb))

(def panel-size [240 :by 500])
(defn get-panel-size []
  [(first panel-size) (last panel-size)])

;; make mel keyboards for xyz-panel 
(defn make-note-circle-mel [id]
  (let [[w h] (get-panel-size)
        mel-hours (get (get @melody-map id) :pitches)
        mel-angle (/ (* 2 (Math/PI)) mel-hours)
        radius (* w 0.3)
        [xc yc] [(/ w 2) (* h 0.2)]
        [wb hb] [50 20]]
    (map (fn [n]
           (let [angle (+ (/ Math/PI -2) (* n mel-angle))
                 x (+ xc (* radius (Math/cos angle)))
                 x (- x (/ wb 2))
                 y (+ yc (* radius (Math/sin angle)))
                 y (- y (/ hb 2))
                 cb (make-note-box (s/join [id "-mel-" (str n)]))]
             (config! cb :bounds [x y wb hb])
             cb))
         (range mel-hours))))
  
;; make rhy keyboards for xyz-panel 
(defn make-note-circle-rhy [id]
  (let [[w h] (get-panel-size)
        rhy-hours (get (get @melody-map id) :beats)
        rhy-angle (/ (* 2 (Math/PI)) rhy-hours)
        radius (* w 0.3)
        [xc yc] [(/ w 2) (* h 0.6)]
        [wb hb] [50 20]]
    (map (fn [n]
           (let [angle (+ (/ Math/PI -2) (* n rhy-angle))
                 x (+ xc (* radius (Math/cos angle)))
                 x (- x (/ wb 2))
                 y (+ yc (* radius (Math/sin angle)))
                 y (- y (/ hb 2))
                 cb (combobox :id (keyword
                                   (s/join ["kb-" id "-rhy-" (str n)]))
                              :model ["X" "O"])]
             (config! cb :bounds [x y wb hb])
             cb))
         (range rhy-hours))))

  ;; adds keyboards to xyz-panel 
(defn add-note-circles [id panel]
  (let [mel-keys (make-note-circle-mel id)
        rhy-keys (make-note-circle-rhy id)]
    (config! panel :items (concat mel-keys rhy-keys))))

(defn make-style [c]
  (style :foreground c :background c))

(defn str-pattern->rhythm [strlist]
  (let [res (atom '())
        curlen (atom 0)]
    (doseq [x strlist]
      (if (= x "X")
        (do
          (swap! res conj @curlen)
          (reset! curlen 1))
        (swap! curlen + 1)))
    (swap! res conj @curlen)
    (reverse @res)))

;; first entry of rhythm is wait before notes start
(defn multiply-rhythm [rhythm n] 
   (if (= n 0)
     '()
     (let [[a & tail] rhythm]
       (if (= a 0)
         (apply concat (repeat n tail))
         (let [b (+ (last rhythm) a)
               newrhythm (assoc (vec tail) (dec (count tail)) b)]
           (concat [a] (apply concat (repeat n newrhythm))))))))

(defn lcm-list [[a & bs]]
  (if bs
    (let [[b & cs] bs]
      (lcm-list (concat [(lcm a b)] cs)))
    a))

(defn make-part [id]
  (let [rhy-prefix (s/join ["#kb-" id "-rhy-"])
        mel-prefix (s/join ["#kb-" id "-mel-"])
        mel-len (get-in @melody-map [id :pitches])
        root (get-in @melody-map [id :root])
        panel (select root [:#kb])
        mel-pattern (map #(value (select panel [(keyword (s/join [mel-prefix %]))]))
                         (range mel-len))
        rhy-seq (map #(value (select panel [(keyword (s/join [rhy-prefix %]))]))
                     (range (get-in @melody-map [id :beats])))
        rhy-pattern (str-pattern->rhythm rhy-seq)
        rhy-len (count (filter #(> % 0) rhy-pattern))
        tot-len (lcm rhy-len mel-len)
        mel-repeats (/ tot-len mel-len)
        rhy-repeats (/ tot-len rhy-len)
        melodyseq (apply concat (repeat mel-repeats mel-pattern))
        fixmelody (if (= (first rhy-pattern) 0)
                    melodyseq
                    (concat [nil] melodyseq))
        rhythm (multiply-rhythm rhy-pattern rhy-repeats)
        total-time (apply + rhythm)
        instrstr (get-in @melody-map [id :instr])]
    [fixmelody rhythm total-time instrstr]))


(defn make-score []
  (let [kbs (keys @melody-map)
        len (count kbs)
        parts (map make-part kbs)
        times (map #(nth % 2) parts)
        totaltime (lcm-list times)
        partrepeats (map #(/ totaltime %) times)
        score (atom '())]
    (doseq [i (range len)]
      (let [thispart (nth parts i)
            thisinstr (nth thispart 3)
            thisrepeats (nth partrepeats i)
            rhythm (apply concat (repeat thisrepeats (second thispart)))
            melodyseq (apply concat (repeat thisrepeats (first thispart)))
            notes (map str->midi melodyseq)
            part (cons thisinstr (map vector notes rhythm))]
        (swap! score conj part)))
    [@score, totaltime]))

(defn play-melody [e]
  (let [[score len] (if @melody
                      [@melody 20]
                      (make-score))
        metro (o/metronome @tempo)
        endseq (atom '())]
    (doseq [[instr & part] score]
      (play metro (str->instr instr) part {:midi true}))
    (o/apply-at (metro len) #(if @repetition (play-melody nil)))
    ))
  
(defn play-solo [id e]
  (let [[mel rhy _ instr] (make-part id)
        notes (map str->midi mel)
        part (map vector notes rhy)]
    (play (o/metronome @tempo) (str->instr instr) part {:midi true})))  
    
(defn remove-circle [id kw]
  (let [len (get-in @melody-map [id kw])
        kwstring (case kw
                   :pitches "-mel-"
                   :beats   "-rhy-")
        idprefix (s/join ["#kb-" id kwstring])
        idlist (vec (map #(keyword (s/join [idprefix (str %)])) (range len)))
        root (get-in @melody-map [id :root])
        panel (select root [:#kb])
        circhash (atom {})]
    ;(println idlist)
    (doseq [k idlist]
      (let [w (select panel [k])]
        (swap! circhash assoc k (value w))
        (remove! panel w)))
    @circhash))

(defn change-circle-mel [id newnum]
  (let [root (get-in @melody-map [id :root])
        panel (select root [:#kb])
        rhyvals (remove-circle id :beats)
        len (get-in @melody-map [id :beats])
        idprefix (s/join ["#kb-" id "-rhy-"])
        kwlist (vec (map #(keyword (s/join [idprefix (str %)])) (range len)))]
    (remove-circle id :pitches)
    (swap! melody-map assoc-in [id :pitches] newnum)
    (config! panel :items (concat (make-note-circle-mel id) (make-note-circle-rhy id)))
    (doseq [kw kwlist]
      (value! (select panel [kw]) (get rhyvals kw)))
    ))
        
(defn change-circle-rhy [id newnum]
  (let [root (get-in @melody-map [id :root])
        panel (select root [:#kb])
        melvals (remove-circle id :pitches)
        len (get-in @melody-map [id :pitches])
        idprefix (s/join ["#kb-" id "-mel-"])
        kwlist (vec (map #(keyword (s/join [idprefix (str %)])) (range len)))]
    (remove-circle id :beats)
    (swap! melody-map assoc-in [id :beats] newnum)
    (config! panel :items (concat (make-note-circle-mel id) (make-note-circle-rhy id)))
    (doseq [kw kwlist]
      (value! (select panel [kw]) (get melvals kw)))
    ))
        
(defn make-panel-controls [id]
  (let [cbp (combobox :model (range 1 10)
                      :id (keyword (s/join ["kb-" (str id) "-numpitch"]))
                      :listen [:action
                               (fn [e]
                                 (change-circle-mel id (value (.getSource e))))])
        cbb (combobox :model (range 1 10)
                      :id (keyword (s/join ["kb-" (str id) "-numbeats"]))
                      :listen [:action
                               (fn [e]
                                 (change-circle-rhy id (value (.getSource e))))])]
    (horizontal-panel :items [(label "# pitches:") cbp
                              (label "  ")
                              (label "# beats:") cbb])))

(defn make-melody-panel [id]
  (let [pan (xyz-panel :id :kb
                       :size panel-size
                       :items []
                       )
        controls (make-panel-controls id)
        playsolo (button :text "Play Solo"
                         :listen [:action #(play-solo id %)])
        chinstr (combobox :model (cons "Default" instruments)
                          :text "Instrument:"
                          :id (keyword (s/join ["kb-" (str id) "-instr"]))
                          :listen [:action #(inst-chooser-part id %)])
        f (frame :title (s/join ["Tune " (str id)])
                 :id (s/join ["frame-" (str id)])
                 :listen [:window-closing
                          (fn [e]
                            (swap! melody-map dissoc id))])
        verticalpanel (vertical-panel :items [pan chinstr playsolo controls])
        numcbs 1]
    (swap! melody-map assoc id {:pitches numcbs, :beats numcbs,
                                :root f, :instr "Default"})
    (add-note-circles id pan)
    (config! f :content verticalpanel)
    (-> f pack! show!)
    f))

(defn make-circles-hash [id]
  (let [root (get-in @melody-map [id :root])
        panel (select root [:#kb])
        melprefix (s/join ["#kb-" id "-mel-"])
        rhyprefix (s/join ["#kb-" id "-rhy-"])
        mellen (get-in @melody-map [id :pitches])
        rhylen (get-in @melody-map [id :beats])
        mellist (map #(keyword (s/join [melprefix (str %)])) (range mellen))
        rhylist (map #(keyword (s/join [rhyprefix (str %)])) (range rhylen))
        idlist (vec (concat mellist rhylist))
        circhash (atom {})]
    (doseq [k idlist]
      (let [w (select panel [k])]
        (swap! circhash assoc k (value w))))
    @circhash))

(defn replace-in-key [key old new]
  (let [oldkeystr (str key)
        newkeystr (s/replace oldkeystr old new)]
    (keyword (subs newkeystr 1))))
        
(defn restore-circles [id oldid panel circhash]
  (let [oldstr (s/join ["-" (str oldid) "-"])
        newstr (s/join ["-" (str id) "-"])]
    (doseq [[k v] circhash]
      (let [newk (replace-in-key k oldstr newstr)]
        (value! (select panel [newk]) v)))))

(defn restore-panel [oldid panhash circhash]
  (let [newroot (new-panel nil)
        id @counter
        panel (select newroot [:#kb])
        newpanhash (assoc panhash :root newroot)
        numpitbut (select newroot [(keyword (s/join ["#kb-" (str id) "-numpitch"]))])
        numbeabut (select newroot [(keyword (s/join ["#kb-" (str id) "-numbeats"]))])
        instrubut (select newroot [(keyword (s/join ["#kb-" (str id) "-instr"]))])
        ]
    (value! numpitbut (get panhash :pitches))
    (value! numbeabut (get panhash :beats))
    (value! instrubut (get panhash :instr))
    (restore-circles id oldid panel circhash)
    (swap! melody-map assoc id newpanhash)))

(defn make-dump []
  (let [dump (atom {})]
    (doseq [[k v] @melody-map]
      (let [val {:panhash (dissoc v :root),
                 :circhash (make-circles-hash k)}]
        (swap! dump assoc k val)))
    (swap! dump assoc :tempo @tempo)
    @dump))
        
;; create slider used to adjust tempo
(def tempo-slider
  (slider :orientation :horizontal
          ;:value 120
          :min 20
          :max 360
          :snap-to-ticks? false
          :paint-labels? true
          :major-tick-spacing 100
          :minor-tick-spacing 20))

(defn restore-dump [dump]
  (value! tempo-slider (get dump :tempo))
  (doseq [[oldid {:keys [panhash circhash]}] (dissoc dump :tempo)]
    (restore-panel oldid panhash circhash)))

(defn save-pattern [e]
  (let [dump (make-dump)
        savefile (choose-file :type :save)]
    (if savefile
      (spit savefile dump))))

(defn load-pattern [e]
  (let [loadfile (choose-file :type :open)]
    (if loadfile
      (let [dumpstr (slurp loadfile)
            dump (clojure.edn/read-string dumpstr)]
        (restore-dump dump)))))


;; handler for tempo-slider
(listen tempo-slider
        :change
        (fn [e]
          (reset! tempo (value tempo-slider))))

(value! tempo-slider 120)

(def repeat-label (label " Off"))

(defn repeat-action [e]
  (swap! repetition not)
  (value! repeat-label (if @repetition " On" " Off")))
                         
(def control-frame
  (frame :title "Music Demo"
         :content (vertical-panel
                   :items [;; play tone
                           (button :id :playTestTone
                                   :text "Play"
                                   :listen [:action play-melody])
                           ;; kill tones
                           (button :id :killTones
                                   :text "Kill Tones"
                                   :listen [:action kill-tones])
                           ;; new panel
                           (button :id :newPanel
                                   :text "New Panel"
                                   :listen [:action new-panel])
                           (button :id :saveButt
                                   :text "Save"
                                   :listen [:action save-pattern])
                           (button :id :loadButt
                                   :text "Load"
                                   :listen [:action load-pattern])
                           ;; spacer
                           (label :text ""
                                  :size [100 :by 30])
                           (label "Tempo:")
                           tempo-slider
                           ;; spacer
                           (label :text ""
                                  :size [100 :by 30])
                           (horizontal-panel
                            :items [(button :id :repeats
                                            :text "Repeat?"
                                            :listen [:action repeat-action])
                                    repeat-label])
                           (combobox :model instruments
                                     :id :instChooser
                                     :listen [:action inst-chooser])
                           (label "Instrument:")
                           (combobox :model instruments
                                     :id :instChooser
                                     :listen [:action inst-chooser])
                           (label "Tonic:")
                           (combobox :model a-scale
                                     :id :tonicChooser
                                     :listen [:action tonic-chooser])
                           (label "Tuning:")
                           (combobox :model ["Equal" "Pythagorean" "Just"
                                             "O'Donnel" "Neidhardt"]
                                     :id :tuningChooser
                                     :listen [:action tuning-chooser])
                           (label "Melody:")
                           (combobox :model ["Major Scale" "Minor Scale"
                                             "Art of Fugue" "Patterns"]
                                     :id :melodyChooser
                                     :listen [:action melody-chooser])
                           ])))

(defn -main [& args]
  (-> control-frame pack! show!)
  ;;(make-melody-canvas "test-canvas")
  )
