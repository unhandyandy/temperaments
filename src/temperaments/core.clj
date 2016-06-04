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

;; (if (not (o/server-connected?))
;;   ;(o/boot-external-server 57110)
;;   (o/connect-external-server 57110)
;; )
(if (not (o/server-connected?))
  (let [portstr (input "Which port should we try?"
                       :value 57110)
        port (read-string portstr)]
    (o/connect-external-server port)
  ))

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
;; (o/definst piano [freq 220
;;                   gate 1
;;                   vel 100
;;                   decay 0.8
;;                   release 0.8
;;                   hard 0.8
;;                   velhard 0.8
;;                   muffle 0.8
;;                   velmuff 0.8
;;                   velcurve 0.8
;;                   stereo 0.2
;;                   tune 0.5
;;                   random 0.1
;;                   stretch 0
;;                   sustain 0]
;;   (let [snd (o/mda-piano {:freq freq
;;                           :gate gate
;;                           :vel vel
;;                           :decay decay
;;                           :release release
;;                           :hard hard
;;                           :velhard velhard
;;                           :muffle muffle
;;                           :velmuff velmuff
;;                           :velcurve velcurve
;;                           :stereo stereo
;;                           :tune tune
;;                           :random random
;;                           :stretch stretch
;;                           :sustain sustain})]
;;                                         ;(o/detect-silence snd 0.05 :action o/FREE)
;;     snd))

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

(def instruments ["Flute" ;"Piano"
                  "Mooger" "CS80" "SSaw" "Ticker" "Ping"])

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
    ;"Piano"   piano,
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

(def name->note
  {"A"       :a3,
   "B\u266D" :a#3,
   "B"       :b3,
   "C"       :c4,
   "C\u266F" :c#4,
   "D"       :d4,
   "E\u266D" :d#4,
   "E"       :e4,
   "F"       :f4,
   "F\u266F" :f#4,
   "G"       :g4,
   "G\u266F" :g#4})

(def note-names '("A" "B♭" "B" "C" "C♯" "D" "E♭" "E" "F" "F♯" "G" "G♯"))
 
;; (defn invert-hash [hash]
;;   (let [gr (group-by val hash)
;;         pairs (map #(vector (->> % val first first) (key %)) gr)]
;;     (into {} pairs)))

;; (def note->name
;;   (invert-hash name->note))

;; (def note->midi
;;   (let [notes (keys name->note)
;;         midis (map o/note notes)
;;         pairs (map vector midis notes)]
;;     (into {} pairs)))

;; (def midi->note
;;   (invert-hash note->midi))

(defn tonic-chooser [e]
  (let [val (value (.getSource e))]
    (reset! tonic (get name->note val))))

(defn str->midi [strnote]
  (let [oct (read-string (str (last strnote)))
        len (dec (count strnote))
        val (subs strnote 0 len)
        corr (case val ("A" "B♭" "B") 3 4)
        octch (* 12 (- oct corr))]
    (+ (o/note (name->note val)) octch)))


(defn midi->str [midi]
  (let [diff (- midi 59)
        corr (if (> diff 0) 0 -1)
        oct (+ 4 corr (quot diff 12))
        ind (mod (+ diff 2) 12)
        nm (nth note-names ind)]
    (s/join [nm (str oct)])))
        
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

(defn make-ratio-box [id]
  (let [cb (text :id (keyword (s/join ["poly-" id]))
                 :text ""
                 :editable? true)]
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

(def polychord-panel-size [240 :by 240])
  
;; make chord keyboard for xyz-panel 
(defn make-ratio-circle [id hours]
  (let [[w & {h :by}] polychord-panel-size
        angle (/ (* 2 (Math/PI)) hours)
        radius (* w 0.3)
        [xc yc] [(/ w 2) (/ h 2)]
        [wb hb] [70 20]]
    (map (fn [n]
           (let [curangle (+ (/ Math/PI -2) (* n angle))
                 x (+ xc (* radius (Math/cos curangle)))
                 x (- x (/ wb 2))
                 y (+ yc (* radius (Math/sin curangle)))
                 y (- y (/ hb 2))
                 cb (make-ratio-box id)]
             (config! cb :bounds [x y wb hb])
             cb))
         (range hours))))

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

(defn play-score
  ([score] (play-score true))
  ([score midiq]
   (let [metro (o/metronome @tempo)]
     (doseq [[instr & part] score]
       (play metro (str->instr instr) part {:midi midiq})))))

(defn play-melody [e]
  (let [[score len] (if @melody
                      [@melody 20]
                      (make-score))
        metro (o/metronome @tempo)]
    (doseq [[instr & part] score]
      (play metro (str->instr instr) part {:midi true}))
    (o/apply-at (metro len) #(if @repetition (play-melody nil)))
    ))
  
(defn play-solo [id e]
  (let [[mel rhy _ instr] (make-part id)
        notes (map str->midi mel)
        part (map vector notes rhy)]
    (play (o/metronome @tempo) (str->instr instr) part {:midi true})))

(defn get-ratios [e]
  (let [root (-> e .getSource .getParent)
        panel (select root [:#ratios])
        fields (select panel [:<javax.swing.text.JTextComponent>])
        ratiostrs (map value fields)]
    (map read-string ratiostrs)))

(defn play-polychord [e]
  (let [ratios (get-ratios e)
        one (->> @tonic o/note midi->freq)
        freqs (map #(* one %) ratios)
        root (-> e .getSource .getParent)
        tonic-button (select root [:#tonic-mode])
        tonic-mode (= (config tonic-button :text) "On")
        freqs (if tonic-mode
                freqs
                (drop 1 freqs))
        mode-button (select root [:#mode])
        simul (= "Chord" (config mode-button :text))
        score (if simul
                (map #(list "Default" [% 1]) freqs)
                [(cons "Default" (map #(vector % 1) freqs))])]
    (play-score score false)))
  

(defn remove-circle
  ([id kw] (remove-circle id kw true))
  ([id kw del?]
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
         (when del? (remove! panel w))))
     @circhash)))

(defn change-circle-mel [id newnum]
  (let [root (get-in @melody-map [id :root])
        panel (select root [:#kb])
        rhyvals (remove-circle id :beats)
        len (get-in @melody-map [id :beats])
        idprefix (s/join ["#kb-" id "-rhy-"])
        kwlist (vec (map #(keyword (s/join [idprefix (str %)])) (range len)))
        transtag (keyword (s/join ["#kb-" id "-mel-translate"]))
        transbut (select root [transtag])]
    (remove-circle id :pitches)
    (swap! melody-map assoc-in [id :pitches] newnum)
    (config! panel :items (concat (make-note-circle-mel id) (make-note-circle-rhy id)))
    (doseq [kw kwlist]
      (value! (select panel [kw]) (get rhyvals kw)))
    (config! transbut :model (range newnum))))
        
(defn change-circle-rhy [id newnum]
  (let [root (get-in @melody-map [id :root])
        panel (select root [:#kb])
        melvals (remove-circle id :pitches)
        len (get-in @melody-map [id :pitches])
        idprefix (s/join ["#kb-" id "-mel-"])
        kwlist (vec (map #(keyword (s/join [idprefix (str %)])) (range len)))
        transtag (keyword (s/join ["#kb-" id "-rhy-translate"]))
        transbut (select root [transtag])]
    (remove-circle id :beats)
    (swap! melody-map assoc-in [id :beats] newnum)
    (config! panel :items (concat (make-note-circle-mel id) (make-note-circle-rhy id)))
    (doseq [kw kwlist]
      (value! (select panel [kw]) (get melvals kw)))
    (config! transbut :model (range newnum))))

(defn change-circle-rat [root id newnum]
  (let [panel (select root [:#ratios])
        fields (select root [:<javax.swing.text.JTextComponent>])]
    (doseq [t fields]
      (remove! panel t))
    (config! panel :items (make-ratio-circle id newnum))))
  

(defn id->root [id]
  (get-in @melody-map [id :root]))

(defn id->panel [id]
  (let [root (id->root id)]
    (select root [:#kb])))

(defn make-kw-list [prefix len]
  (vec (map #(keyword (s/join [prefix (str %)])) (range len))))

(defn kw->idtag [kw]
  (let [word (subs (str kw) 1)]
    (keyword (s/join ["#" word]))))

(defn select-by-id [pan kw]
  (let [tag (kw->idtag kw)]
    (select pan [tag])))

(defn get-e-val [e]
  (value (.getSource e)))

;;;;;;;;;;;;;; transforms start

(defn translate-seq 
  ([n] #(translate-seq n %))
  ([n seq]
   (let [del (mod n (count seq))
         head (take del seq)
         tail (drop del seq)]
     (concat tail head))))
  
(defn reflect-seq
  ([n] #(reflect-seq n %))
  ([n seq]
   (let [del (mod (+ n 1) (count seq))
         trans (translate-seq del seq)]
     (reverse trans))))

(defn transpose-seq
  ([n] #(transpose-seq n %))
  ([n seq]
   (map #(+ n %) seq)))

(defn transpose-names
  ([n] #(transpose-names n %))
  ([n seq]
   (let [oldnotes (map str->midi seq)
         newnotes (transpose-seq n oldnotes)]
     (map midi->str newnotes))))

(defn invert-about
  ([center] #(invert-about center %))
  ([center nt]
   (let [diff (- nt center)]
     (int (- center diff)))))
      
(defn invert-seq
  ([center] #(invert-seq center %))
  ([center seq]
   (map (invert-about center) seq)))

(defn invert-names
  ([n] #(invert-names n %))
  ([n seq]
   (let [oldnotes (map str->midi seq)
         newnotes (invert-seq n oldnotes)]
     (map midi->str newnotes))))

(defn transform-dict [dict trans]
  (let [kws (keys dict)
        oldvals (vals dict)
        newvals (trans oldvals)
        pairs (map #(vector %1 %2) kws newvals)]
    (into {} pairs)))    
    
(defn transform-necklace [trans id typekw]
  (let [panel (id->panel id)
        olddict (remove-circle id typekw false)
        newdict (transform-dict olddict trans)
        kwlist (keys newdict)]
    (doseq [kw kwlist]
        (value! (select panel [kw]) (get newdict kw)))))

(defn make-transform-panel [id typekw]
  (let [len (get-in @melody-map [id typekw])
        typename (if (= typekw :beats)
                   "-rhy-" "-mel-")
        reflect-button (button :text "Reflect!"
                               :listen [:action (fn [e]
                                                  (transform-necklace
                                                   (reflect-seq 0) id typekw))])
        translabel (label "Translate: ")
        transbox (combobox :model (range len)
                           :id (keyword (s/join ["kb-" id typename "translate"]))
                           :listen [:action (fn [e]
                                           (transform-necklace
                                                   (translate-seq (get-e-val e))
                                                   id typekw))])]
    (horizontal-panel :items [reflect-button translabel transbox])))

(defn make-rowops-panel [id]
  (let [reflect-button (button :text "Invert!"
                               :listen [:action
                                        (fn [e]
                                          (transform-necklace
                                           (invert-names (+ (o/note @tonic) 3.5))
                                           id :pitches))])
        translabel (label "Transpose: ")
        transbox (combobox :model (range -6 7)
                           :id (keyword (s/join ["kb-" id "-mel-transpose"]))
                           :listen [:action (fn [e]
                                           (transform-necklace
                                                   (transpose-names (get-e-val e))
                                                   id :pitches))])]
    (value! reflect-button 0)
    (horizontal-panel :items [reflect-button translabel transbox])))

;;;;;;;;;;;;;; transforms end

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
        numcbs 1]
    (swap! melody-map assoc id {:pitches numcbs, :beats numcbs,
                                :root f, :instr "Default"})
    (add-note-circles id pan)
    (let [rowops (make-rowops-panel id)
          mel-controls (make-transform-panel id :pitches)
          rhy-controls (make-transform-panel id :beats)
          verticalpanel (vertical-panel :items [rowops mel-controls pan
                                                rhy-controls chinstr
                                                playsolo controls])]
      (config! f :content verticalpanel)
      (-> f pack! show!)
      f)))


(defn make-mono-panel [id]
  (let [prefix (s/join ["monochord-" (str id) "-"])
        tonic-kw (keyword (s/join [prefix "tonic"]))
        tonic-label (label "Tonic: ")
        tonic-box (combobox :model keyboard)
        tonic-bar (horizontal-panel :items [tonic-label tonic-box])
        note-label (label "Ratio: ")
        note-ratio (text :text "" :editable? true)
        note-bar (horizontal-panel :items [note-label note-ratio])
        mode-button (button :text "Seq."
                            :listen [:action (fn [e]
                                               (let [src (.getSource e)
                                                     curval (config src :text)]
                                                 (if (= curval "Seq.")
                                                   (config! src :text "Chord")
                                                   (config! src :text "Seq."))))])
        play-button (button :text "Play"
                            :listen [:action
                                     (fn [e]
                                       (let [ton (str->midi (value tonic-box))
                                             tonfreq (midi->freq ton)
                                             ratiostr (value note-ratio)
                                             ratio (read-string ratiostr)
                                             notefreq (* tonfreq ratio)
                                             tonvec [tonfreq 1]
                                             notevec [notefreq 1]
                                             simul (= "Chord"
                                                      (config mode-button :text))
                                             score (if simul
                                                     [["Default" tonvec]
                                                      ["Default" notevec]]
                                                     [["Default" tonvec
                                                       notevec]])
                                             metro (o/metronome @tempo)]
                                         (play-score score false)))])
        pan (vertical-panel :id :monochord
                            :size [160 :by 105]
                            :items [tonic-bar note-bar mode-button play-button])
        f (frame :title (s/join ["Monochord " (str id)])
                 :id (s/join ["mono-" (str id)])
                 :content pan)]
    (-> f pack! show!)))

(def save-polychord)
(def load-polychord)

(defn make-poly-panel [id]
  (let [prefix (s/join ["polychord-" (str id) "-"])
        mode-button (button :text "Seq."
                            :id :mode
                            :listen [:action (fn [e]
                                               (let [src (.getSource e)
                                                     curval (config src :text)]
                                                 (if (= curval "Seq.")
                                                   (config! src :text "Chord")
                                                   (config! src :text "Seq."))))])
        play-button (button :text "Play"
                            :listen [:action play-polychord])
        sizer-label (label "# notes: ")
        tonic-label (label "Tonic: ")
        tonic-button (button :text "On"
                             :id :tonic-mode
                             :listen [:action (fn [e]
                                               (let [src (.getSource e)
                                                     curval (config src :text)]
                                                 (if (= curval "On")
                                                   (config! src :text "Off")
                                                   (config! src :text "On"))))])
        tonic-bar (horizontal-panel :items [tonic-label tonic-button])
        circle-sizer (combobox :model (range 1 10)
                               :listen [:action
                                        (fn [e]
                                          (let [butt (.getSource e)
                                                root (-> butt .getParent .getParent)]
                                            (change-circle-rat root id (value butt))))])
        sizer-bar (horizontal-panel :items [sizer-label circle-sizer])
        saver (button :text "Save"
                      :listen [:action save-polychord])
        loader (button :text "Load"
                      :listen [:action load-polychord])
        fields (make-ratio-circle id 2)
        pan (xyz-panel :id :ratios
                       :size polychord-panel-size
                       :items fields)
        vertpan (vertical-panel :id :polychord
                                :items [pan mode-button play-button tonic-bar
                                        sizer-bar saver loader])
        f (frame :title (s/join ["Polychord " (str id)])
                 :id (s/join ["poly-" (str id)])
                 :content vertpan)]
    (value! circle-sizer 2)
    (-> f pack! show!)))
                                     
(defn new-mono [e]
  (swap! counter inc)
  (make-mono-panel @counter))

(defn new-poly [e]
  (swap! counter inc)
  (make-poly-panel @counter))

                                        
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

(defn save-polychord [e]
  (let [ratios (get-ratios e)
        savefile (choose-file :type :save)]
    (if savefile
      (spit savefile (vec ratios)))))

(defn load-polychord [e]
  (let [loadfile (choose-file :type :open)]
    (if loadfile
      (let [dumpstr (slurp loadfile)
            dump (clojure.edn/read-string dumpstr)
            csize (count dump)
            id (swap! counter inc)
            fields (make-ratio-circle id csize)
            root (-> e .getSource .getParent)
            panel (select root [:#ratios])]
        (doseq [[t r] (map vector fields dump)]
          (value! t r))
        (config! panel :items fields)))))


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
                           (button :id :monoChord
                                   :text "Polychord"
                                   :listen [:action new-poly])
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

(config! control-frame :on-close :exit)

(defn -main [& args]
  (-> control-frame pack! show!)
  ;;(make-melody-canvas "test-canvas")
  )
