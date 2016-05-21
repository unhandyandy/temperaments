(ns temperaments.core
  (:use seesaw.core
        seesaw.graphics
        seesaw.color
        seesaw.keystroke
        seesaw.chooser
        clojure.math.numeric-tower)
  (:require [overtone.core :as o]
            [clojure.string :as s])
  (:gen-class))

(native!)

(def tonic (atom :a3))

(def tuning (atom :equalTemperament))

(def tuning-map
  (atom {:equalTemperament (map #(Math/pow 2 (/ % 12)) (range 0 12)),
         :pythagorean '(1 256/243 9/8 32/27 81/64 4/3 729/512 3/2 128/81 27/16 16/9 243/128),
         :justIntonation '(1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8)
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

(def flute syn/simple-flute)
(def cs80  syn/cs80lead)
(def ssaw  syn/supersaw)
(def tick  syn/ticker)
(def ping  syn/ping)

(def inst-cur flute)

(defn play-test-tone [e]
  (inst-cur 440))

(defn kill-tones [e]
  (o/clear))

(def aof-subject '([:d4 2] [:a4 2] [:f4 2] [:d4 2] [:c#4 2] [:d4 1]
                   [:e4 1] [:f4 2.5] [:g4 0.5] [:f4 0.5] [:e4 0.5] [:d4 1]))
(def major '([:c4 1] [:d4 1] [:e4 1] [:f4 1] [:g4 1] [:a4 1] [:b4 1] [:c5 1]))
(def minor '([:c4 1] [:d4 1] [:eb4 1] [:f4 1] [:g4 1] [:ab4 1] [:bb4 1] [:c5 1]))

(defn inst-chooser [e]
  (let [val (value (.getSource e))]
    (case val
      "Flute"  (def inst-cur flute)
      "CS80"   (def inst-cur cs80)
      "SSaw"   (def inst-cur ssaw)
      "Ticker" (def inst-cur tick)
      "Ping"   (def inst-cur ping))))

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
      "Just"        (reset! tuning :justIntonation)))
  ;;(println (midi->freq 60))
  )


; https://compuzzle.wordpress.com/2015/02/10/overtone-and-clojure/
; setup a sound for our metronome to use
(def kick syn/ticker)
 
; setup a tempo for our metronome to use
(def one-twenty-bpm (o/metronome 120))
 
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

(def melody (atom [(note-to-midi major)]))

(defn melody-chooser [e]
  (let [val (value (.getSource e))]
    (case val
      "Art of Fugue"  (reset! melody [(note-to-midi aof-subject)])
      "Major Scale"   (reset! melody [(note-to-midi major)])
      "Minor Scale"   (reset! melody [(note-to-midi minor)])
      "Patterns"      (reset! melody false)
      )))

;Playing a Melody

;;(println aof-subject)

(defn play-one
  [metronome beat instrument [pitch dur]]
  (let [end (+ beat dur)]
    (if pitch
      (let [id (o/at (metronome beat) (instrument (midi->freq pitch)))]
        (o/at (metronome end) (o/ctl id :gate 0))))
    end))

(defn play
  ([metronome inst score]
     (play metronome (metronome) inst score))
  ([metronome beat instrument score]
     (let [cur-note (first score)]
       (when cur-note
         (let [next-beat (play-one metronome beat instrument cur-note)]
           (o/apply-at (metronome next-beat) play metronome next-beat instrument
             (next score) []))))))

(def a-scale ["A" "B\u266D" "B" "C" "C\u266F" "D"
              "E\u266D" "E" "F" "F\u266F" "G" "G\u266F"] )
(def c-scale ["C" "C\u266F" "D"
              "E\u266D" "E" "F" "F\u266F" "G" "G\u266F" "A" "B\u266D" "B"] )

(def make-melody-panel)
(defn new-panel [e]
  (swap! counter inc)
  (make-melody-panel @counter))



;(config! control-frame :on-close :exit)

;; keyboard for canvas
;; (defn keyboard [melid kbnum]
;;   (let [oct1 (map #(s/join [% "3"]) c-scale)
;;         oct2 (map #(s/join [% "4"]) c-scale)
;;         notes (concat oct1 oct2)]
;;     (map #(action :name %
;;                   :handler (fn [e]
;;                              (assoc (get (get melody-map melid) :pitches)
;;                                     kbnum %)))
;;          notes)))

(def keyboard 
  (let [oct1 (map #(s/join [% "3"]) c-scale)
        oct2 (map #(s/join [% "4"]) c-scale)]
    (concat oct1 oct2)))

(def test-model '("A" "B" "C" "D\u266D"))
                                    
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
(defn add-note-circles [id]
  (let [root (get-in @melody-map [id :root])
        mel-keys (make-note-circle-mel id)
        rhy-keys (make-note-circle-rhy id)]
    (config! root :items (concat mel-keys rhy-keys))))

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
        mel-pattern (map #(value (select root [(keyword (s/join [mel-prefix %]))]))
                         (range mel-len))
        rhy-seq (map #(value (select root [(keyword (s/join [rhy-prefix %]))]))
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
        total-time (apply + rhythm)]
    [fixmelody rhythm total-time]))


(defn make-score []
  (let [kbs (keys @melody-map)
        len (count kbs)
        parts (map make-part kbs)
        times (map #(nth % 2) parts)
        totaltime (lcm-list times)
        partrepeats (map #(/ totaltime %) times)
        score (atom '())]
    (doseq [i (range len)]
      (let [rhythm (apply concat (repeat (nth partrepeats i) (second (nth parts i))))
            melodyseq (apply concat (repeat (nth partrepeats i) (first (nth parts i))))
            notes (map str->midi melodyseq)
            part (map vector notes rhythm)]
        (swap! score conj part)))
    @score))


            
(defn play-melody [e]
  (let [score (or @melody
                  (make-score))]
    (doseq [part score]
      (play one-twenty-bpm inst-cur part))))

(defn play-solo [id e]
  (let [[mel rhy] (make-part id)
        notes (map str->midi mel)
        part (map vector notes rhy)]
    (play one-twenty-bpm inst-cur part)))

(defn remove-circle [id kw]
  (let [len (get-in @melody-map [id kw])
        kwstring (case kw
                   :pitches "-mel-"
                   :beats   "-rhy-")
        idprefix (s/join ["#kb-" id kwstring])
        idlist (vec (map #(keyword (s/join [idprefix (str %)])) (range len)))
        root (get-in @melody-map [id :root])
        circhash (atom {})]
    (doseq [k idlist]
      (let [w (select root [k])]
        ;(println w)
        (swap! circhash assoc k (value w))
        (remove! root w)))
    @circhash))

(defn change-circle-mel [id newnum]
  (let [root (get-in @melody-map [id :root])
        rhyvals (remove-circle id :beats)
        len (get-in @melody-map [id :beats])
        idprefix (s/join ["#kb-" id "-rhy-"])
        kwlist (vec (map #(keyword (s/join [idprefix (str %)])) (range len)))]
    (remove-circle id :pitches)
    (swap! melody-map assoc-in [id :pitches] newnum)
    (config! root :items (concat (make-note-circle-mel id) (make-note-circle-rhy id)))
    (doseq [kw kwlist]
      (value! (select root [kw]) (get rhyvals kw)))
    ))
        
(defn change-circle-rhy [id newnum]
  (let [root (get-in @melody-map [id :root])
        melvals (remove-circle id :pitches)
        len (get-in @melody-map [id :pitches])
        idprefix (s/join ["#kb-" id "-mel-"])
        kwlist (vec (map #(keyword (s/join [idprefix (str %)])) (range len)))]
    (remove-circle id :beats)
    (swap! melody-map assoc-in [id :beats] newnum)
    (config! root :items (concat (make-note-circle-mel id) (make-note-circle-rhy id)))
    (doseq [kw kwlist]
      (value! (select root [kw]) (get melvals kw)))
    ))
        
(defn make-panel-controls [id]
  (let [cbp (combobox :model (range 1 10)
                      :listen [:action
                               (fn [e]
                                 (change-circle-mel id (value (.getSource e))))])
        cbb (combobox :model (range 1 10)
                      :listen [:action
                               (fn [e]
                                 (change-circle-rhy id (value (.getSource e))))])]
    (horizontal-panel :items [(label "# pitches:") cbp
                              (label "  ")
                              (label "# beats:") cbb])))

(defn make-melody-panel [id]
  (let [pan (xyz-panel :id id 
                       :size panel-size
                       :items []
                       )
        controls (make-panel-controls id)
        playsolo (button :text "Play Solo"
                         :listen [:action #(play-solo id %)])
        verticalpanel (vertical-panel :items [pan playsolo controls])
        f (frame :title (s/join ["Tune " (str id)])
                 :id (s/join ["frame-" (str id)])
                 :listen [:window-closing
                          (fn [e]
                            (swap! melody-map dissoc id))])
        numcbs 1]
    (swap! melody-map assoc id {:pitches numcbs, :beats numcbs, :root pan})
    (add-note-circles id)
    (config! f :content verticalpanel)
    (-> f pack! show!)
    pan))  

(def control-frame
  (frame :title "Tuning Demo"
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
                           ;; spacer
                           (label :text ""
                                  :size [100 :by 30])
                           (label "Instrument:")
                           (combobox :model ["Flute" "CS80" "SSaw" "Ticker" "Ping"]
                                     :id :instChooser
                                     :listen [:action inst-chooser])
                           (label "Tonic:")
                           (combobox :model a-scale
                                     :id :tonicChooser
                                     :listen [:action tonic-chooser])
                           (label "Tuning:")
                           (combobox :model ["Equal" "Pythagorean" "Just"]
                                     :id :tuningChooser
                                     :listen [:action tuning-chooser])
                           (label "Melody:")
                           (combobox :model ["Major Scale" "Minor Scale" "Art of Fugue" "Patterns"]
                                     :id :melodyChooser
                                     :listen [:action melody-chooser])
                           ])))

(defn -main [& args]
  (-> control-frame pack! show!)
  ;;(make-melody-canvas "test-canvas")
  )
