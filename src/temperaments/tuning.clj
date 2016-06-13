(ns temperaments.tuning
  (:use clojure.math.numeric-tower
        temperaments.core)
  (:require [overtone.core :as o])
  (:gen-class))

(def tonic (atom :a3))

(def tempo (atom 120))

(def repetition (atom false))

(def twelfth6 (sqrt 2))
(def twelfth3 (sqrt twelfth6))
(def twelfth1  (expt twelfth3 1/3))
(def twelfth4 (expt 2 1/3))
(def twelfth5 (* twelfth1 twelfth4))
(def twelfth2 (* twelfth1 twelfth1))
(def twelfth7 (* twelfth3 twelfth4))

(def equal-temperament (map #(Math/pow 2 (/ % 12)) (range 0 12)))
(def pythagorean '(1 256/243 9/8 32/27 81/64 4/3 729/512 3/2 128/81 27/16 16/9 243/128))
(def just-intonation '(1 16/15 9/8 6/5 5/4 4/3 45/32 3/2 8/5 5/3 9/5 15/8))
(def odonnel (list 1,(* 8/9 twelfth3),
                        (* 8/9 twelfth4),(* 8/9 twelfth5),
                        (* 8/9 twelfth6),
                        4/3,(* 8/9 twelfth4 twelfth4),
                        (* 4/3 twelfth2),(* 8/9 twelfth6 twelfth4),
                        (* 32/27 twelfth6),(* twelfth6 twelfth4),
                        (* 32/27 twelfth4 twelfth4)))
(def neidhardt (list 1,(* 8/9 twelfth3),
                        (* 8/9 twelfth4),(* 8/9 twelfth5),
                        (* 8/9 twelfth6),
                        4/3,(* 8/9 twelfth4 twelfth4),
                        (* 4/3 twelfth2),(* 8/9 twelfth6 twelfth4),
                        (* 32/27 twelfth6),(* 9/8 twelfth4 twelfth4),
                        (* 32/27 twelfth4 twelfth4)))

(def tuning (atom equal-temperament))

(def counter (atom 0))

(def melody-map (atom {}))

(defn midi->freq [n]
  (let [tonic-note (o/note @tonic)
        tonic-freq (o/midi->hz tonic-note)
        diff (- n tonic-note)
        octave (if (< diff 0)
                 (dec (quot (inc diff) 12))
                 (quot diff 12))
        interval (mod diff 12)]
    (* tonic-freq (Math/pow 2 octave) (nth @tuning interval))))

(def inst-cur (atom nil))

