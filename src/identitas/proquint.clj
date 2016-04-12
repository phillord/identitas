(ns
    ^{:doc "Bi-directional Transformation between numbers and a pronouncable
equivalent."
      :author "Phillip Lord"}
    identitas.core
  (:require [clojure.string]
            [identitas.util :as u]
            ))

(def ^:private int-to-consonant
  '[\b \d \f \g \h \j \k \l
    \m \n \p \r \s \t \v \z])

(def ^:private int-to-vowel
  '[\a \i \o \u])

(def ^:private mask-4 0xF0000000)
(def ^:private mask-2 0xC0000000)

(defn ^:private int-shift [i mask left-shift right-shift]
  (let [j (bit-and i mask-4)
        i (bit-shift-left i left-shift)
        j (unsigned-bit-shift-right j right-shift)]
    [i j]))

(defn ^:private int-to-proint-1 [i]
  (let [[i1 j1] (int-shift i  mask-4 4 28)
        [i2 j2] (int-shift i1 mask-2 2 30)
        [i3 j3] (int-shift i2 mask-4 4 28)
        [i4 j4] (int-shift i3 mask-2 2 30)
        [i5 j5] (int-shift i4 mask-4 4 28)]
    [i5
     (str
      (nth int-to-consonant j1)
      (nth int-to-vowel j2)
      (nth int-to-consonant j3)
      (nth int-to-vowel j4)
      (nth int-to-consonant j5))]))

(def ^:private consonant-to-int
  {\b 0, \d 1, \f 2, \g 3,
   \h 4, \j 5, \k 6, \l 7,
   \m 8, \n 9, \p 10, \r 11,
   \s 12, \t 13, \v 14, \z 15})

(def ^:private vowel-to-int
  {\a 0, \i 1, \o 2, \u 3})


(defn ^:private proint-to-int-1
  ([s acc]
   (if (seq s)
     (if-let [add (get consonant-to-int (first s))]
       (recur (rest s)
              (+ (bit-shift-left acc 4) add))
       (if-let [add (get vowel-to-int (first s))]
         (recur (rest s)
                (+ (bit-shift-left acc 2) add))
         (recur (rest s) acc)))
     acc)))


;; * Public Interface


;; ** Conversion from and to numbers

(defn int-to-proint [i sep]
  (let [[i1 j1]
        (int-to-proint-1 i)
        [_ j2]
        (int-to-proint-1 i1)]
    (str j1 sep j2)))

(defn proint-to-int [p]
  (proint-to-int-1 p 0))

(defn short-to-proshort
  "Returns a short proquint."
  [s]
  (let [[h l] (u/integer-to-short s)]
    (when (not (= 0 h))
      (throw (IllegalArgumentException. (str "Value too large: " s))))
    (let [[j1 j2]
          (clojure.string/split
           (int-to-proint s " ")
           #"\s+")]
      j2)))

(defn proshort-to-short
  "Returns a short given a short proquint."
  [p]
  (proint-to-int (str "babab-" p)))

(defn long-to-prolong
  ([l]
   (long-to-prolong l "-"))
  ([l sep]
   (let [[i-big-end i-little-end]
         (u/long-to-integer l)]
     (str (int-to-proint i-big-end sep)
          sep
          (int-to-proint i-little-end sep)))))

(defn prolong-to-long [p]
  (let [[p1 p2 p3 p4]
        (clojure.string/split p)]
    (u/integer-to-long
     [(proint-to-int (str p1 "-" p2))
      (proint-to-int (str p3 "-" p4))])))


;; ** Random Identifiers

(defn random-proshort
  ([]
   (short-to-proshort (rand-int Short/MIN_VALUE Short/MAX_VALUE))))

(defn random-proint
  ([]
   (random-proint "-"))
  ([sep]
   (int-to-proint (rand-int Integer/MIN_VALUE Integer/MAX_VALUE) sep)))

(defn random-prolong
  ([]
   (random-prolong "-"))
  ([sep]
   (long-to-prolong (rand Long/MIN_VALUE Long/MAX_VALUE) sep)))
