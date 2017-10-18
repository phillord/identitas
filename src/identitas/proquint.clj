(ns
    ^{:doc "Bi-directional Transformation between numbers and a pronouncable
equivalent."
      :author "Phillip Lord"}
    identitas.proquint
  (:require [clojure.string :as str]
            [identitas.util :as u]
            [primitive.operator.integer :as i]))

(def ^:private int-to-consonant
  '[\b \d \f \g \h \j \k \l
    \m \n \p \r \s \t \v \z])

(def ^:private int-to-vowel
  '[\a \i \o \u])

(def ^:private mask-4 (unchecked-int 0xF0000000))
(def ^:private mask-2 (unchecked-int 0xC0000000))

(defn ^:private int-shift [i mask left-shift right-shift]
  (let [j (i/and i mask-4)
        i (i/left-shift i left-shift)
        j (i/unsigned-right-shift j right-shift)]
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
              (i/+ (i/left-shift acc 4) add))
       (if-let [add (get vowel-to-int (first s))]
         (recur (rest s)
                (i/+ (i/left-shift acc 2) add))
         (recur (rest s) acc)))
     acc)))


(defn check-short [acc]
  "Limit short output to unsigned Short/MAX (65535) and signed Short/MIN -32768"
  (when (or (< 65535  acc)
            (> -32768 acc))
            (throw (IllegalArgumentException.
                    (str "Value Too large for short: " acc))))
  acc)


(defn ^:private proshort-to-short-1
  "Returns short number only"
          ([s acc]
           (if (seq s)
             (if-let [add (get consonant-to-int (first s))]
               (recur (rest s)
                      (i/+ (i/left-shift acc 4) add))
               (if-let [add (get vowel-to-int (first s))]
                 (recur (rest s)
                      (i/+ (i/left-shift acc 2) add))
                 (recur (rest s) acc)))
             (check-short acc))))

;; * Public Interface


;; ** Conversion from and to numbers

(defn int-to-proint
  ([i]
   (int-to-proint i "-"))
  ([i sep]
   (when (not (and (<= Integer/MIN_VALUE i)
                   (>= Integer/MAX_VALUE i)))
     (throw (IllegalArgumentException.
             (str "Number out of range: " i))))
   (let [[i1 j1]
         (int-to-proint-1 i)
         [_ j2]
         (int-to-proint-1 i1)]
     (str j1 sep j2))))



(defn proint-to-int [p]
  (let [s (subs (str/lower-case p) 0 11)]
    (proint-to-int-1 s 0)))


(defn short-to-proshort
  "Returns a short proquint."
  [s]
  (when (not (and (<= Short/MIN_VALUE s)
                  (>= Short/MAX_VALUE s)))
    (throw (IllegalArgumentException.
            (str "Number out of range for short: " s))))
    (let [[j1 j2]
          (clojure.string/split
           (int-to-proint s " ")
           #"\s+")]
      j2))

(defn proshort-to-short
  "Returns a short given a short proquint.

The JVM does not actually have a short datatype, so by short, we mean a number
between 0 and 65535."
  [p]
  (let [s (subs (str/lower-case p) 0 5)]
  (unchecked-short (proshort-to-short-1 s 0))))

(defn long-to-prolong
  ([l]
   (when (not (and (<= Long/MIN_VALUE l)
                   (>= Long/MAX_VALUE l)))
     (throw (IllegalArgumentException.
             (str "Number out of range for Long: " l))))
  (long-to-prolong l "-"))
  ([l sep]
   (let [[i-big-end i-little-end]
         (u/long-to-integer l)]
     (str (int-to-proint i-big-end sep)
          sep
          (int-to-proint i-little-end sep)))))

(defn prolong-to-long [p]
  (let [s (subs (str/lower-case p) 0 23)] 
  (let [[p1 p2 p3 p4]
        (clojure.string/split s #"-")]
    (u/integer-to-long
     [(proint-to-int (str p1 "-" p2))
      (proint-to-int (str p3 "-" p4))]))))


;; ** Random Identifiers

(defn ranged-rand
  "Returns random int in range start <= rand < end"
  [start end]
  (+ start (int (rand-int (- end start)))))

(defn random-proshort
  ([]
   (short-to-proshort (ranged-rand -32768 32767))))

(defn random-proint
  ([]
   (random-proint "-"))
  ([sep]
   (int-to-proint (rand Integer/MIN_VALUE Integer/MAX_VALUE) sep)))

(defn random-prolong
  ([]
   (random-prolong "-"))
  ([sep]
   (long-to-prolong (rand Long/MIN_VALUE Long/MAX_VALUE) sep)))
