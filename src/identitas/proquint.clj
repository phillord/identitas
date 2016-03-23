(ns proquint.core
  (:require [clojure.string]
            [proquint.damm]))

(def uint2consonant
  '[\b \d \f \g \h \j \k \l
    \m \n \p \r \s \t \v \z])

(def uint2vowel
  '[\a \i \o \u])

(def mask-4 0xF0000000)
(def mask-2 0xC0000000)

(defn ^:private uint-shift [i mask left-shift right-shift]
  (let [j (bit-and i mask-4)
        i (bit-shift-left i left-shift)
        j (unsigned-bit-shift-right j right-shift)]
    [i j]))

(defn uint2quint-1 [i]
  (let [[i1 j1] (uint-shift i  mask-4 4 28)
        [i2 j2] (uint-shift i1 mask-2 2 30)
        [i3 j3] (uint-shift i2 mask-4 4 28)
        [i4 j4] (uint-shift i3 mask-2 2 30)
        [i5 j5] (uint-shift i4 mask-4 4 28)]
    [i5
     (str
      (nth uint2consonant j1)
      (nth uint2vowel j2)
      (nth uint2consonant j3)
      (nth uint2vowel j4)
      (nth uint2consonant j5))]))

(defn uint2quint [i sep]
  (let [[i1 j1]
        (uint2quint-1 i)
        [_ j2]
        (uint2quint-1 i1)]
    (str j1 sep j2)))

(def consonant2uint
  {\b 0, \d 1, \f 2, \g 3,
   \h 4, \j 5, \k 6, \l 7,
   \m 8, \n 9, \p 10, \r 11,
   \s 12, \t 13, \v 14, \z 15})

(def vowel2uint
  {\a 0, \i 1, \o 2, \u 3})


(defn ^:private quint2uint-1
  ([s acc]
   (if (seq s)
     (if-let [add (get consonant2uint (first s))]
       (recur (rest s)
              (+ (bit-shift-left acc 4) add))
       (if-let [add (get vowel2uint (first s))]
         (recur (rest s)
                (+ (bit-shift-left acc 2) add))
         (recur (rest s) acc)))
     acc)))

(defn quint2uint [s]
  (quint2uint-1 s 0))

(defn random-proquint
  ([]
   (random-proquint "-"))
  ([sep]
   (uint2quint (rand-int Integer/MAX_VALUE) sep)))

(def max-val-by-10
  (unchecked-divide-int
   Integer/MAX_VALUE 10))

(defn random-damm-proquint
  ([]
   (random-damm-proquint "-"))
  ([sep]
   (uint2quint
    (proquint.damm/add-check
     (rand-int max-val-by-10)) sep)))

(defn damm-valid? [ident]
  (proquint.damm/valid?
   (quint2uint ident)))
