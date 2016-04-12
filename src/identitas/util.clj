(ns
  ^{:doc "Utility functions for identitas"
    :author "Phillip Lord"
    :private true}
    identitas.util)

(defn integer-to-short
  "Given an integer return a tuple of shorts.

i should be an integer. The return value is the big end and little end of
the integer respectively."
  [i]
  [(bit-shift-right i 16)
   (unchecked-short i)])

(defn short-to-integer
  "Given a tuple of shorts return an int.

s should be a two element list, each of two shorts representing the big and
  little end of the return value respectively. "
  [s]
  (let [[h l] s]
    (bit-or
     (bit-shift-left h 16)
     (bit-and l 0xFFFF))))

(defn long-to-integer
  "Given a long return a tuple of ints.

l should be an long. The return value is a two element list representing the
big and little end of l respectively."
  [l]
  [(bit-shift-right l 32)
   (unchecked-int l)])

(defn integer-to-long
  "Given a tuple of ints return a long.

i should be a two element list, of two integers representing the big and
little end of the return value respectively."
  [i]
  (let [[h l] i]
    (bit-or
     (bit-shift-left h 32)
     (bit-and l 0xFFFFFFFF))))
