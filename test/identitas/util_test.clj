(ns identitas.util-test
  (:require [identitas.util :as u]
            [clojure.test :refer :all]))

(deftest short-integer-conversions
  (is
   (= [[0 0] [0 Short/MAX_VALUE]]
      (map #(u/integer-to-short %) [0 Short/MAX_VALUE])))
  (is
   (=
    [0 Short/MAX_VALUE]
    (map #(u/short-to-integer %) [[0 0] [0 Short/MAX_VALUE]])))

  (is (= [[-32768 0] [32767 -1]]
         (map #(u/integer-to-short %) [Integer/MIN_VALUE Integer/MAX_VALUE]))))1


(deftest integer-long-conversions
  (is
   (= [[0 0] [0 Integer/MAX_VALUE]]
      (map #(u/long-to-integer %) [0 Integer/MAX_VALUE])))
  (is
   (=
    [0 Integer/MAX_VALUE]
    (map #(u/integer-to-long %) [[0 0] [0 Integer/MAX_VALUE]])))

  (is (= [[-140737488355328 0] [140737488355327 -1]]
         (map #(u/integer-to-short %) [Long/MIN_VALUE Long/MAX_VALUE]))))
