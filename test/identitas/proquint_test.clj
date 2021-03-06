(ns identitas.proquint-test
  (:require [identitas.proquint :as p]
            [clojure.test :refer :all]))


(deftest long-prolong-conversions
  (is
   (=
    ["mabab-babab-babab-babab" "babab-babab-babab-babab"
     "luzuz-zuzuz-zuzuz-zuzuz"]
    (map p/long-to-prolong [Long/MIN_VALUE 0 Long/MAX_VALUE])))

  (is
   (thrown?
    IllegalArgumentException
    (p/long-to-prolong (+' 1 Long/MAX_VALUE))))

  (is
   (thrown?
    IllegalArgumentException
    (p/long-to-prolong (-' 1 Long/MIN_VALUE)))))

(deftest int-proint-conversions
  (is
   (=
    ["mabab-babab" "babab-babab" "luzuz-zuzuz"]
    (map p/int-to-proint [Integer/MIN_VALUE 0 Integer/MAX_VALUE])))

  (is
   (thrown?
    IllegalArgumentException
    (p/int-to-proint (+ 1 Integer/MAX_VALUE))))

  (is
   (thrown?
    IllegalArgumentException
    (p/int-to-proint (- 1 Integer/MIN_VALUE)))))

(deftest short-proshort-conversions
  (is
   (=
    ["mabab" "babab" "luzuz"]
    (map p/short-to-proshort [-32768 0 32767])))

  (is
   (thrown?
    IllegalArgumentException
    (p/short-to-proshort (+ 1 Short/MAX_VALUE))))

  (is
   (thrown?
    IllegalArgumentException
    (p/short-to-proshort (- 1 Short/MIN_VALUE)))))


(def int-pro-int
  (comp p/proint-to-int p/int-to-proint))

(defn roundtrip? [f n]
  (= n (f n)))

(deftest int-round
  (is
   (roundtrip? int-pro-int 0))

  (is
   (roundtrip? int-pro-int 1))

  (is
   (roundtrip? int-pro-int Integer/MAX_VALUE))

  (is
   (roundtrip? int-pro-int -1))

  (is
   (roundtrip? int-pro-int Integer/MIN_VALUE)))

