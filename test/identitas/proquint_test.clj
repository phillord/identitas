(ns identitas.proquint-test
  (:require [identitas.proquint :as p]
            [clojure.test :refer :all]))


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
    ["babab" "zuzuz"]
    (map p/short-to-proshort [0 65535])))

  (is
   (thrown?
    IllegalArgumentException
    (p/short-to-proshort 65536)))

  (is
   (thrown?
    IllegalArgumentException
    (p/short-to-proshort -1))))
