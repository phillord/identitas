(ns identitas.core
  (:require [identitas.proquint :as p]
            [identitas.damm :as d]))

(def ^{:private true} max-val-by-10
  (unchecked-divide-int
   Integer/MAX_VALUE 10))

(defn random-damm-proint
  ([]
   (random-damm-proint "-"))
  ([sep]
   (p/int-to-proint
    (d/add-check
     (rand-int max-val-by-10)) sep)))

(defn proint-damm-valid? [ident]
  (d/valid?
   (p/proint-to-int ident)))
