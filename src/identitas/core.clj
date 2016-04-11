(ns identitas.core)

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

(defn proquint-damm-valid? [ident]
  (proquint.damm/valid?
   (quint2uint ident)))
