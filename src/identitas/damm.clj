(ns
    ^{:doc "Validate integers with a check-digit according to the Damm algorithm."
      :author "Phillip Lord"}
    identitas.damm)


;; https://en.wikipedia.org/wiki/Damm_algorithm
;; https://en.wikibooks.org/wiki/Algorithm_Implementation/Checksums/Damm_Algorithm#Python

;;# we use the matrix given in the WP article because it's a good one
(def matrix
  [
   [0, 3, 1, 7, 5, 9, 8, 6, 4, 2]
   [7, 0, 9, 2, 1, 5, 4, 8, 6, 3]
   [4, 2, 0, 6, 8, 7, 1, 3, 5, 9]
   [1, 7, 5, 0, 9, 8, 3, 4, 2, 6]
   [6, 1, 2, 3, 0, 4, 5, 9, 7, 8]
   [3, 6, 7, 4, 2, 0, 9, 5, 8, 1]
   [5, 8, 6, 9, 7, 2, 0, 1, 3, 4]
   [8, 9, 4, 5, 3, 6, 2, 0, 1, 7]
   [9, 4, 3, 8, 6, 1, 7, 2, 0, 5]
   [2, 5, 8, 1, 4, 3, 6, 7, 9, 0]
   ])

(defn- listify
  "Convert a number into a list of digits."
  [number]
  (loop [lst '()
         number number]
    (if (zero? number)
      lst
      (recur (cons (unchecked-remainder-int number 10)
                   lst)
             (unchecked-divide-int number 10)))))

(defn- check-digit-1 [seq-number interim]
  (if (seq seq-number)
    (recur (rest seq-number)
           (nth
            (nth matrix interim)
            (first seq-number)))
    interim))

(defn check-digit [number]
  (check-digit-1
   (listify number) 0))

(defn add-check [number]
  (+
   (* 10 number)
   (check-digit number)))

(defn valid? [number]
  (zero?
   (check-digit number)))
