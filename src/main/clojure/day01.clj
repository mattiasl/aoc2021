(ns day01
  (:require [clojure.string :refer [split]]))

(def input (map #(Integer/parseInt %) (split (slurp "./src/main/clojure/day01.in") #"\n")))

(def test-input [199 200 208 210 200 207 240 269 260 263])

(defn part-1 [input]
  (->> (partition 2 1 input)
       (filter #(< (first %) (second %)))
       (count)))

(defn part-2 [input]
  (->> (partition 3 1 input)
       (map #(reduce + %))
       (part-1)))

(part-1 test-input)
(part-2 test-input)

(part-1 input)
(part-2 input)