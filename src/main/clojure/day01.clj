(ns day01
  (:require [clojure.string :refer [split]]))

(def input (mapv #(Integer/parseInt %) (split (slurp "./src/main/clojure/day01.in") #"\n")))

(defn solve [measurements offset]
  (->> (partition offset 1 measurements)
       (filter #(< (first %) (last %)))
       (count)))

(solve input 2)
(solve input 4)