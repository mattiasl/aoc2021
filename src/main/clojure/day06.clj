(ns day06
  (:require [clojure.string :refer [split]]))

(def input (map #(Integer/parseInt %) (split (slurp "./src/main/clojure/day06.in") #",")))

(defn input->initial-state [input]
  (reduce (fn [a [k v]] (update a k + v))
          (vec (repeat 9 0))
          (frequencies input)))

(defn simulate-day [state]
  (let [hatched (first state)
        new-state (conj (vec (rest state)) hatched)]
    (update new-state 6 + hatched)))

(defn solver [input days]
  (as-> (input->initial-state input) $
        (reduce (fn [state _] (simulate-day state)) $ (range days))
        (apply + $)))

;part 1
(solver input 80)
;part 2
(solver input 256)