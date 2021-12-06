(ns day06
  (:require [clojure.string :refer [split]]))

(def input (map #(Integer/parseInt %) (split (slurp "./src/main/clojure/day06.in") #",")))

(defn input->initial-state [input]
  (reduce merge
          (reduce merge (for [x (range 9)] {x 0}))
          (frequencies input)))

(defn simulate-day [state]
  (let [hatched (get state 0)
        sixes-and-eights {6 (+ (get state 7) hatched), 8 hatched}]
    (reduce (fn [a c]
              (if (= c 8)
                (merge a sixes-and-eights)
                (assoc a c (get state (inc c)))))
            state
            (range 9))))

(defn solver [input days]
  (as-> (input->initial-state input) $
        (reduce (fn [state _] (simulate-day state)) $ (range days))
        (vals $)
        (apply + $)))

;part 1
(solver input 80)
;part 2
(solver input 256)