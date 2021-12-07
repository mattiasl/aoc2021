(ns day07
  (:require [clojure.string :refer [split]]))

(def input (map #(Integer/parseInt %) (split (slurp "./src/main/clojure/input/day07.in") #",")))

(defn fuel-cost-part-1 [^long a ^long b]
  (Math/abs (- a b)))

(defn sum-n [n]
  (/ (* n (inc n)) 2))

(defn fuel-cost-part-2 [a b]
  (sum-n (fuel-cost-part-1 a b)))

(defn solver [input fuel-cost-fn]
(let [freq (frequencies input)]
  (->> (range (apply min input) (inc (apply max input)))
       (map (fn [x] (reduce (fn [a [k v]] (+ a (* v (fuel-cost-fn x k)))) 0 freq)))
       (partition 2 1)
       (some (fn [[a b]] (when (< a b) a))))))

;part 1
(time (solver input fuel-cost-part-1))
;part 2
(time (solver input fuel-cost-part-2))