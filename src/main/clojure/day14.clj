(ns day14
  (:require [clojure.string :refer [join split starts-with? trim]]))

(def input (split (slurp "./src/main/clojure/input/day14.in") #"\n\n"))

(defn parse-rule [rule]
  (let [[[a b] c] (rest (re-matches #"([A-Z]+) -> ([A-Z])" rule))]
    [(str a b) [(str a c) (str c b)]]))

(defn parse-rules [rules]
  (reduce (fn [a [k v]] (assoc a k v)) {} (map parse-rule (split rules #"\n"))))

(defn step [rules polymer]
  (reduce (fn [a [pair amount]]
            (apply merge-with + [a (reduce (fn [a key]
                                             (assoc a key amount))
                                           {} (get rules pair))]))
          {} polymer))

(defn solver [input steps]
  (let [rules (parse-rules (last input))
        polymer (first input)
        first-and-last {(str (first polymer) (last polymer)) 1}]
    (->> (range steps)
         (reduce (fn [state _] (step rules state))
                 (frequencies (map join (partition 2 1 polymer))))
         (merge-with + first-and-last)
         (reduce (fn [a [[x y] z]] (conj a {x z} {y z})) [])
         (apply merge-with +)
         (vals)
         (apply (juxt max min))
         (map #(/ % 2))
         (apply -))))

(time (solver input 10))
(time (solver input 40))