(ns day01
  (:require [ysera.test :refer [is=]])
  (:require [clojure.string :refer [split]]))

(def input (mapv #(Integer/parseInt %) (split (slurp "./src/main/clojure/day01.in") #"\n")))

(def test-input [199 200 208 210 200 207 240 269 260 263])

(defn solve [measurements offset]
  {:test (fn []
           (is= (solve test-input 2) 7)
           (is= (solve test-input 4) 5))}
  (->> (partition offset 1 measurements)
       (filter #(< (first %) (last %)))
       (count)))

(solve input 2)
(solve input 4)