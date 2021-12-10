(ns day10
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [map-invert]]))

(def input (-> (slurp "./src/main/clojure/input/day10.in")
               (split #"\n")))

(def closing->open {\) \( \> \< \} \{ \] \[})

(defn analyze [type chunk]
  (let [open (set (vals closing->open))]
    (loop [stack '() errors [] [ch & xs] chunk]
      (cond
        (nil? ch) (if (= type "error") errors stack)
        (contains? open ch) (recur (conj stack ch) errors xs)
        (get closing->open ch) (if (= (peek stack) (get closing->open ch))
                                (recur (pop stack) errors xs)
                                (recur stack (conj errors ch) xs))))))

(defn corrupt? [chunk] (analyze "error" chunk))
(defn incomplete? [chunk] (analyze "missing" chunk))

(defn part-1 [input]
  (let [points {\) 3, \] 57, \} 1197, \> 25137}]
    (->> (map corrupt? input)
         (filter (complement empty?))
         (map first)
         (map #(get points %))
         (apply +))))

(defn part-2 [input]
  (let [points {\( 1, \[ 2, \{ 3, \< 4}
        scores (->> (filter #(empty? (corrupt? %)) input)
                    (map incomplete?)
                    (map #(reduce (fn [a ch] (+ (* a 5) (get points ch))) 0 %))
                    (sort))]
    (nth scores (quot (count scores) 2))))

(part-1 input)
(part-2 input)