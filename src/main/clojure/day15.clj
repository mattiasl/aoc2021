(ns day15
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [union]]
            [clojure.data.priority-map :refer [priority-map]]))

(def input (-> (slurp "./src/main/clojure/input/day15.in")
               (split #"\n")))

(def inf (Integer/MAX_VALUE))

(defn risk-at-pos [risk-levels [x y] [xm ym]]
  (let [risk-level (Character/digit (get (get risk-levels (mod y xm)) (mod x ym)) 10)]
    (->> [(+ (quot x xm) (quot y ym) risk-level) 10]
         (apply (juxt mod quot))
         (apply +))))

(defn input->risk-levels [input factor]
  (let [xm (count (first input))
        ym (count input)]
    [(->> (for [y (range (* factor ym)) x (range (* factor xm))] [x y])
          (reduce (fn [a [x y]] (assoc a [x y] (risk-at-pos input [x y] [xm ym]))) {}))
     [(dec (* factor xm)) (dec (* factor ym))]]))

(defn get-neighbours [[xm ym] vertex]
  (let [dirs [[0 -1] [1 0] [0 1] [-1 0]]]
    (->> (reduce (fn [a dir] (conj a (mapv + vertex dir))) [] dirs)
         (filter (fn [[x y]] (and (<= 0 x xm) (<= 0 y ym)))))))

(defn backtrack [target previous]
  (loop [s '() u target]
    (if (nil? u)
      s
      (recur (conj s u) (get previous u)))))

(defn dijkstra [graph source target size]
  (loop [q (priority-map source 0)
         state {:distance {source 0}
                :previous {}}]
    (let [[u _] (peek q)
          q' (pop q)]
      (if (= u target)
        (backtrack target (get state :previous))
        (let [result (reduce (fn [a neighbour]
                               (let [alt (+ (or (get (get state :distance) u) inf) (get graph neighbour))
                                     dist (or (get (get state :distance) neighbour) inf)]
                                 (if (< alt dist)
                                   (merge-with union a {:distance {neighbour alt} :previous {neighbour u}})
                                   a)))
                             {:distance {} :previous {}}
                             (get-neighbours size u))]
          (recur (into q' (get result :distance)) (merge-with union state result)))))))

(defn solver [input factor]
  (let [[graph target] (input->risk-levels input factor)]
    (->> (dijkstra graph [0 0] target target)
         (map #(get graph %))
         (rest)
         (apply +))))

(time (solver input 1))
(time (solver input 5))