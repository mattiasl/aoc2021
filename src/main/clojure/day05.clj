(ns day05
  (:require [clojure.string :refer [split]]))

(def input
  (-> (slurp "./src/main/clojure/day05.in")
      (split #"\n")))

(defn line->coords [line]
  (let [parsed (rest (re-matches #"(\d+),(\d+) \-\> (\d+),(\d+)" line))
        [x1 y1 x2 y2] (mapv #(Integer/parseInt %) parsed)]
    [[x1 y1] [x2 y2]]))

(defn not-diagonal? [[[x1 y1] [x2 y2]]]
  (or (and (= x1 x2) (not= y1 y2))
      (and (not= x1 x2) (= y1 y2))))

(defn coords->gradient [[[x1 y1] [x2 y2]]]
  (cond (and (neg? (- x1 x2)) (neg? (- y1 y2))) [1 1]
        (and (neg? (- x1 x2)) (pos? (- y1 y2))) [1 -1]
        (and (pos? (- x1 x2)) (neg? (- y1 y2))) [-1 1]
        (and (pos? (- x1 x2)) (pos? (- y1 y2))) [-1 -1]
        (and (= x1 x2) (pos? (- y1 y2))) [0 -1]
        (and (= x1 x2) (neg? (- y1 y2))) [0 1]
        (and (pos? (- x1 x2)) (= y1 y2)) [-1 0]
        (and (neg? (- x1 x2)) (= y1 y2)) [1 0]))

(defn update-diagram [diagram [[x1 y1] [x2 y2]]]
  (let [len (max (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))
        [dx dy] (coords->gradient [[x1 y1] [x2 y2]])]
    (->> (for [r (range (inc len))]
           (let [coords [(+ x1 (* dx r)) (+ y1 (* dy r))]
                 new-val (inc (or (get diagram coords) 0))]
             [coords new-val]))
         (reduce (fn [a [k v]] (assoc a k v)) diagram))))

(defn solver
  ([lines] (solver lines (constantly true)))
  ([lines pred] (->> (map line->coords lines)
                     (filter pred)
                     (reduce update-diagram {})
                     (vals)
                     (filter #(<= 2 %))
                     (count))))

; part-1
(solver input not-diagonal?)
; part-2
(solver input)