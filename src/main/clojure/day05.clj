(ns day05
  (:require [clojure.string :refer [split]]))

(def input (-> (slurp "./src/main/clojure/input/day05.in")
               (split #"\n")))

(defn line->coords [line]
  (let [parsed (rest (re-matches #"(\d+),(\d+) \-\> (\d+),(\d+)" line))
        [x1 y1 x2 y2] (map #(Integer/parseInt %) parsed)]
    [[x1 y1] [x2 y2]]))

(defn not-diagonal? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

(defn coords->gradient [[[x1 y1] [x2 y2]]]
  (cond (and (neg? (- x1 x2)) (neg? (- y1 y2))) [1 1]
        (and (neg? (- x1 x2)) (pos? (- y1 y2))) [1 -1]
        (and (pos? (- x1 x2)) (neg? (- y1 y2))) [-1 1]
        (and (pos? (- x1 x2)) (pos? (- y1 y2))) [-1 -1]
        (and (= x1 x2) (pos? (- y1 y2))) [0 -1]
        (and (= x1 x2) (neg? (- y1 y2))) [0 1]
        (and (pos? (- x1 x2)) (= y1 y2)) [-1 0]
        (and (neg? (- x1 x2)) (= y1 y2)) [1 0]))

(defn update-diagram [diagram [[^int x1 ^int y1] [^int x2 ^int y2]]]
  (let [len (max (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))
        [dx dy] (coords->gradient [[x1 y1] [x2 y2]])]
    (->> (for [r (range (inc len))]
           [(+ x1 (* dx r)) (+ y1 (* dy r))])
         (reduce (fn [a c] (update a c #(if % (inc %) 1))) diagram))))

(defn solver
  ([lines] (solver lines (constantly true)))
  ([lines pred] (->> (map line->coords lines)
                     (filter pred)
                     (reduce update-diagram {})
                     (vals)
                     (filter #(<= 2 %))
                     (count))))

; part-1
(time (solver input not-diagonal?))
; part-2
(time (solver input))