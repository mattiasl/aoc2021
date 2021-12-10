(ns day09
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [difference union]]))

(def input (-> (slurp "./src/main/clojure/input/day09.in")
               (split #"\n")))

(defn to-int-vec [row]
  (mapv (fn [x] (Character/digit x 10)) row))

(defn input->heightmap [input]
  (mapv to-int-vec input))

(defn height-at-pos [heightmap [x y]]
  (or (get (get heightmap y) x) 10))

(defn low? [heightmap [x y]]
  (let [height-by-xy (partial height-at-pos heightmap)
        c (height-by-xy [x y])
        n (height-by-xy [x (dec y)])
        e (height-by-xy [(inc x) y])
        s (height-by-xy [x (inc y)])
        w (height-by-xy [(dec x) y])]
    (and (< c n) (< c e) (< c s) (< c w))))

(defn low-points [heightmap]
  (let [xm (count (get heightmap 0))
        ym (count heightmap)]
    (->> (for [y (range ym) x (range xm)]
           [x y])
         (filter (fn [pos] (low? heightmap pos))))))

(defn get-neighbors [xm ym [x y]]
  (let [dirs [[0 -1] [1 0] [0 1] [-1 0]]]
    (filter
      (fn [[x y]] (and (< -1 x xm) (< -1 y ym)))
      (mapv #(mapv + [x y] %) dirs))))

(defn filter-increasing [pos heightmap neighbors]
  (let [height (height-at-pos heightmap pos)]
    (filter (fn [[x' y']]
              (let [neighbor-height (height-at-pos heightmap [x' y'])]
                (and (not= neighbor-height 9)
                     (< height neighbor-height))))
            neighbors)))

(defn basins [x-max y-max heightmap start]
  (loop [visited #{} backlog #{start}]
    (if (empty? backlog)
      visited
      (let [current (first backlog)
            neighbors (difference (->> (get-neighbors x-max y-max current)
                                       (filter-increasing current heightmap)
                                       (set))
                                  visited)]
        (recur (conj visited current) (union (rest backlog) neighbors))))))

(defn part-1 [input]
  (let [heightmap (input->heightmap input)]
    (->> (low-points heightmap)
         (map (fn [p] (height-at-pos heightmap p)))
         (map inc)
         (apply +))))

(defn part-2 [input]
  (let [heightmap (input->heightmap input)
        basins-fn (partial basins (count (get input 0)) (count input) heightmap)]
    (->> (low-points heightmap)
         (map #(count (basins-fn %)))
         (sort >)
         (take 3)
         (apply *))))

(part-1 input)
(part-2 input)