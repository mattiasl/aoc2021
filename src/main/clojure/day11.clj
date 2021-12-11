(ns day11
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [difference union]]))

(def input (-> (slurp "./src/main/clojure/input/day11.in")
               (split #"\n")))

(defn to-int-vec [row]
  (mapv (fn [x] (Character/digit x 10)) row))

(defn input->grid [input]
  (let [xm (count (first input))
        ym (count input)
        energy (mapv to-int-vec input)]
    (->> (for [y (range ym) x (range xm)] [x y])
         (reduce (fn [a [x y]] (assoc a [x y] (get (get energy y) x))) {}))))

(defn get-neighbors [xm ym [x y]]
  (let [dirs [[0 -1] [1 0] [0 1] [-1 0] [-1 -1] [1 -1] [1 1] [-1 1]]]
    (filter (fn [[x y]] (and (< -1 x xm) (< -1 y ym)))
            (mapv #(mapv + [x y] %) dirs))))

(defn increase-energy
  ([grid] (increase-energy grid (keys grid)))
  ([grid positions] (merge grid (reduce (fn [acc pos]
                                          (let [energy (or (get acc pos) (get grid pos))]
                                            (assoc acc pos (inc energy)))) {} positions))))

(defn flash? [flashed [octopus energy]]
  (and (> energy 9) (not (contains? flashed octopus))))

(defn flash [octopuses grid flashed]
  (let [neighbors (->> (map #(get-neighbors 10 10 %) octopuses)
                       (reduce (fn [acc pos] (concat acc pos)) [])
                       (filter #(not (contains? flashed %))))]
    (increase-energy grid neighbors)))

(defn reset-flashed [octopuses]
  (reduce (fn [grid [pos energy]] (assoc grid pos (if (> energy 9) 0 energy))) {} octopuses))

(defn do-step [state]
  (let [updated-grid (increase-energy state)]
    (loop [flashed #{} grid updated-grid]
      (let [flashing (->> (filter #(flash? flashed %) grid)
                          (map #(first %)))]
        (if (empty? flashing)
          [(reset-flashed grid) (count flashed)]
          (recur
            (reduce #(conj %1 %2) flashed flashing)
            (flash flashing grid flashed)))))))

(defn part-1 [input]
  (->> (reduce (fn [[grid flashes] _]
                 (let [[updated-grid new-flashes] (do-step grid)]
                   [updated-grid (+ flashes new-flashes)])) [(input->grid input) 0] (range 100))
       (last)))

(defn part-2 [input]
  (let [octopuses (input->grid input)]
    (loop [o octopuses c 0 i 0]
      (if (= (count o) c)
        i
        (let [[state flashes] (do-step o)]
          (recur state flashes (inc i)))))))

(part-1 input)
(part-2 input)