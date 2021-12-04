(ns day04
  (:require [clojure.string :refer [join split starts-with? trim]]))

(def input (split (slurp "./src/main/clojure/day04.in") #"\n"))

(defn parse-randoms [randoms]
  (->> (split randoms #",")
       (map #(Integer/parseInt %))))

(defn transpose [x]
  (apply map list x))

(defn parse-row [row]
  (map #(Integer/parseInt %) (split (trim row) #"[ ]+")))

(defn parse-board [board]
  (let [parsed-rows (map parse-row board)
        parsed-cols (transpose parsed-rows)]
    (concat parsed-rows parsed-cols)))

(defn parse-boards [boards]
  (->> (filter #(not= % '("")) (partition-by #(= % "") boards))
       (map parse-board)))

(defn update-board [number board]
  (map (fn [col-or-row] (filter (fn [x] (not= number x)) col-or-row)) board))

(defn bingo? [board]
  (->> (map (fn [x] (= 0 (count x))) board)
       (some true?)
       (boolean)))

(defn score [number board]
  (* number (->> (flatten board)
                 (set)
                 (apply +))))

(defn part-1 [input]
  (loop [[number & xs] (parse-randoms (first input))
         boards (parse-boards (rest input))]
    (let [updated-boards (map #(update-board number %) boards)
          boards-with-bingo (filter bingo? updated-boards)]
      (if (= 0 (count boards-with-bingo))
        (recur xs updated-boards)
        (score number boards-with-bingo)))))

(defn part-2 [input]
  (loop [[number & xs] (parse-randoms (first input))
         boards (parse-boards (rest input))]
    (let [updated-boards (map #(update-board number %) boards)
          boards-without-bingo (filter (complement bingo?) updated-boards)]
      (if (not= 0 (count boards-without-bingo))
        (recur xs boards-without-bingo)
        (score number updated-boards)))))

(part-1 input)
(part-2 input)