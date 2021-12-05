(ns day04
  (:require [clojure.string :refer [join split starts-with? trim]]))

(def input (split (slurp "./src/main/clojure/day04.in") #"\n"))

(defn parse-randoms [randoms]
  (->> (split randoms #",")
       (map #(Integer/parseInt %))))

(defn transpose [x]
  (apply map list x))

(defn parse-row [row]
  (->> (split (trim row) #"\s+")
       (map #(Integer/parseInt %))))

(defn parse-board [board]
  (let [parsed-rows (map parse-row board)
        parsed-cols (transpose parsed-rows)]
    (concat parsed-rows parsed-cols)))

(defn parse-boards [boards]
  (->> (partition-by #(= % "") boards)
       (filter #(not= % '("")))
       (map parse-board)))

(defn update-board [number board]
  (map (fn [col-or-row] (filter #(not= number %) col-or-row)) board))

(defn bingo? [board]
  (->> (map empty? board)
       (some true?)
       (boolean)))

(defn score [number board]
  (->> (flatten board)
       (set)
       (apply +)
       (* number)))

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