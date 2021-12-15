(ns day13
  (:require [clojure.string :refer [join split]]))

(def input (split (slurp "./src/main/clojure/input/day13.in") #"\n\n"))

(defn parse-dot [dot]
  (let [parsed (rest (re-matches #"(\d+),(\d+)" dot))]
    (mapv #(Integer/parseInt %) parsed)))

(defn parse-dots [input]
  (map parse-dot (split input #"\n")))

(defn parse-fold [fold]
  (let [parsed (rest (re-matches #"fold along (x|y)=(\d+)" fold))]
    [(first parsed) (Integer/parseInt (second parsed))]))

(defn parse-folds [input]
  (map parse-fold (split input #"\n")))

(defn create-paper [dots]
  (let [width (inc (reduce max (map first dots)))
        height (inc (reduce max (map last dots)))
        cache (set dots)]
    (reduce (fn [rows y]
              (conj rows (reduce (fn [cols x]
                                   (conj cols (if (contains? cache [x y]) 1 0))
                                   ) [] (range width)))
              ) [] (range height))))

(defn transpose [x]
  (apply mapv vector x))

(defn fold-y [paper pos]
  (let [lower-fold (subvec paper (inc pos))
        upper (subvec paper 0 pos)
        unfolded-size (- (count upper) (count lower-fold))
        unfolded (subvec paper 0 unfolded-size)
        upper-fold (subvec upper unfolded-size)]
    (loop [[t & ts] upper-fold [b & bs] (reverse lower-fold) result unfolded]
      (if (nil? t)
        result
        (recur ts bs (conj result (mapv + t b)))))))

(defn fold-x [paper pos]
  (transpose (fold-y (transpose paper) pos)))

(defn fold [paper [axis pos]]
  (if (= axis "x")
    (fold-x paper pos)
    (fold-y paper pos)))

(defn part-1 [input]
  (let [dots (parse-dots (first input))
        paper (create-paper dots)
        folds (parse-folds (second input))]
    (->> (fold paper (first folds))
         (flatten)
         (filter (complement zero?))
         (count))))

(defn part-2 [input]
  (let [dots (parse-dots (first input))
        paper (create-paper dots)
        folds (parse-folds (second input))]
    (->> folds
         (reduce (fn [state a-fold]
                   (fold state a-fold))
                 paper)
         (map (fn [line] (join (map #(if (= 0 %) " " "#") line))))
         (map str))))

(time (part-1 input))
(time (part-2 input))