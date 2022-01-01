(ns day20
  (:require [clojure.string :refer [join split]]))

(def input (-> (slurp "./src/main/clojure/input/day20.in")
               (split #"\n\n")))

(defn make-int->light-set [image-enhancement-algorithm]
  (->> image-enhancement-algorithm
       (keep-indexed (fn [i x] (when (= x \#) i)))
       (set)))

(defn bits->num [^String bits]
  (Integer/parseInt bits 2))

(defn pixel->bit [image [[x-min x-max] [y-min y-max]] ob-val pixel]
  (let [[x y] pixel]
    (if (contains? image pixel)
      "1"
      (if (and (< x-min x x-max) (< y-min y y-max))
        "0"
        ob-val))))

(defn enhance-pixel [pixel pixel->bit-fn enhance-alg]
  (let [dirs [[-1 -1] [0 -1] [1 -1] [-1 0] [0 0] [1 0] [-1 1] [0 1] [1 1]]]
    (->> (mapv #(mapv + pixel %) dirs)
         (map pixel->bit-fn)
         (join)
         (bits->num)
         (contains? enhance-alg))))

(defn boundaries [image-pixels]
  [(->> (map first image-pixels)
        (apply (juxt min max)))
   (->> (map second image-pixels)
        (apply (juxt min max)))])

(defn image->pixel-set [image]
  (->> (for [y (range (count image)) x (range (count (first image)))]
         [x y])
       (filter (fn [[x y]] (= (get (get image y) x) \#)))
       (set)))

(defn enhance-image [image enhance-alg ob-val]
  (let [[[x-min x-max] [y-min y-max]] (boundaries image)
        ob-pixel-fn (partial pixel->bit image [[(dec x-min) (inc x-max)] [(dec y-min) (inc y-max)]] ob-val)]
    (->> (reduce (fn [enhanced-image y]
                   (reduce (fn [enhanced-image x]
                             (if (enhance-pixel [x y] ob-pixel-fn enhance-alg)
                               (conj! enhanced-image [x y])
                               enhanced-image)
                             ) enhanced-image (range (- x-min 2) (+ x-max 2)))
                   ) (transient #{}) (range (- y-min 2) (+ y-max 2)))
         (persistent!))))

(defn enhance-image-steps [[alg image] steps]
  (let [int->light (make-int->light-set alg)
        state (image->pixel-set (split image #"\n"))]
    (->> (reduce (fn [enhanced-image step]
                   (enhance-image enhanced-image int->light (if (= (mod step 2) 0) "0" "1")))
                 state (range steps))
         (count))))

(enhance-image-steps input 2)
(time (enhance-image-steps input 50))