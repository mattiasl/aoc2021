(ns day02
  (:require [clojure.string :refer [split]]))

(def input (map #(let [parts (split % #" ")
                       cmd (first parts)
                       val (Integer/parseInt (second parts))]
                   [cmd val])
                (split (slurp "./src/main/clojure/day02.in") #"\n")))

(defn exec-fn-part-1 [[cmd x]]
  (cond
    (= cmd "forward") [x 0]
    (= cmd "down") [0 x]
    :else [0 (- x)]))

(defn part-1 [commands]
  (->> (map exec-fn-part-1 commands)
       (reduce #(map + %1 %2))
       (apply *)))

(defn exec-fn-part-2 [cmd x aim]
  (cond
    (= cmd "forward") [x (* x aim) 0]
    (= cmd "down") [0 0 x]
    :else [0 0 (- x)]))

(defn part-2 [commands]
  (->> (reduce (fn [acc [command x]]
                 (mapv + acc (exec-fn-part-2 command x (last acc))))
               [0 0 0]
               commands)
       (drop-last)
       (apply *)))

(part-1 input)
(part-2 input)