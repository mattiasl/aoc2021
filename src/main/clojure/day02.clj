(ns day02
  (:require [clojure.string :refer [split]]))

(def input (map #(let [parts (split % #" ")
                       cmd (first parts)
                       val (Integer/parseInt (second parts))]
                   [cmd val])
                (split (slurp "./src/main/clojure/day02.in") #"\n")))

(defn exec-instruction-part-1 [[cmd x]]
  (condp = cmd "forward" [x 0]
               "down" [0 x]
               "up" [0 (- x)]))

(defn part-1 [commands]
  (->> (map exec-instruction-part-1 commands)
       (reduce #(map + %1 %2))
       (apply *)))

(defn exec-instruction-part-2 [cmd x aim]
  (condp = cmd "forward" [x (* x aim) 0]
               "down" [0 0 x]
               "up" [0 0 (- x)]))

(defn part-2 [commands]
  (->> (reduce (fn [acc [command x]]
                 (map + acc (exec-instruction-part-2 command x (last acc))))
               [0 0 0]
               commands)
       (drop-last)
       (apply *)))

(part-1 input)
(part-2 input)