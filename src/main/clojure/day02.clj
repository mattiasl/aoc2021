(ns day02
  (:require [ysera.test :refer [is=]])
  (:require [clojure.string :refer [split]]))

(def input (map #(let [parts (split % #" ")
                       cmd (first parts)
                       val (Integer/parseInt (second parts))]
                   [cmd val])
                (split (slurp "./src/main/clojure/day02.in") #"\n")))

(defn map-cmd-part-1 [[cmd x]]
  (cond
    (= cmd "forward") [x 0]
    (= cmd "down") [0 x]
    :else [0 (- x)]))

(defn part-1 [commands]
  (->> (map map-cmd-part-1 commands)
       (reduce #(map + %1 %2))
       (apply *)))

(defn map-cmd-part-2 [[cmd x] aim]
  (cond
    (= cmd "forward") [x (* x aim) aim]
    (= cmd "down") [0 0 (+ aim x)]
    :else [0 0 (- aim x)]))

(defn part-2 [commands]
  (->> (loop [remaining-commands commands
              state [0 0]
              aim 0]
         (if-let [command (first remaining-commands)]
           (let [[c x a] (map-cmd-part-2 command aim)]
             (recur (rest remaining-commands) (map + state [c x]) a))
           state))
       (apply *)))

(part-1 input)
(part-2 input)