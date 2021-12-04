(ns day03
  (:require [clojure.string :refer [join split starts-with?]]))

(def input (split (slurp "./src/main/clojure/day03.in") #"\n"))

(defn to-decimal [bits] (Integer/parseInt bits 2))

(defn to-int-list [report] (map #(if (= % \1) 1 0) report))

(defn count-ones [reports]
  (reduce #(map + %1 %2)
          (repeat (count (first reports)) 0)
          (map to-int-list reports)))

(defn calc-gamma [reports]
  (->> (count-ones reports)
       (map #(if (> (/ % (count reports)) 1/2) 1 0))
       (join)))

(defn calc-life-support-rating [reports [bca bcb]]
  (loop [bits ""
         index 0
         remaining-reports reports]
    (let [ones (nth (count-ones remaining-reports) index)
          new-bits (join [bits (if (< (/ ones (count remaining-reports)) 1/2) bca bcb)])
          filtered-reports (filter #(starts-with? % new-bits) reports)]
      (if (= 1 (count filtered-reports))
        (first filtered-reports)
        (recur new-bits (inc index) filtered-reports)))))

(defn part-1 [input]
  (let [gamma (calc-gamma input)
        epsilon (join (map #(if (= \0 %) "1" "0") gamma))]
    (* (to-decimal gamma) (to-decimal epsilon))))

(defn part-2 [input]
  (->> '(["1" "0"] ["0" "1"])
       (map (partial calc-life-support-rating input))
       (map to-decimal)
       (apply *)))

(part-1 input)
(part-2 input)