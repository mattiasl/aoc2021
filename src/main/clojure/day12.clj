(ns day12
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [union]]))

(def input (-> (slurp "./src/main/clojure/input/day12.in")
               (split #"\n")))

(defn parse-row [state row]
  (let [[a b] (split row #"-")]
    (-> (reduce (fn [graph [from to]]
                  (if (= to "start")
                    graph
                    (if (contains? graph from)
                      (update graph from (fn [from to] (conj from to)) to)
                      (assoc graph from #{to})))
                  ) state [[a b] [b a]])
        (dissoc "end"))))

(defn small? [x]
  (not (nil? (re-matches #"[a-z]+" x))))

(defn small-once [neighbour path]
  (contains? (set (filter small? path)) neighbour))

(defn one-small-twice [neighbour path]
  (let [freq (frequencies (filter small? path))
        visits (get freq neighbour)]
    (and (not (nil? visits))
         (some #(= % 2) (vals freq)))))

(defn walk [graph active-paths can-visit?]
  (reduce (fn [paths path]
            (let [node (last path)
                  neighbors (get graph node)]
              (reduce (fn [a neighbor]
                        (if (can-visit? neighbor path)
                          a
                          (conj a (conj path neighbor))))
                      paths
                      neighbors)))
          #{}
          active-paths))

(defn solver [input pred]
  (let [graph (reduce (fn [a c] (parse-row a c)) {} input)]
    (->> (loop [active #{["start"]} paths #{}]
           (let [walked (walk graph active pred)
                 ended (filter #(= "end" (last %)) walked)]
             (if (empty? walked)
               paths
               (recur (disj walked ended) (apply merge paths ended)))))
         (count))))

(time (solver input small-once))
(time (solver input one-small-twice))