(ns day08
  (:require [clojure.string :refer [split trim]]
            [clojure.set :refer [intersection map-invert]]))

(def input (-> (slurp "./src/main/clojure/input/day08.in")
               (split #"\n")))

(defn parse-note [note]
  (->> (map trim (split note #"\|"))
       (map #(split % #" "))))

(defn one? [x _]
  (= (count x) 2))

(defn four? [x _]
  (= (count x) 4))

(defn seven? [x _]
  (= (count x) 3))

(defn eight? [x _]
  (= (count x) 7))

(defn six? [x state]
  (let [four (get state 4)
        one (get state 1)]
    (and (= (count x) 6)
         (= (count (intersection x one)) 1)
         (= (count (intersection x four)) 3))))

(defn nine? [x state]
  (let [four (get state 4)]
    (and (= (count x) 6)
         (= (intersection x four) four))))

(defn three? [x state]
  (let [one (get state 1)]
    (and (= (count x) 5)
         (= (intersection x one) one))))

(defn five? [x state]
  (let [nine (get state 9)]
    (and (= (count x) 5)
         (= (count (intersection x nine)) 5))))

(defn two? [x state]
  (and (= (count x) 5)
       (not (five? x state))))

(defn nought? [x state]
  (and (= (count x) 6)
       (not (nine? x state))
       (not (six? x state))))

(def deduce-order [{8 eight?}
                   {1 one?}
                   {4 four?}
                   {7 seven?}
                   {9 nine?}
                   {3 three?}
                   {0 nought?}
                   {6 six?}
                   {2 two?}
                   {5 five?}])

(defn deduce [note]
  (loop [signal-pattern note
         state {}
         [x & xs] deduce-order]
    (if (nil? x)
      (map-invert state)
      (let [id (first (keys x))
            pred (get x id)
            current (first (filter #(pred % state) signal-pattern))]
        (recur
          (remove #(= current %) signal-pattern)
          (assoc state id current)
          xs)))))

(defn note->digits [note]
  (let [part (parse-note note)
        deduced (deduce (map set (first part)))]
    (reduce (fn [a c] (+ (* 10 a) (get deduced (set c)))) 0 (second part))))

(defn part-1 [input]
  (reduce (fn [a [_ output]]
            (->> output
                 (filter (fn [x]
                           (or (one? x {})
                               (four? x {})
                               (seven? x {})
                               (eight? x {}))))
                 (count)
                 (+ a)))
          0
          (map parse-note input)))

(defn part-2 [input]
  (->> (map note->digits input)
       (apply +)))

(part-1 input)
(part-2 input)