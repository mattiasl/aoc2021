(ns day18
  (:require [clojure.string :refer [index-of last-index-of split]]))

(def input (-> (slurp "./src/main/clojure/input/day18.in")
               (split #"\n")))

(defn str->sfn [sfn]
  (first (mapv clojure.edn/read-string [sfn])))

(defn magnitude [sfn]
  (if (coll? sfn)
    (let [[left right] sfn
          left' (magnitude left)
          right' (magnitude right)]
      (+ (* 3 left') (* 2 right')))
    sfn))

(defn sfn+ [left right]
  [left right])

(defn rn-split [n]
  (let [left (quot n 2)]
    [left (+ left (mod n 2))]))

(defn sfn-split
  ([sfn] (sfn-split sfn (atom false)))
  ([sfn split?]
   (if (coll? sfn)
     (let [[left right] sfn
           left' (sfn-split left split?)
           right' (sfn-split right split?)]
       [left' right'])
     (if (or (< sfn 10) @split?)
       sfn
       (do
         (swap! split? (constantly true))
         (rn-split sfn))))))

(defn find-and-prepare-explode
  ([sfn exploded?] (find-and-prepare-explode sfn exploded? 0))
  ([sfn exploded? depth]
   (if (coll? sfn)
     (let [[left right] (if (and (= depth 4) (nil? @exploded?))
                          (do
                            (swap! exploded? (constantly sfn))
                            [nil nil])
                          sfn)
           left' (find-and-prepare-explode left exploded? (inc depth))
           right' (find-and-prepare-explode right exploded? (inc depth))]
       [left' right'])
     sfn)))

(defn add-to-last-number [left n]
  (clojure.string/replace left #"(\d+)(?!.*\d+)" #(str (+ (Integer/parseInt (last %1)) n))))

(defn add-to-first-number [right n]
  (clojure.string/replace-first right #"(\d+)" #(str (+ (Integer/parseInt (last %1)) n))))

(defn do-explode [partially-exploded explosion]
  (let [partially-exploded-string (str partially-exploded)
        explosion-pos (index-of partially-exploded-string "[nil nil]")
        left (-> (subs partially-exploded-string 0 explosion-pos)
                 (add-to-last-number (first explosion)))
        right (-> (subs partially-exploded-string (+ (count "[nil nil]") explosion-pos))
                  (add-to-first-number (second explosion)))]
    (str->sfn (str left " 0 " right))))

(defn explode [sfn]
  (let [exploded? (atom nil)
        partially-exploded (find-and-prepare-explode sfn exploded?)]
    (if (nil? @exploded?)
      sfn
      (do-explode partially-exploded @exploded?))))

(defn sfn-reduce [sfn]
  (loop [sfn sfn]
    (let [exploded-sfn (explode sfn)]
      (if (not= sfn exploded-sfn)
        (recur exploded-sfn)
        (let [split-sfn (sfn-split sfn)]
          (if (not= sfn split-sfn)
            (recur split-sfn)
            sfn))))))

(defn part-1 [input]
  (->> (map str->sfn input)
       (reduce (fn [a c]
                 (->> (sfn+ a c)
                      (sfn-reduce))))
       (magnitude)))

(defn part-2 [input]
  (->> (for [a input b input]
         (part-1 [a b]))
       (apply max)))

(part-1 input)
(part-2 input)