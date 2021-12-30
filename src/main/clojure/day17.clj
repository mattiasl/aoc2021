(ns day17)

(defn hit? [[x y]]
  (and (<= 287 x 309) (<= -76 y -48)))

(defn step [pos [^int dx ^int dy :as dir]]
  [(mapv + pos dir) [(* (compare dx 0) (dec (Math/abs dx))) (dec dy)]])

(defn hits? [velocity]
  (loop [pos [0 0] dir velocity ys []]
    (if (hit? pos)
      (apply max ys)
      (if (or (< (last pos) -76) (< 309 (first pos)))
        nil
        (let [[pos dir] (step pos dir)]
          (recur pos dir (conj ys (last pos))))))))

;no binary search nor trial and error was used for this :)
(time (hits? [24 75]))

(time (->> (for [dy (range -76 100 1) dx (range 1 400 1)]
             (hits? [dx dy]))
           (filter some?)
           (count)))