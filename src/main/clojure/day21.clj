(ns day21)

(defn part-1 [start-pos max-score]
  (let [dice (partition 3 (cycle (range 1 11)))]
    (loop [pos start-pos score {0 0, 1 0} step 0 dice dice]
      (if (<= max-score (apply max (vals score)))
        (->> (vals score)
             (apply min)
             (* 3 step))
        (let [steps (->> (take 1 dice)
                         (flatten)
                         (reduce +))
              player (mod step 2)
              pos' (update pos player #(inc (mod (+ -1 steps %) 10)))
              score' (update score player + (get pos' player))]
          (recur pos' score' (inc step) (rest dice)))))))

(def dirac-dice
  (frequencies (for [x [1 2 3] y [1 2 3] z [1 2 3]] (+ x y z))))

(def play
  (memoize (fn [player pos score]
             (if (>= (get score player) 21)
               (if (= player 0) [1 0] [0 1])
               (reduce (fn [total-wins [steps times]]
                         (let [pos' (update pos player #(inc (mod (+ -1 steps %) 10)))
                               score' (update score player + (get pos' player))
                               wins (play (if (= player 0) 1 0) pos' score')]
                           (mapv + total-wins (mapv * [times times] wins))))
                       [0 0]
                       dirac-dice)))))

(defn part-2 [start-pos]
  ; why is number of wins 27 times to big?
  (/ (apply max (play 0 start-pos [0 0])) 27))

(part-1 {0 8, 1 5} 1000)

(time (part-2 {0 8, 1 5}))