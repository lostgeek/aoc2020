(ns aoc2020.day17
  "AOC 2020 Day 17"
  (:require [clojure.string :as string])
  (:require [clojure.set :as sets])
  (:gen-class))

(defn get-data
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n")))]
    [(vec (map vec data))]))

(defn disp
  [grid]
  (for [slice grid]
    (println (string/join "\n" (map #(apply str %) slice))
             "\n")))

(defn count-neighbors
  [grid x y z]
  (count (remove #(= \. %)
                 (for [dz (range -1 2)
                       dy (range -1 2)
                       dx (range -1 2)
                       :when (not (and (zero? dz)
                                       (zero? dy)
                                       (zero? dx)))]
                   (get-in grid [(+ z dz) (+ y dy) (+ x dx)] \.)))))

(defn next-step
  [grid]
  (vec (for [z (range -1 (inc (count grid)))]
         (vec (for [y (range -1 (inc (count (first grid))))]
                (vec (for [x (range -1 (inc (count (ffirst grid))))]
                       (let [neigh (count-neighbors grid x y z)
                             cube (get-in grid [z y x] \.)]
                         (cond
                           (and (= \# cube)
                                (or (= 2 neigh) (= 3 neigh)))
                           \#

                           (and (= \. cube)
                                (= 3 neigh))
                           \#

                           :else
                           \.)))))))))

(defn part1
  [data]
  (count (remove #(= \. %) (flatten (nth (iterate next-step data) 6)))))

(defn count-neighbors-4d
  [grid x y z w]
  (count (remove #(= \. %)
                 (for [dw (range -1 2)
                       dz (range -1 2)
                       dy (range -1 2)
                       dx (range -1 2)
                       :when (not (and (zero? dw)
                                       (zero? dz)
                                       (zero? dy)
                                       (zero? dx)))]
                   (get-in grid [(+ w dw) (+ z dz) (+ y dy) (+ x dx)] \.)))))

(defn next-step-4d
  [grid]
  (vec (for [w (range -1 (inc (count grid)))]
         (vec (for [z (range -1 (inc (count (first grid))))]
                (vec (for [y (range -1 (inc (count (ffirst grid))))]
                       (vec (for [x (range -1 (inc (count (first (ffirst grid)))))]
                              (let [neigh (count-neighbors-4d grid x y z w)
                                    cube (get-in grid [w z y x] \.)]
                                (cond
                                  (and (= \# cube)
                                       (or (= 2 neigh) (= 3 neigh)))
                                  \#

                                  (and (= \. cube)
                                       (= 3 neigh))
                                  \#

                                  :else
                                  \.)))))))))))

(defn part2
  [data]
  (count (remove #(= \. %) (flatten (nth (iterate next-step-4d [data]) 6)))))

(defn main
  [& args]
  (let [data (get-data "resources/day17.txt")]
    (str "part 1: " (time (part1 data)) "\n"
         "part 2: " (time (part2 data)))))
