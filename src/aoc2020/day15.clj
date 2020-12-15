(ns aoc2020.day15
  "AOC 2020 Day 15"
  (:require [clojure.string :as string])
  (:gen-class))

(defn get-data
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #","))
                 (#(map read-string %))
                 vec)]
    data))

(defn next-num
  [history]
  (let [rev (reverse history)
        n (first rev)
        prev (filter #(= n (second %)) (map-indexed vector rev))]
    (if (nil? (second prev))
      0
      (- (first (second prev)) (first (first prev))))))

(defn next-step
  [history]
  (conj history (next-num history)))

(defn part1
  [data]
  ; (seq (take 20 (nth (iterate next-step data) (- 2020 (count data))))))
  (last (nth (iterate next-step data) (- 2020 (count data)))))

(defn prepare-history
  [data]
  (apply merge
         (map-indexed (fn [ind x]
                        {x {:prev nil :recent ind}})
                      data)))

(defn update-history
  [history n i]
  (let [entry (get history n)]
    (merge history {n {:prev (:recent entry)
                       :recent i}})))

(defn part2
  [data]
  (loop [history (prepare-history data)
         latest (last data)
         i (count data)]
    ; (when (zero? (mod i 100000)) (println (float (/ i 30000000))))
    (if (= i 30000000)
      latest
      (let [entry (get history latest)
            diff (if (nil? (:prev entry))
                   0
                   (- (:recent entry) (:prev entry)))]
        (recur (update-history history diff i)
               diff
               (inc i))))))

(defn main
  [& args]
  (let [data (get-data "resources/day15.txt")]
    (str "part 1: " (time (part1 data)) "\n"
         "part 2: " (time (part2 data)))))
