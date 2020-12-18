(ns aoc2020.day15
  "AOC 2020 Day 15"
  (:require [clojure.string :as string])
  (:gen-class))

(defn get-data
  [f]
  (let [data (->> f
                 slurp
                 string/trim
                 (#(string/split % #","))
                 (map read-string)
                 vec)]
    data))

(defn prepare-history
  [data]
  (into (hash-map) (apply merge
                          (map-indexed (fn [ind x]
                                         {x ind})
                                       data))))

(defn find-nth
  [data n]
  (loop [history (prepare-history data)
         latest (last data)
         i (count data)]
    ; (when (zero? (mod i 1000000)) (println (float (/ i n))))
    (if (= i n)
      latest
      (let [entry (get history latest)
            diff (if (nil? entry)
                   0
                   (- (dec i) entry))]
        (recur (assoc history latest (dec i))
               diff
               (inc i))))))

(defn part1
  [data]
  (find-nth data 2020))

(defn part2
  [data]
  (find-nth data 30000000))

(defn main
  [& args]
  (let [data (get-data "resources/day15.txt")]
    (str "part 1: " (time (part1 data)) "\n"
         "part 2: " (time (part2 data)))))
