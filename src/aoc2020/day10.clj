(ns aoc2020.day10
  "AOC 2020 Day 10"
  (:require [clojure.string :as string])
  (:gen-class))

(defn get-data
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n")))]
      (map read-string data)))

(defn part1
  [data]
  (let [data (conj data 0 (+ 3 (apply max data)))
        freq (frequencies (map #(apply - %) (partition 2 1 (sort data))))]
    (* (get freq -1) (get freq -3))))

(defn part2
  [d]
  (let [d (conj d 0 (+ 3 (apply max d)))]
    (loop [data (rest (sort d))
           ways {(first (sort d)) 1}]
      (let [n (first data)
            data (rest data)
            ways (assoc ways n (+ (get ways (- n 1) 0)
                                  (get ways (- n 2) 0)
                                  (get ways (- n 3) 0)))]
        (if (not-empty data)
          (recur data ways)
          (get ways n))))))

(defn main
  [& args]
  (let [data (get-data "resources/day10.txt")]
    (str "part 1: " (part1 data) "\n"
         "part 2: " (part2 data))))
