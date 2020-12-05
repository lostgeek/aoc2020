(ns aoc2020.day05
  "AOC 2020 Day 5"
  (:require [clojure.string :as string])
  (:gen-class))

(defn get-data
  [f]
  (-> f
      slurp
      string/trim
      (#(string/split % #"\n"))))

(defn find-seat
  [pass row low high]
  (let [curr (first pass)
        pass (rest pass)
        diff (inc (- high low))
        low (if (= \R curr)
              (+ low (/ diff 2))
              low)
        high (if (= \L curr)
               (- high (/ diff 2))
               high)]
    (if (= 2 diff)
      (+ (* 8 row) low)
      (find-seat pass row low high))))

(defn find-row
  [pass low high]
  (let [curr (first pass)
        pass (rest pass)
        diff (inc (- high low))
        low (if (= \B curr)
              (+ low (/ diff 2))
              low)
        high (if (= \F curr)
               (- high (/ diff 2))
               high)]
    (if (= 2 diff)
      low
      (find-row pass low high))))

(defn parse
  [pass]
  (let [row (find-row pass 0 127)]
    (find-seat (take-last 3 pass) row 0 7)))

(defn part1
  [passes]
  (apply max (map parse passes)))

(defn part2
  [passes]
  (let [pairs (partition 2 1 (sort (map parse passes)))]
    (inc (first (first (filter #(= -2 (apply - %)) pairs))))))

(defn main
  [& args]
  (let [data (get-data "resources/day05.txt")]
    (str "part 1: " (part1 data) "\n"
         "part 2: " (part2 data))))
