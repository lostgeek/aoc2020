(ns aoc2020.day06
  "AOC 2020 Day 6"
  (:require [clojure.string :as string])
  (:gen-class))

(defn get-data
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n")))
        empty-lines (concat [-1] (keep-indexed #(if (= "" %2) %1) data) [(count data)])
        slices (partition 2 1 empty-lines)]
    (for [s slices]
      (let [forms (subvec data (inc (first s)) (second s))]
        (for [form forms]
          (set (char-array form)))))))

(defn part1
  [groups]
  (apply +
         (for [g groups]
           (count (filter identity
                          (for [c (map char (range (int \a) (inc (int \z))))]
                            (pos? (count (filter #(contains? % c) g)))))))))

(defn part2
  [groups]
  (apply +
         (for [g groups]
           (count (filter identity
                          (for [c (map char (range (int \a) (inc (int \z))))]
                            (= (count g) (count (filter #(contains? % c) g)))))))))
(defn main
  [& args]
  (let [groups (get-data "resources/day06.txt")]
    (str "part 1: " (part1 groups) "\n"
         "part 2: " (part2 groups))
  ))
