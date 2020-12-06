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
        empty-lines (concat [-1]
                            (keep-indexed #(if (= "" %2) %1) data)
                            [(count data)])
        slices (partition 2 1 empty-lines)]
    (for [s slices]
      (let [forms (subvec data (inc (first s)) (second s))]
        (map #(set (char-array %)) forms)))))

(def questions (map char (range (int \a) (inc (int \z)))))

(defn check-group-1
  [group]
  (let [checks
        (for [q questions]
          (pos? (count (filter #(contains? % q) group))))]
    (count (filter identity checks))))

(defn check-group-2
  [group]
  (let [checks
        (for [q questions]
          (= (count group)
             (count (filter #(contains? % q) group))))]
    (count (filter identity checks))))

(defn part1
  [groups]
  (apply + (map check-group-1 groups)))

(defn part2
  [groups]
  (apply + (map check-group-2 groups)))

(defn main
  [& args]
  (let [groups (get-data "resources/day06.txt")]
    (str "part 1: " (part1 groups) "\n"
         "part 2: " (part2 groups))
  ))
