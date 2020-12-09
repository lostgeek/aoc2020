(ns aoc2020.day09
  "AOC 2020 Day 9"
  (:require [clojure.string :as string])
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn get-data
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n")))]
      (map read-string data)))

(defn check-numbers
  [data length]
  (for [i (range length (count data))]
    (let [curr (nth data i)
          prev (subvec (vec data) (- i length) i)]
      (or (first (filter #(= curr (apply + %)) (combo/combinations prev 2)))
          curr))))

(defn search-sum
  [data numbers goal]
  (if (empty? data)
    nil
    (let [numbers (conj numbers (first data))
          sum (apply + numbers)
          data (rest data)]
      (if (<= goal sum)
        (if (and (= goal sum)
                 (< 1 (count numbers)))
          numbers nil) ;(println numbers))
        (search-sum data numbers goal)))))

(defn find-contiguous-range
  [data number]
  (first (filter identity
           (for [i (range (count data))]
             (search-sum (nthrest data i) [] number)))))

(defn part1
  [data]
  (first (filter #(not (seq? (second %))) (map-indexed vector (check-numbers data 25)))))

(defn part2
  [data]
  (let [[idx number] (part1 data)
        numbers (find-contiguous-range data number)]
    (+ (apply min numbers) (apply max numbers))))

(defn main
  [& args]
  (let [data (get-data "resources/day09.txt")]
    (str "part 1: " (part1 data) "\n"
         "part 2: " (part2 data))))
