(ns aoc2020.day01
  "AOC 2020 Day 1"
  (:require [clojure.string :as string])
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn get-data
  [f]
  (-> f
      slurp
      string/trim
      (#(string/split % #"\n"))
      (#(map read-string %))))

(defn find-combo
  [data n]
  (let [pairs (combo/combinations data n)
        pair (first (filter #(= 2020 (apply + %)) pairs))
        result (apply * pair)]
    result))

(defn main
  [& args]
  (let [data (get-data "resources/day01.txt")
        res1 (find-combo data 2)
        res2 (find-combo data 3)]
    (str "part 1: " res1 "\npart 2: " res2)))
