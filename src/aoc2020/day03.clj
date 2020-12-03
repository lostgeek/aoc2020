(ns aoc2020.day03
  "AOC 2020 Day 3"
  (:require [clojure.string :as string])
  (:gen-class))

(defn get-data
  [f]
  (-> f
      slurp
      string/trim
      (#(string/split % #"\n"))))

(defn check
  [data right down]
  (let [data (take-nth down data)]
    (apply + (for [i (range (count data))]
               (let [row (nth data i)]
                 (if (= \# (nth row (mod (* right i) (count row))))
                   1
                   0))))))

(defn part01
  [data]
  (check data 3 1))

(defn part02
  [data]
  (* (check data 1 1)
     (check data 3 1)
     (check data 5 1)
     (check data 7 1)
     (check data 1 2)))

(defn main
  [& args]
  (let [data (get-data "resources/day03.txt")]
    (str "part 1: " (part01 data) "\n"
         "part 2: " (part02 data))))
