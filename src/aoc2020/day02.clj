(ns aoc2020.day02
  "AOC 2020 Day 2"
  (:require [clojure.string :as string])
  (:gen-class))

(defn get-data
  [f]
  (letfn [(parse-line [line]
            (let [match (re-matches #"(\d+)-(\d+) (\w): (.*)" line)]
              {:req {:min (read-string (nth match 1))
                     :max (read-string (nth match 2))
                     :char (first (nth match 3))}
               :pw (nth match 4)}))]
        (-> f
            slurp
            string/trim
            (#(string/split % #"\n"))
            (#(map parse-line %)))))

(defn validate-part1
  [{:keys [req pw]}]
  (let [occ (count (filter #(= (:char req) %) pw))]
    (and (<= (:min req) occ)
         (>= (:max req) occ))))

(defn validate-part2
  [{:keys [req pw]}]
  (let [to-check [(get pw (dec (:min req)))
                  (get pw (dec (:max req)))]]
    (= 1 (count (filter #(= (:char req) %) to-check)))))

(defn main
  [& args]
  (let [data (get-data "resources/day02.txt")]
    (str "part 1: " (count (filter validate-part1 data)) "\n"
         "part 2: " (count (filter validate-part2 data)))))
