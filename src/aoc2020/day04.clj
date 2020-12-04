(ns aoc2020.day04
  "AOC 2020 Day 4"
  (:require [clojure.string :as string])
  (:gen-class))

(defn get-data
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n")))
        empty-lines (concat [0] (keep-indexed #(if (= "" %2) %1) data) [(count data)])
        slices (partition 2 1 empty-lines)]
    (for [s slices]
      (string/join " " (subvec data (first s) (second s))))))

(defn parse-passport
  [data]
  (apply merge
         (for [entry (string/split data #" ")]
           (if-let [match (re-matches #"(.+):(.+)" entry)]
             {(keyword (nth match 1)) (nth match 2)}))))

(defn is-passport
  [p]
  (if (and (:byr p)
           (:iyr p)
           (:eyr p)
           (:hgt p)
           (:hcl p)
           (:ecl p)
           (:pid p))
    true false))

(defn validate-passport
  [p]
  (if (and 
        (<= 1920 (read-string (:byr p "0")))
        (>= 2002 (read-string (:byr p "0")))

        (<= 2010 (read-string (:iyr p "0")))
        (>= 2020 (read-string (:iyr p "0")))

        (<= 2020 (read-string (:eyr p "0")))
        (>= 2030 (read-string (:eyr p "0")))

        (<= 4 (count (:hgt p "")))
        (let [unit (subs (:hgt p)
                         (- (count (:hgt p)) 2)
                         (count (:hgt p)))
              number (read-string 
                       (subs (:hgt p)
                             0
                             (- (count (:hgt p)) 2)))]
          (case unit
            "cm" (and (<= 150 number)
                      (>= 193 number))
            "in" (and (<=  59 number)
                      (>=  76 number))
            false))

        (re-matches #"#[0-9a-f]{6}" (:hcl p ""))

        (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} (:ecl p))

        (re-matches #"[0-9]{9}" (:pid p "")))
    true false))

(defn part1
  [passports]
  (count (filter identity (map is-passport passports))))

(defn part2
  [passports]
  (count (filter identity (map validate-passport passports))))

(defn main
  [& args]
  (let [data (get-data "resources/day04.txt")
        passports (map parse-passport data)]
    (str "part 1: " (part1 passports) "\n"
         "part 2: " (part2 passports))))
