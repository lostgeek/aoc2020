(ns aoc2020.day13
  "AOC 2020 Day 13"
  (:require [clojure.string :as string])
  (:gen-class))

(defn parse
  [entry]
  (if (= "x" entry)
    :x
    (read-string entry)))

(defn get-data
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n")))]
    {:time (read-string (first data))
     :busses (map parse (string/split (second data) #","))}))

(defn find-time
  [bus min-time]
  (first (filter #(<= min-time %) (iterate (partial + bus) 0))))

(defn part1
  [notes]
  (let [busses (remove #(= :x %) (:busses notes))
        [ind arrival] (apply min-key second
                             (map-indexed vector (map #(find-time % (:time notes)) busses)))
        bus (nth busses ind)]
    (* (- arrival (:time notes)) bus)))

(defn create-timetable
  [busses zerobus]
  (remove #(= :x (second %)) (map-indexed vector busses)))

(defn part2
  [notes min-t]
  (let [max-bus (apply max (remove #(= :x %) (:busses notes)))
        busses (create-timetable (:busses notes) max-bus)
        offset (first (first (filter #(= max-bus (second %)) busses)))]
    (loop [t (- (* max-bus (long (Math/ceil (/ min-t max-bus))))
                offset)
           cnt 0]
      (when (zero? (mod cnt 100000))
        (println t))
      (if (every? identity
                  (for [[dt bus] busses]
                    (= (mod (- dt) bus) (mod t bus))))
        t
        (recur (+ t max-bus) (inc cnt))))))
(defn main
  [& args]
  (let [data (get-data "resources/day13-test-4.txt")
        min-t 0]
    (str "part 1: " (time (part1 data)) "\n"
         "part 2: " (time (part2 data min-t)))))
