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

(defn create-timetables
  [busses min-t]
  (vec (for [bus busses]
         (if (= :x bus)
           (repeat :x)
           (iterate (partial + bus)
                    (* bus (long (Math/floor (/ min-t bus)))))))))

(defn next-step
  [b]
  (loop [busses b
         i 0]
    (cond
      (= i (count busses))
      busses

      (zero? i)
      (recur (update busses i rest) (inc i))

      (or (= :x (first (nth busses i)))
              (> (first (nth busses i)) (first (first busses))))
      (recur busses (inc i))

      :else
      (recur (update busses i rest) i))))

(defn check
  [busses]
  (apply = (remove #(= :x %) (map (fn [x] (if (= :x (second x))
                          :x
                          (- (second x) (first x))))
                  (map-indexed vector (map first busses))))))


(defn part2
  [notes min-t]
  (loop [busses (create-timetables (:busses notes) min-t)
         cnt 0]
    (let [busses (next-step busses)]
      (when (zero? (mod cnt 100000))
        (println cnt "--" (map first busses)))
      (if (check busses)
        (seq (map first busses))
        (recur busses (inc cnt))))))

(defn main
  [& args]
  (let [data (get-data "resources/day13.txt")
        min-t 100000000000000]
    (str "part 1: " (time (part1 data)) "\n"
         "part 2: " (time (part2 data min-t)))))
