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
  [busses]
  (remove #(= :x (second %)) (map-indexed vector busses)))

(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn find-dt
  [busses i goal]
  (let [busses (map second busses)
        one-dt (apply * (vec-remove (vec busses) i))
        bus (nth busses i)
        mods (map #(mod (* one-dt %) bus) (range bus))]
    (* one-dt (first (first (filter #(= goal (second %)) (map-indexed vector mods)))))))

(defn part2
  [notes min-t]
  (let [busses (create-timetable (:busses notes))]
    (loop [t 0
           i 0]
      (let [[ind bus] (nth busses i)
            goal (mod (- ind) bus)
            dt (find-dt busses i goal)
            t (+ t dt)]
        (if (< (inc i) (count busses))
          (recur t (inc i))
          (mod t (apply * (map second busses))))))))

(defn main
  [& args]
  (let [data (get-data "resources/day13.txt")
        min-t 0]
    (str "part 1: " (time (part1 data)) "\n"
         "part 2: " (time (part2 data min-t)))))
