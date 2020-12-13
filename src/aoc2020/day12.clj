(ns aoc2020.day12
  "AOC 2020 Day 12"
  (:require [clojure.string :as string])
  (:gen-class))

(defn get-data
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n")))]
    (map (fn [l] {:dir (first l) :arg (read-string (subs l 1))}) data)))

(defn turn-left
  [face]
  (case face
    \N \W
    \E \N
    \S \E
    \W \S))

(defn turn-right
  [face]
  (case face
    \N \E
    \E \S
    \S \W
    \W \N))

(defn part1
  [data]
  (loop [dirs data
         pos [0 0]
         face \E]
    (let [dir (:dir (first dirs))
          arg (:arg (first dirs))
          dirs (rest dirs)]
      (println pos face)
      (if (nil? dir)
        (reduce + (map #(Math/abs %) pos))
        (case dir
          \N (recur dirs
                    [(first pos)
                     (+ (second pos) arg)]
                    face)

          \S (recur dirs
                    [(first pos)
                     (- (second pos) arg)]
                    face)

          \E (recur dirs
                    [(+ (first pos) arg)
                     (second pos)]
                    face)

          \W (recur dirs
                    [(- (first pos) arg)
                     (second pos)]
                    face)

          \L (recur dirs
                    pos
                    (nth (iterate turn-left face) (/ arg 90)))

          \R (recur dirs
                    pos
                    (nth (iterate turn-right face) (/ arg 90)))

          \F (recur (conj dirs {:dir face :arg arg})
                    pos
                    face))))))

(defn turn-waypoint-left
  [waypoint]
  [(* -1 (second waypoint)) (first waypoint)])

(defn turn-waypoint-right
  [waypoint]
  [(second waypoint) (* -1 (first waypoint))])

(defn part2
  [data]
  (loop [dirs data
         pos [0 0]
         waypoint [10 1]]
    (let [dir (:dir (first dirs))
          arg (:arg (first dirs))
          dirs (rest dirs)]
      (println "p:" pos " w:" waypoint)
      (if (nil? dir)
        (reduce + (map #(Math/abs %) pos))
        (case dir
          \N (recur dirs
                    pos
                    [(first waypoint)
                     (+ (second waypoint) arg)])

          \S (recur dirs
                    pos
                    [(first waypoint)
                     (- (second waypoint) arg)])

          \E (recur dirs
                    pos
                    [(+ (first waypoint) arg)
                     (second waypoint)])

          \W (recur dirs
                    pos
                    [(- (first waypoint) arg)
                     (second waypoint)])

          \L (recur dirs
                    pos
                    (nth (iterate turn-waypoint-left waypoint) (/ arg 90)))

          \R (recur dirs
                    pos
                    (nth (iterate turn-waypoint-right waypoint) (/ arg 90)))

          \F (recur dirs
                    [(+ (first pos) (* arg (first waypoint)))
                     (+ (second pos) (* arg (second waypoint)))]
                    waypoint))))))

(defn main
  [& args]
  (let [data (get-data "resources/day12.txt")]
    (str "part 1: " (part1 data) "\n"
         "part 2: " (part2 data))))
