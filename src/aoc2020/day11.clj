(ns aoc2020.day11
  "AOC 2020 Day 11"
  (:require [clojure.string :as string])
  (:gen-class))

(defn get-data
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n")))]
    (vec (map #(vec (char-array %)) data))))

(defn disp
  [seats]
  (println (string/join "\n" (map #(apply str %) seats))))

(defn get-neighbors
  [seats x y]
  (remove nil? [(get-in seats [(dec y) (dec x)] nil)
                (get-in seats [(dec y)      x ] nil)
                (get-in seats [(dec y) (inc x)] nil)
                (get-in seats [     y  (dec x)] nil)
                (get-in seats [     y  (inc x)] nil)
                (get-in seats [(inc y) (dec x)] nil)
                (get-in seats [(inc y)      x ] nil)
                (get-in seats [(inc y) (inc x)] nil)]))

(defn dir-seats
  [seats x y dirx diry]
  (for [i (range 1 (* 2 (count seats)))]
    (get-in seats [(+ y (* i diry)) (+ x (* i dirx))])))


(defn get-sight-seat
  [seats x y dirx diry]
  (first (filter #(not= \. %) (dir-seats seats x y dirx diry))))


(defn get-sight-neighbors
  [seats x y]
  [(get-sight-seat seats x y  1  1)
   (get-sight-seat seats x y  0  1)
   (get-sight-seat seats x y -1  1)
   (get-sight-seat seats x y  1  0)
   (get-sight-seat seats x y -1  0)
   (get-sight-seat seats x y  1 -1)
   (get-sight-seat seats x y  0 -1)
   (get-sight-seat seats x y -1 -1)])

(defn next-step-part1
  [seats]
  (vec (for [y (range (count seats))]
         (vec (for [x (range (count (first seats)))]
                (let [seat (get-in seats [y x])
                      neigh (count (filter #(= \# %)
                                           (get-neighbors seats x y)))]
                  (cond
                    (and (= \L seat)
                         (zero? neigh))
                    \#

                    (and (= \# seat)
                         (<= 4 neigh))
                    \L

                    :else
                    seat)))))))

(defn next-step-part2
  [seats]
  (vec (for [y (range (count seats))]
         (vec (for [x (range (count (first seats)))]
                (let [seat (get-in seats [y x])
                      neigh (count (filter #(= \# %)
                                           (get-sight-neighbors seats x y)))]
                  (cond
                    (and (= \L seat)
                         (zero? neigh))
                    \#

                    (and (= \# seat)
                         (<= 5 neigh))
                    \L

                    :else
                    seat)))))))

(defn count-occupied
  [seats]
  (count (filter #(= \# %) (flatten seats))))

(defn part1
  [data]
  (loop [seats data
         prev-occupied 0]
    ; (disp seats)
    ; (println "")
    (let [seats (next-step-part1 seats)
          occupied (count-occupied seats)]
      (if (= prev-occupied occupied)
        occupied
        (recur seats occupied)))))

(defn part2
  [data]
  (loop [seats data
         prev-occupied 0]
    (disp seats)
    (println "")
    (let [seats (next-step-part2 seats)
          occupied (count-occupied seats)]
      (if (= prev-occupied occupied)
        occupied
        (recur seats occupied)))))

(defn main
  [& args]
  (let [data (get-data "resources/day11.txt")]
    (str "part 1: " (part1 data) "\n"
         "part 2: " (part2 data))))
