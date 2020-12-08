(ns aoc2020.day08
  "AOC 2020 Day 8"
  (:require [clojure.string :as string])
  (:gen-class))

(defn parse
  [data]
  (into [] (for [line data]
             (let [sp (string/split line #" ")
                   command (keyword (first sp))
                   arg0 (read-string (second sp))]
               [command arg0]))))

(defn get-data
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n")))]
      (parse data)))

(defn find-loop
  [code]
  (let [ip (atom 0)
        acc (atom 0)
        visited (atom (vec (repeat (count code) false)))]
    (while (not (nth @visited @ip true))
      (swap! visited update @ip not)
      (let [[command arg0] (nth code @ip :term)]
        (case command
          :acc (do (reset! acc (+ @acc arg0))
                   (swap! ip inc))
          :jmp (reset! ip (+ @ip arg0))
          :nop (swap! ip inc)
          {:ip @ip :acc @acc :reason :error})))
    (if (>= @ip (count code))
      {:ip @ip :acc @acc :reason :terminated}
      {:ip @ip :acc @acc :reason :loop-found})))

(defn part1
  [code]
  (find-loop code))

(defn part2
  [code]
  (first (filter identity
                 (for [i (range (count code))]
                   (case (first (nth code i))
                     :nop (let [out (find-loop (assoc-in code [i 0] :jmp))]
                            (when (= :terminated (:reason out))
                              out))
                     :jmp (let [out (find-loop (assoc-in code [i 0] :nop))]
                            (when (= :terminated (:reason out))
                              out))
                     :acc nil
                     nil)))))

(defn main
  [& args]
  (let [data (get-data "resources/day08.txt")]
    (str "part 1: " (part1 data) "\n"
         "part 2: " (part2 data))))
