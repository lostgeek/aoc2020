(ns aoc2020.core
  (:require [aoc2020.day01 :as day01])
  (:require [aoc2020.day02 :as day02])
  (:require [aoc2020.day03 :as day03])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println
    (case (Integer/parseInt (first args))
      1 (day01/main)
      2 (day02/main)
      3 (day03/main)
      "Day not found.")))
