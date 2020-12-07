(ns aoc2020.core
  (:require [aoc2020.day01 :as day01])
  (:require [aoc2020.day02 :as day02])
  (:require [aoc2020.day03 :as day03])
  (:require [aoc2020.day04 :as day04])
  (:require [aoc2020.day05 :as day05])
  (:require [aoc2020.day06 :as day06])
  (:require [aoc2020.day07 :as day07])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println
    (case (Integer/parseInt (first args))
      1 (day01/main)
      2 (day02/main)
      3 (day03/main)
      4 (day04/main)
      5 (day05/main)
      6 (day06/main)
      7 (day07/main)
      "Day not found.")))
