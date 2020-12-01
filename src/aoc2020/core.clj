(ns aoc2020.core
  (:require [aoc2020.day01 :as day01])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println
    (case (Integer/parseInt (first args))
      1 (day01/main)
      "Day not found.")))
