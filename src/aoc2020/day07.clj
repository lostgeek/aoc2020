(ns aoc2020.day07
  "AOC 2020 Day 7"
  (:require [clojure.string :as string])
  (:gen-class))

(defn make-color-keyword
  [text]
  (keyword (string/replace text #" " "-")))

(defn parse-content
  [text]
  (for [match (re-seq #"(\d+) ([a-z ]+ [a-z ]+) bag" text)]
    {:number (read-string (nth match 1))
     :color (make-color-keyword (nth match 2))}))

(defn get-data
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n")))]
    (apply merge (for [line data]
                   (let [match (re-matches #"(.+) bags contain (.+)." line)
                         color (nth match 1)
                         content (nth match 2)]
                     {(make-color-keyword color) (parse-content content)})))))

(defn multiply-content
  [content n]
  (for [{:keys [number color]} content]
    {:number (* number n) :color color}))

(defn resolve-bags
  [rules do-not-resolve maxsteps]
  (if (= 0 maxsteps)
    rules
    (let [new-rules (apply merge 
                           (for [[outer-color content] (seq rules)]
                             {outer-color (apply concat 
                                                 (for [{:keys [number color] :as bag} content]
                                                   (if (or (empty? (get rules color))
                                                           (= do-not-resolve color))
                                                     [bag]
                                                     (conj (multiply-content (get rules color) number)
                                                           {:number number :color (keyword (str "unpacked-" (name color)))}))))}))
          maxsteps (dec maxsteps)]
      (if (= new-rules rules)
        rules
        (resolve-bags new-rules do-not-resolve maxsteps)))))

(defn search-color
  [[color content] target]
  (not-empty (filter #(= target (:color %)) content)))

(defn part1
  [data]
  (count (filter #(search-color % :shiny-gold) (resolve-bags data :shiny-gold 100))))

(defn part2
  [data]
  (apply + (seq (map :number (:shiny-gold (resolve-bags data :none 100))))))

(defn main
  [& args]
  (let [data (get-data "resources/day07.txt")]
    (str "part 1: " (part1 data) "\n"
         "part 2: " (part2 data))
  ))
