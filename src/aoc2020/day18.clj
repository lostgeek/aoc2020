(ns aoc2020.day18
  "AOC 2020 Day 18"
  (:require [clojure.string :as string])
  (:gen-class))

(defn get-data
  [f]
  (let [data (->> f
                 slurp
                 string/trim
                 (#(string/split % #"\n")))]
   data))

(defn parse-input
  [input]
  (let [symbols (string/split
                  (string/replace (string/replace input
                                                  #"\)" " )")
                                  #"\(" "( ")
                  #" ")]
    (vec (for [s symbols]
           (case s
             "(" s
             ")" s
             "+" s
             "*" s
             (Integer/parseInt s))))))

(defn search-matching-brackets
  [symbols]
  (loop [symbols (rest symbols)
         content []
         level 1]
    (if (zero? level)
      {:content (drop-last 1 content)
       :rest-symbols symbols}
      (case (first symbols)
        "(" (recur (rest symbols) (conj content (first symbols)) (inc level))
        ")" (recur (rest symbols) (conj content (first symbols)) (dec level))
        (recur (rest symbols) (conj content (first symbols)) level)))))

(defn reverse-search-matching-brackets
  [symbols]
  (loop [symbols (rest (reverse symbols))
         content []
         level 1]
    (if (zero? level)
      {:content (reverse (drop-last 1 content))
       :rest-symbols (reverse symbols)}
      (case (first symbols)
        ")" (recur (rest symbols) (conj content (first symbols)) (inc level))
        "(" (recur (rest symbols) (conj content (first symbols)) (dec level))
        (recur (rest symbols) (conj content (first symbols)) level)))))

(defn do-calc
  [operator lhs rhs]
  (case operator
    "+" (+ lhs rhs)
    "*" (* lhs rhs)))

(defn calc
  [input]
  (loop [symbols input
         acc nil
         operator nil]
    ; (println "s:" symbols "\na:" acc "\no:" operator "\n")
    (case (first symbols)
      "(" (let [{:keys [content rest-symbols]} (search-matching-brackets symbols)]
            (if (nil? acc)
              (recur rest-symbols (calc content) operator)
              (recur rest-symbols (do-calc operator acc (calc content)) nil)))
      "+" (recur (rest symbols) acc "+")
      "*" (recur (rest symbols) acc "*")
      nil acc ;return
      (if (nil? acc)
        (recur (rest symbols) (first symbols) operator)
        (recur (rest symbols) (do-calc operator acc (first symbols)) nil)))))

(defn part1
  [data]
  (reduce + (map #(-> % parse-input calc) data)))

(defn find-next-group
  [input]
  (if (= "(" (first input))
    (search-matching-brackets input)
    {:content [(first input)] :rest-symbols (rest input)}))

(defn find-prev-group
  [input]
  (if (= ")" (last input))
    (reverse-search-matching-brackets input)
    {:content [(last input)] :rest-symbols (drop-last input)}))

(defn apply-advanced-math
  [input]
  (loop [symbols input
         result []]
    (if (empty? symbols)
      result
      (if (= "+" (first symbols))
        (let [{:keys [content rest-symbols]} (find-next-group (rest symbols))
              {prev-content :content
               prev-rest-symbols :rest-symbols} (find-prev-group result)]
          ; (println "p-r-s:" prev-rest-symbols
                   ; "\np-c:" prev-content
                   ; "\nc:" content
                   ; "\nr-s:" rest-symbols "\n")
          (recur rest-symbols (concat prev-rest-symbols ["(" "("] (apply-advanced-math prev-content) [")" "+" "("] (apply-advanced-math content) [")" ")"])))
        (recur (rest symbols) (conj (vec result) (first symbols)))))))

(defn part2
  [data]
  (reduce + (map #(-> % parse-input apply-advanced-math calc) data)))

(defn main
  [& args]
  (let [data (get-data "resources/day18-test2.txt")]
    (str "part 1: " (time (part1 data)) "\n"
         "part 2: " (time (part2 data)))))
