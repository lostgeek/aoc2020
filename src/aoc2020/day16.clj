(ns aoc2020.day16
  "AOC 2020 Day 16"
  (:require [clojure.string :as string])
  (:require [clojure.set :as sets])
  (:gen-class))

(defn parse-rules
  [rules]
  (apply merge
         (for [line rules]
           (let [match (re-matches #"(.*): (\d+)-(\d+) or (\d+)-(\d+)" line)
                 field (nth match 1)
                 start-1 (Integer/parseInt (nth match 2))
                 end-1 (Integer/parseInt (nth match 3))
                 start-2 (Integer/parseInt (nth match 4))
                 end-2 (Integer/parseInt (nth match 5))]
             {(keyword field)
              (set (concat (range start-1 (inc end-1))
                           (range start-2 (inc end-2))))}))))

(defn parse-tickets
  [tickets]
  (for [line (drop 2 tickets)]
    (map #(Integer/parseInt %) (string/split line #","))))

(defn get-data
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n")))
        empty-lines (concat [0] (keep-indexed #(if (= "" %2) %1) data) [(count data)])
        slices (partition 2 1 empty-lines)
        rules (apply subvec data (first slices))
        our (apply subvec data (second slices))
        nearby (apply subvec data (last slices))]
    {:rules (parse-rules rules)
     :our (first (parse-tickets our))
     :nearby (parse-tickets nearby)}))

(defn invalid-sum
  [rules ticket]
  (reduce +
          (remove nil?
                  (for [n ticket]
                    (when-not (some identity (map #(contains? % n) (vals rules)))
                      n)))))

(defn part1
  [data]
  (reduce +
          (map (partial invalid-sum (:rules data))
               (:nearby data))))

(defn check-validity
  [rules ticket]
  "Checks that every number on ticket is in the range of some rule"
  (every? identity
          (for [n ticket]
            (some true?
                    (map #(contains? % n) (vals rules))))))

(defn remove-invalid
  [rules tickets]
  (filter #(check-validity rules %) tickets))

(defn try-field-at-pos
  [rule tickets pos]
  (every? true? (map (partial contains? rule) (map #(nth % pos) tickets))))

(defn calc-possible-pos
  [rule tickets matched]
  (let [possible-pos (sets/difference (set (range (count (first tickets))))
                                      (set (vals matched)))]
    (remove nil? (map (fn [pos] (when (try-field-at-pos rule tickets pos) pos))
                      possible-pos))))

(defn find-order
  [{:keys [rules our nearby] :as data}]
  (let [tickets (remove-invalid rules nearby)]
    (loop [unmatched (keys rules)
           matched {}
           tried []]
      ; (println "u:" unmatched "\nm:" matched "\nt:" tried "\n")
      (if (empty? unmatched)
        (if (empty? tried)
          matched
          (recur (conj (rest tried) (first tried))
                 matched
                 []))

        (let [field (first unmatched)
              possible-pos (calc-possible-pos (get rules field) tickets matched)]
          (if (= 1 (count possible-pos))
            ; found match
            (recur (concat tried (rest unmatched))
                   (assoc matched field (first possible-pos))
                   [])
            ; try next unmatched
            (recur (rest unmatched)
                   matched
                   (conj tried field))))))))

(defn part2
  [{:keys [rules our nearby] :as data}]
  (let [order (find-order data)]
    (reduce *
            (for [field (remove #(not (string/starts-with? (name %) "departure"))
                                (keys rules))]
              (nth our (get order field))))))

(defn main
  [& args]
  (let [data (get-data "resources/day16.txt")]
    (str "part 1: " (time (part1 data)) "\n"
         "part 2: " (time (part2 data)))))
