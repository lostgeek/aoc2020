(ns aoc2020.day14
  "AOC 2020 Day 14"
  (:require [clojure.string :as string])
  (:gen-class))

(defn parse
  [line]
  (let [parts (string/split line #" = ")]
    (if (= "mask" (first parts))
      {:cmd :mask
       :value (second parts)}
      (let [pos (read-string (subs (first parts) 4 (dec (count (first parts)))))
            value (read-string (second parts))]
        {:cmd :mem
         :pos pos
         :value value}))))

(defn get-data
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n")))]
    (map parse data)))

(def mask (atom ""))

(defn exec-1
  [memory {:keys [cmd pos value] :as line}]
  (case cmd
    :mask
    (do (reset! mask value)
        memory)

    :mem
    (let [set-pattern (Long/parseLong (string/replace @mask #"X" "0") 2)
          unset-pattern (Long/parseLong (string/replace @mask #"X" "1") 2)
          new-val (bit-or set-pattern
                          (bit-and unset-pattern
                                   value))]
      (assoc memory pos new-val))))

(defn part1
  [data]
  (loop [code data
         memory {}]
    (let [line (first code)
          code (rest code)]
      (if (nil? line)
        (reduce + (vals memory))
        (recur code (exec-1 memory line))))))

(defn calc-address
  [p X-inds fill-ins]
  (loop [pos p
         i 0]
      (if (= i (count X-inds))
        pos
        (let [ind (- 35 (nth X-inds i))
              fill (nth fill-ins i)]
          (if (= \0 fill)
            (recur (bit-clear pos ind) (inc i))
            (recur (bit-set pos ind) (inc i)))))))

(defn addresses
  [pos]
  (let [pos (bit-or pos (Long/parseLong (string/replace @mask #"X" "0") 2))
        X-count (count (filter #(= \X %) @mask))
        X-inds (map first
                    (filter #(= \X (second %))
                            (map-indexed vector @mask)))]
    (for [i (range (Math/pow 2 X-count))]
      (let [fill-ins (concat (take (- X-count (count (Long/toBinaryString i)))
                                   (repeat \0))
                             (Long/toBinaryString i))]
        (calc-address pos X-inds fill-ins)))))

(defn exec-2
  [memory {:keys [cmd pos value] :as line}]
  (case cmd
    :mask
    (do (reset! mask value)
        memory)

    :mem
    (do (apply assoc memory
               (interleave (addresses pos)
                           (repeat value))))))

(defn part2
  [data]
  (loop [code data
         memory {}]
    (let [line (first code)
          code (rest code)]
      (if (nil? line)
        (reduce + (vals memory))
        (recur code (exec-2 memory line))))))

(defn main
  [& args]
  (let [data (get-data "resources/day14.txt")]
    (str "part 1: " (time (part1 data)) "\n"
         "part 2: " (time (part2 data)))))
