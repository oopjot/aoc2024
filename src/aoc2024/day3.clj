(ns aoc2024.day3)

(def input (clojure.string/split-lines (slurp "resources/day_3.txt")))

(defn parse-line
  [line]
  (re-seq #"mul\(\d{1,3},\d{1,3}\)" line))

(defn parse-mul
  [mul]
  (apply * (map #(Integer/parseInt %1) (re-seq #"\d{1,3}" mul))))

(def ops-1
  (flatten (map parse-line input)))

(def result-1
  (apply + (map parse-mul ops-1)))

(defn parse-line-do
  [line]
  (re-seq #"mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)" line))

(def ops-2
  (flatten (map parse-line-do input)))

(defn reduce-ops
  [input-ops]
  (loop [ops     []
         to-read input-ops
         enabled true]
    (let [op (first to-read)]
      (cond
        (empty? to-read) ops
        (= op "do()")    (recur ops (rest to-read) true)
        (= op "don't()") (recur ops (rest to-read) false)
        enabled          (recur (conj ops op) (rest to-read) enabled)
        :else            (recur ops (rest to-read) enabled)))))

(def result-2
  (apply + (map parse-mul (reduce-ops ops-2))))
