(ns aoc2024.day7)
(require '[clojure.math.combinatorics :as comb])

(def input (clojure.string/split-lines (slurp "resources/day_7.txt")))

(defn parse-equation
  [line]
  (let [[r ns] (clojure.string/split line #":")]
    {:result (Long/parseLong r)
     :numbers (map #(Long/parseLong %)
                   (rest (clojure.string/split ns #" ")))}))

(defn calibration
  [numbers ops]
  (reduce #(%2 %1) (first numbers)
          (map partial ops (rest numbers))))

(defn valid-calibration?
  [op-types {result :result numbers :numbers}]
  (loop [ops-list (comb/selections op-types (dec (count numbers)))]
    (cond
      (empty? ops-list) false
      (= (calibration numbers (first ops-list))
         result)        true
      :else             (recur (rest ops-list)))))

(def result-1
  (->> input
       (map parse-equation)
       (filter (partial valid-calibration? [+ *]))
       (map :result)
       (reduce + 0)))

(defn ||
  [a b]
  (Long/parseLong (apply str (concat (str b) (str a)))))

(def result-2
  (->> input
       (map parse-equation)
       (filter (partial valid-calibration? [|| + *]))
       (map :result)
       (reduce + 0)))
