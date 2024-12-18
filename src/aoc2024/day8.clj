(ns aoc2024.day8)
(require '[clojure.math.combinatorics :as comb])

(def input (clojure.string/split-lines (slurp "resources/day_8.txt")))

(defn build-map
  [input]
  (->> (for [[y row] (map-indexed vector input)
             [x c]   (map-indexed vector row)
             :when (not (= \. c))]
         [c [x y]])
       (reduce #(update %1 (first %2) conj (second %2))
               {:row-max (dec (count input))
                :col-max (dec (count (first input)))})))

(defn antennas
  [map']
  (dissoc map' :row-max :col-max))

(defn antinodes
  [map' a b]
  (let [d (map - a b)
        d' (map (comp #(* -1 %) -) a b)]
    [(map + a d)
     (map + b d')]))

(defn on-map?
  [map' [x y]]
  (and
   (>= x 0) (<= x (:col-max map'))
   (>= y 0) (<= y (:row-max map'))))

(defn antinodes-from-freq
  [map' [f locs]]
  (let [pairs (comb/combinations locs 2)]
    (for [[a b] pairs
          an (antinodes map' a b)
          :when (on-map? map' an)]
      an)))

(defn all-antinodes
  [input]
  (let [map' (build-map input)
        antennas (antennas map')]
    (for [freq antennas
          an (antinodes-from-freq map' freq)]
      an)))

(def result-1
  (count (set (all-antinodes input))))


(defn antinodes-left
  [map' a b]
  (let [d (map - a b)]
    (loop [m      1
           result [a b]]
      (let [an (map + a (map #(* m %) d))]
        (if (on-map? map' an)
          (recur (+ m 1) (cons an result))
          result)))))

(defn antinodes-right
  [map' a b]
  (let [d (map (comp #(* -1 %) -) a b)]
    (loop [m 1
           result [a b]]
      (let [an (map + a (map #(* m %) d))]
        (if (on-map? map' an)
          (recur (+ m 1) (cons an result))
          result)))))


(defn all-antinodes-mul
  [input]
  (let [map' (build-map input)
        antennas (antennas map')]
    (for [[_ locs] antennas
          [a b] (comb/combinations locs 2)
          :let [an-left (antinodes-left map' a b)
                an-right (antinodes-right map' a b)
                ans (concat an-left an-right)]
          an ans]
      an)))

(def result-2
  (count (set (all-antinodes-mul input))))


(defn vis
  [input]
  (let [map' (build-map input)
        res-2 (set (all-antinodes-mul input))]
    (for [y (range (inc (:row-max map')))
          x (range (inc (:col-max map')))]
      (let [c (if (contains? res-2 [x y]) \# \.)]
        (if (= (:col-max map') x)
          (str c "\n")
          (str c))))))

