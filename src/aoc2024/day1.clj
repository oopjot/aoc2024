(ns aoc2024.day1)

(def input (clojure.string/split-lines (slurp "resources/day1.txt")))

(defn input->locations
  [input]
  (let [[loc-1 loc-2] (reduce (fn [[loc-1 loc-2] line]
                                (let [parts (clojure.string/split line #" ")
                                      left  (Integer/parseInt (first parts))
                                      right (Integer/parseInt (last parts))]
                                  [(cons left loc-1) (cons right loc-2)]))
                              [[] []]
                              input)]
    [(sort loc-1) (sort loc-2)]))

(defn sum-distances
  [input]
  (let [[loc-1 loc-2] (input->locations input)]
    (reduce + 0 (map #(Math/abs (- %1 %2)) loc-1 loc-2))))

(defn similarity-score
  [input]
  (let [[loc-1 loc-2] (input->locations input)]
    (loop [locs       loc-1
           last-d     nil
           last-score nil
           score-sum  0]
      (if (empty? locs)
        score-sum
        (if (= last-d (first locs))
          (recur (rest locs) last-d last-score (+ score-sum last-score))
          (let [curr  (first locs)
                score (* curr (count (filter #(= curr %) loc-2)))]
            (recur (rest locs) curr score (+ score-sum score))))))))

