(ns aoc2024.day2)

(def input (clojure.string/split-lines (slurp "resources/day_2.txt")))

(defn parse-line
  [input]
  (map #(Integer/parseInt %) (clojure.string/split input #" ")))

(defn safe?
  [report]
  (let [pred-1 (if (> (first report) (second report)) > <)
        pred-2 #(let [diff (Math/abs (- %1 %2))] (and (> diff 0) (< diff 4)))]
    (loop [levels report
           last-level nil]
      (cond
        (empty? levels) true
        (= last-level nil) (recur (rest levels) (first levels))
        (and
         (pred-1 last-level (first levels))
         (pred-2 last-level (first levels))) (recur (rest levels) (first levels))
        :else false))))


(def result-1
  (count (filter identity (map (comp safe? parse-line) input))))

(defn safe-2?
  [report]
  (boolean 
   (or (safe? report)
               (some safe? (map #(concat (take %1 report) (drop (+ 1 %1) report))
                                (range (count report)))))))

(def result-2
  (count (filter identity (map (comp safe-2? parse-line) input))))
