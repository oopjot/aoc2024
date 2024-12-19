(ns aoc2024.day11)

(def input (clojure.string/trim-newline (slurp "resources/day_11.txt")))

(defn stones
  [input]
  (map #(Integer/parseInt %)
       (clojure.string/split input #" ")))

(defn split-stone
  [stone]
  (let [s    (str stone)
        half (/ (count s) 2)]
    (mapv #(Integer/parseInt (apply str %))
         [(take half s) (drop half s)])))

(defn transform
  [stone]
  (cond
    (= stone 0)                 [1]
    (even? (count (str stone))) (split-stone stone)
    :else                       [(* 2024 stone)]))

(defn blink
  [stones]
  (mapcat transform stones))

(defn n-times
  [n f]
  (apply comp (repeat n f)))

(defn result
  [n]
  (reduce + 0
          (pmap #(count ((n-times n blink) (vector %)))
               (stones input))))

(defn count-stone
  ([n]
   #(count-stone n %))
  ([n stone]
   (cond
     (= n 0)                     1
     (even? (count (str stone))) (+ (count-stone (- n 1) (first (split-stone stone)))
                                    (count-stone (- n 1) (second (split-stone stone))))
     :else                       (count-stone (- n 1) (* 2024 stone)))))

(defn result-2
  [n]
  (reduce + 0
          (pmap (count-stone n) (stones input))))
