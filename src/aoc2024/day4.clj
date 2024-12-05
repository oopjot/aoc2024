(ns aoc2024.day4)

(def input (clojure.string/split-lines (slurp "resources/day_4.txt")))
(defn grid-get
  [grid x y]
  (nth (nth grid y) x))

(defn in-bounds
  [left top right bottom]
  #(and
    (>= %1 left) (<= %1 right)
    (>= %2 top) (<= %2 bottom)))

(defn spec-table
  [grid]
  (let [rows (- (count grid) 1)
        cols (- (count (first grid)) 1)]
    {:left       {:bounds [3 0 cols rows]
                  :step   [-1 0]}
     :up         {:bounds [0 3 cols rows]
                  :step   [0 -1]}
     :right      {:bounds [0 0 (- cols 3) rows]
                  :step   [1 0]}
     :down       {:bounds [0 0 cols (- rows 3)]
                  :step   [0 1]}
     :left-up    {:bounds [3 3 cols rows]
                  :step   [-1 -1]}
     :right-up   {:bounds [0 3 (- cols 3) rows]
                  :step   [1 -1]}
     :left-down  {:bounds [3 0 cols (- rows 3)]
                  :step   [-1 1]}
     :right-down {:bounds [0 0 (- cols 3) (- rows 3)]
                  :step   [1 1]}}))

(defn xmas?
  ([grid x y]
   (vector (xmas? grid x y :left)
           (xmas? grid x y :up)
           (xmas? grid x y :right)
           (xmas? grid x y :down)
           (xmas? grid x y :left-up)
           (xmas? grid x y :right-up)
           (xmas? grid x y :left-down)
           (xmas? grid x y :right-down)))
  ([grid x y direction]
   (let [spec   (get (spec-table grid) direction)
         step-x (first (:step spec))
         step-y (second (:step spec))
         bounds (:bounds spec)]
     (and ((apply in-bounds bounds) x y)
          (reduce (fn [result [i c]]
                    (let [x' (+ x (* i step-x))
                          y' (+ y (* i step-y))]
                      (and
                       (= c (grid-get grid x' y'))
                       result)))
                  true [[0 \X] [1 \M] [2 \A] [3 \S]])))))

(defn count-xmas
  [grid]
  (count (filter identity (flatten (for [[y row] (map-indexed vector grid)
                                         [x c] (map-indexed vector row)
                                         :when (= c \X)]
                                     (xmas? grid x y))))))

(defn x-mas?
  [grid x y]
  (let [max-row (- (count grid) 2)
        max-col (- (count (first grid)) 2)]
    (if (or (< x 1) (< y 1) (> x max-row) (> y max-col))
      false
      (let [lu (grid-get grid (dec x) (dec y))
            ru (grid-get grid (inc x) (dec y))
            ld (grid-get grid (dec x) (inc y))
            rd (grid-get grid (inc x) (inc y))]
        (and
         (or (and (= lu \M) (= rd \S)) (and (= lu \S) (= rd \M)))
         (or (and (= ru \M) (= ld \S)) (and (= ru \S) (= ld \M))))))))

(defn count-x-mas
  [grid]
  (count (filter identity (for [[y row] (map-indexed vector grid)
                                [x c] (map-indexed vector row)
                                :when (= c \A)]
                            (x-mas? grid x y)))))
