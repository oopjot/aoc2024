(ns aoc2024.day10)

(def input (clojure.string/split-lines (slurp "resources/day_10.txt")))

(defn build-map
  [input]
  (into [] (map (fn [row] (into [] (map #(Integer/parseInt (str %)) row))) input)))

(defn trailheads
  [map']
  (for [[y row] (map-indexed vector map')
        [x h] (map-indexed vector row)
        :when (= h 0)]
    [x y]))

(defn tops
  [map']
  (into #{} (for [[y row] (map-indexed vector map')
                 [x h] (map-indexed vector row)
                 :when (= h 9)]
             [x y])))

(defn height
  [map' [x y]]
  (try ((map' y) x)
       (catch IndexOutOfBoundsException _ (do (println [x y]) nil))))

(defn valid-neighbor?
  [map' [x y]]
  (fn [pos]
    (if (nil? pos) false
        (let [[x' y'] pos
              ymax    (dec (count map'))
              xmax    (dec (count (first map')))
              h       (height map' [x y])]
          (and (>= x' 0) (<= x' xmax) (>= y' 0) (<= y' ymax)
               (not (nil? h))
               (= 1 (- (height map' [x' y']) h)))))))



(defn around
  [pos]
  (map #(into [] (map + pos %)) [[-1 0] [1 0] [0 -1] [0 1]]))

(defn neighbors
  [map' pos]
  (filter (valid-neighbor? map' pos)
          (around pos)))

(defn dfs
  ([map' head] (dfs map' head []))
  ([map' head visited]
   (let [ns    (neighbors map' head)
         moves (filter #(not (contains? visited %)) ns)]
     (if (= 9 (height map' head))
       [head]
       (apply conj visited head (mapcat #(dfs map' % visited) moves))))))

(def result-1
  (let [map'  (build-map input)
        heads (trailheads map')
        tops  (tops map')]
    (reduce + 0
            (for [head heads
                  :let [path (dfs map' head)
                        score (count (filter #(contains? tops %) path))]]
              score))))

(defn rating
  [top path]
  (count (filter #(= top %) path)))

(def result-2
  (let [map' (build-map input)]
    (reduce + 0
            (for [head (trailheads map')
                  top  (tops map')
                  :let [path (dfs map' head)]]
              (rating top path)))))
