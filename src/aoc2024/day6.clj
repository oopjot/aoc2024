(ns aoc2024.day6)

(def input (clojure.string/split-lines (slurp "resources/day_6.txt.tmp")))

(defn obstacles
  [input]
  (for [[y row] (map-indexed vector input)
        [x c] (map-indexed vector row)
        :when (= c \#)]
    [x y]))

(defn initial-guard-position
  [input]
  (first (for [[y row] (map-indexed vector input)
               [x c]   (map-indexed vector row)
               :when   (= c \^)]
           {:pos [x y] :dir :up})))

(defn initial-map
  [input]
  {:obs (obstacles input)
   :guard (initial-guard-position input)
   :visited []
   :add-obs []
   :row-max (dec (count input))
   :col-max (dec (count (first input)))})

(defn next-pos
  [guard]
  (let [{dir :dir [x y] :pos} guard]
    (cond
      (= dir :up)    [x (- y 1)]
      (= dir :right) [(+ x 1) y]
      (= dir :left)  [(- x 1) y]
      (= dir :down)  [x (+ y 1)])))

(defn turn-right
  [dir]
  (cond
    (= dir :up) :right
    (= dir :right) :down
    (= dir :down) :left
    (= dir :left) :up))

(defn left-map?
  [x y max-x max-y]
  (or (< x 0)
      (> x max-x)
      (< y 0)
      (> y max-y)))

(defn guard-step
  [map']
  (let [guard   (:guard map')
        [x' y'] (next-pos guard)
        obs     (:obs map')]
    (cond
      (contains? (set obs) [x' y']) (-> map'
                                        (update-in [:guard :dir] turn-right)
          
          )
      :else                         (-> map'
                                         (update :visited conj (:pos guard))
                                         (assoc-in [:guard :pos] [x' y'])))))

(defn patrol
  [input]
  (loop [map' (initial-map input)]
    (let [guard (:guard map')
          [x y] (:pos guard)]
      (cond
        (left-map?
         x
         y
         (:col-max map')
         (:row-max map')) map'
        :else             (recur (update (guard-step map') :visited conj [x y]))))))

(def result-1
  (->> input
       (patrol)
       (:visited)
       (set)
       (count)))

(defn add-obstacle?
  [map' to-add]
  (let [visited (set (:visited map'))
        g (:guard map')
        {[x y] :pos dir :dir} g
        [x' y'] (next-pos g)]
    (and
     (contains? visited [x y])
     (not (left-map? x' y' (:col-max map') (:row-max map')))
     (not (contains? (set to-add) [x' y'])))))


(defn add-obstacles
  [input to-add]
  (loop [map'   (initial-map input)
         to-add to-add]
    (let [guard (:guard map')
          [x y] (:pos guard)]
      (cond
        (left-map?
         x
         y
         (:col-max map')
         (:row-max map'))    to-add
        (add-obstacle? map' to-add) (conj to-add (next-pos guard))
        :else                (recur (guard-step map') to-add)))))


