(ns aoc2024.day5)

(def input (clojure.string/split-lines (slurp "resources/day_5.txt")))

(defn rules
  [input]
  (reduce (fn [acc line]
            (let [[a b] (map #(Integer/parseInt %) (clojure.string/split line #"\|"))]
              (if (contains? acc b)
                (update acc b conj a)
                (assoc acc b #{a}))))
          {}
          (take-while #(not (= "" %)) input)))

(defn drop-until
  [pred input]
  (loop [dropping true
         input input
         result []]
    (cond
      (empty? input) result
      (and dropping (pred (first input))) (recur false (rest input) result)
      dropping (recur dropping (rest input) result)
      :else (recur dropping (rest input) (conj result (first input))))))

(defn middle-value
  [vect]
  (when-not (empty? vect)
    (vect (quot (count vect) 2))))

(defn updates
  [input]
  (->> input
      (drop-until (partial = ""))
      (map #(clojure.string/split % #","))
      (map (partial map #(Integer/parseInt %)))))

(defn valid?
  [rules update]
  (loop [pages update]
    (let [current (first pages)
          rest'   (rest pages)
          rule    (get rules current)
          common  (clojure.set/intersection (set rest') rule)]
      (cond
        (empty? pages)        true
        (not (empty? common)) false
        :else                 (recur (rest pages))))))

(defn correct-updates
  [input]
  (let [rules (rules input)
        updates (updates input)]
   (for [update updates
         :when (valid? rules update)]
     update)))

(defn count-middle
  [updates]
  (->> updates
       (map #(apply vector %))
       (map middle-value)
       (reduce + 0)))

(def result-1
  (count-middle (correct-updates input)))

(defn incorrect-updates
  [input]
  (let [rules (rules input)
        updates (updates input)]
    (for [update updates
          :when (not (valid? rules update))]
      (apply vector update))))

(defn update-comparator
  [rules]
  #(let [rule (get rules %2)]
    (contains? rule %1)))

(defn order-update
  [rules update]
  (sort (update-comparator rules) update))

(defn ordered-updates
  [input]
  (let [rules (rules input)
        updates (incorrect-updates input)]
    (map #(sort (update-comparator rules) %) updates)))

(def result-2
  (count-middle (ordered-updates input)))
