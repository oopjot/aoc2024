(ns aoc2024.day9)

(def input (clojure.string/trim-newline (slurp "resources/day_9.txt.tmp")))

(defn file-blocks
  [input]
  (->> input
       (map #(Integer/parseInt (str %)))
       (take-nth 2)
       (map-indexed vector)))

(defn space-blocks
  [input]
  (take-nth 2
            (rest (map #(Integer/parseInt (str %)) input))))

(defn compact
  [fbs' sbs]
  (loop [fbs    (reverse (rest fbs'))
         sbs    sbs
         result [(first fbs')]]
    (cond
      (empty? (rest fbs)) (conj result (first fbs))
      (empty? sbs) result
      :else        (let [sb          (first sbs)
                         [tid tsize] (first fbs)]
                     (cond
                       (> sb tsize) (recur
                                     (rest fbs)
                                     (cons (- sb tsize) (rest sbs))
                                     (conj result [tid tsize]))
                       (= sb tsize) (recur
                                     (drop-last (rest fbs))
                                     (rest sbs)
                                     (conj result (first fbs) (last fbs)))
                       (< sb tsize) (recur
                                     (drop-last (conj (rest fbs) [tid (- tsize sb)]))
                                     (rest sbs)
                                     (conj result [tid sb] (last fbs))))))))

(defn expand
  [blocks]
  (flatten (map #(repeat (second %) (first %)) blocks)))


(defn checksum
  [blocks]
  (reduce + 0 (map (partial apply *)
                   (map-indexed vector blocks))))



