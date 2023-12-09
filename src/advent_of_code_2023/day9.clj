(ns advent-of-code-2023.day9
  (:require [clojure.string :as str]))

(defn differences [previous-values sequence]
  (let [diff (map (fn [[eka toka]] (- eka toka)) sequence)]
    (if (every? zero? diff)
      (apply + previous-values)
      (recur (conj previous-values (first diff)) (partition 2 1 diff)))))

(defn differences2 [previous-values sequence]
  (let [diff (map (fn [[eka toka]] (- eka toka)) sequence)]
    (if (every? zero? diff)
      (->> previous-values
           reverse
           (reduce #(- %2 %1)))
      (recur (conj previous-values (last diff)) (partition 2 1 diff)))))

(defn solver [diff-fn input-path]
  (->> (slurp input-path)
       (str/split-lines)
       (map #(->> (str/split % #" ")
                  (map bigint)
                  reverse
                  diff-fn))
       (apply +)))

(def part1 (partial solver #(->> %
                                 ((juxt (comp vector first) (partial partition 2 1)))
                                 (apply differences))))
(def part2 (partial solver #(->> %
                                 ((juxt (comp vector last) (partial partition 2 1)))
                                 (apply differences2))))

(comment
  (part1 "src/advent_of_code_2023/day9.txt")
  (part2 "src/advent_of_code_2023/day9.txt"))
