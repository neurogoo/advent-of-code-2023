(ns advent-of-code-2023.day11
  (:require [clojure.string :as str]))

(defn solver [expansion-factor]
  (let [expansion-factor (- expansion-factor 1)
        expand-fn (fn [[res-idx res-v] [idx v]]
                    (if (every? #(= \. %) v)
                      [(conj res-idx idx) (conj res-v v)]
                      [res-idx (conj res-v v)]))
        [y-expand x-expand starmap] (->> (slurp "src/advent_of_code_2023/day11.txt")
                     (str/split-lines)
                     (map-indexed (fn [idx x] [(- idx 1) x]))
                     (reduce expand-fn [[] []])
                     ((fn [[x-idxs x]]
                        (concat [x-idxs]
                                (reduce expand-fn [[] []] (map-indexed (fn [idx x] [(- idx 1) x]) (apply mapv vector x)))))))
        starmap (apply mapv vector starmap)
        galaxies (for [x (range 0 (count (first starmap)))
                       y (range 0 (count starmap))
                       :let [smaller-than-x (count (take-while #(< % x) x-expand))
                             smaller-than-y (count (take-while #(< % y) y-expand))]
                       :when (= \# (get-in starmap [y x]))]
                   [(+ (* expansion-factor smaller-than-y) y)
                    (+ (* expansion-factor smaller-than-x) x)])]
    (apply + (map second (set (for [galaxy-a galaxies
                                    galaxy-b galaxies
                                   :when (not= galaxy-a galaxy-b)]
                               [(sort (list galaxy-a galaxy-b)) (+ (Math/abs (- (first galaxy-a) (first galaxy-b)))
                                                                   (Math/abs (- (second galaxy-a) (second galaxy-b))))]))))))

(def part1 (partial solver 2))
(def part2 (partial solver 1000000))
