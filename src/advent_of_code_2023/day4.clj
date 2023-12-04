(ns advent-of-code-2023.day4
  (:require [clojure.string :as str]
            [clojure.math :as math]
            [clojure.set :as s]))

(defn part1 [input-path]
  (->> (slurp input-path)
       (str/split-lines)
       (map #(->> %
                  (drop-while (fn [c] (not= \: c)))
                  (drop 2)
                  (apply str)
                  ((fn [s] (str/split s #" \| ")))
                  (map (fn [s] (set (remove empty? (str/split s #"\s+")))))
                  (apply s/intersection)
                  ((fn [s] (case (count s)
                             0 0
                             (math/pow 2 (- (count s) 1)))))))
       (apply +)))

(defn part2 [input-path]
  (let [input (->> (slurp input-path)
                   (str/split-lines))]
    (->> input
         (map #(->> %
                    ((juxt (comp bigint (partial re-find #"\d+"))
                           (fn [s]
                             (->> s
                                  (drop-while (fn [c] (not= \: c)))
                                  (drop 2)
                                  (apply str)
                                  ((fn [s] (str/split s #" \| ")))
                                  (map (fn [s] (set (remove empty? (str/split s #"\s+")))))
                                  (apply clojure.set/intersection)
                                  count))))))
         (reduce (fn [res [card-number number-count]]
                   (merge-with + res
                               (into {}
                                     (map #(vector (+ card-number %) (* 1 (get res card-number))) (range 1 (+ 1 number-count))))))
                 (into {} (map #(vector % 1) (range 1 (+ 1 (count input))))))
         (map second)
         (apply +))))

(comment
  (part1 "src/advent_of_code_2023/day4.txt")
  (part2 "src/advent_of_code_2023/day4.txt"))
