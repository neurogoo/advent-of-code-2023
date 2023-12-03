(ns advent-of-code-2023.day3
  (:require [clojure.string :as str]))

(defn- hotzones [symbol-pattern input]
  (for [i (range 0 (count input))
        j (range 0 (count (first input)))
        :when (re-find symbol-pattern (str (get-in input [i j])))]
    (for [x (range -1 2)
          y (range -1 2)
          :let [new-i (+ i y)
                new-j (+ j x)]
          :when (and (>= new-i 0) (<= new-i (count input))
                     (>= new-j 0) (<= new-j (count (first input)))
                     (not= [y x] [new-i new-j])
                     (re-find #"[0-9]" (str (get-in input [new-i new-j]))))]
      [new-i new-j])))

(defn- numbers [input]
  (for [i (range 0 (count (first input)))]
    (->> (get input i)
         (map-indexed
          (fn [idx c]
            (when (re-find #"[0-9]" (str c))
              [[i idx] c])))
         (partition-by nil?)
         (filter (partial every? identity))
         (map (fn [numvec]
                (->> numvec
                     (reduce (fn [[indices numbers] [index number]]
                               [(conj indices index) (conj numbers number)]) [[] []])
                     ((fn [[indices numbers]] [indices (bigint (apply str numbers))]))))))))

(defn part1 [input-path]
  (let [input (->> (slurp input-path)
                   (str/split-lines))
        hotzones (->> input
                      (hotzones #"[^0-9\.]")
                      (apply concat)
                      (into #{}))]
    (->> (numbers input)
         (apply concat)
         (filter (fn [[indices _]] (some hotzones indices)))
         (map second)
         (apply +))))

(defn part2 [input-path]
  (let [input (->> (slurp input-path)
                   (str/split-lines))
        hotzones (->> input
                      (hotzones #"[\*]")
                      (remove empty?)
                      (map set))]
    (->> (for [gear hotzones]
           (filter (fn [[zones _]]
                     (some gear zones)) (apply concat (numbers input))))
         (filter #(= 2 (count %)))
         (map #(->> %
                    (map second)
                    (apply *)))
         (apply +))))
(comment
  (part1 "src/advent_of_code_2023/day3.txt")
  (part2 "src/advent_of_code_2023/day3.txt"))
