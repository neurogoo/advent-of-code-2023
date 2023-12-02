(ns advent-of-code-2023.core
  (:require [clojure.string :as str]))

(comment
  ;;Day 1 part 1
  (->> (slurp "src/advent_of_code_2023/day1.txt")
       (str/split-lines)
       (map (fn [x]
              (->> x
                   (map #(re-matches #"[1-9]" (str %)))
                   (remove nil?)
                   ((juxt first last))
                   str/join
                   bigint)))
       (apply +))
  ;;Day 1 part 2
  (->> (slurp "src/advent_of_code_2023/day1.txt")
       (str/split-lines)
       (mapv (fn [x]
               (let [pattern "one|two|three|four|five|six|seven|eight|nine"
                     name-to-number {"one" "1"
                                     "two" "2"
                                     "three" "3"
                                     "four" "4"
                                     "five" "5"
                                     "six" "6"
                                     "seven" "7"
                                     "eight" "8"
                                     "nine" "9"}]
                 (bigint (str (-> x
                                  (str/replace-first (re-pattern pattern) name-to-number)
                                  ((fn [s] (re-find #"[1-9]" s))))
                              (-> x
                                  str/reverse
                                  (str/replace-first
                                   (re-pattern (apply str (reverse pattern)))
                                   (into {} (map (fn [[key val]] [(str/reverse key) val]) name-to-number)))
                                  ((fn [s] (re-find #"[1-9]" s)))))))))
       (apply +))

  ;;Day 2 part 1
  (let [limit {"red" 12 "green" 13 "blue" 14}]
    (->> (slurp "src/advent_of_code_2023/day2.txt")
         (str/split-lines)
         (filter #(->> %
                       (re-seq #"\d+ (?:red|green|blue)")
                       (map (fn [s] (str/split s #" ")))
                       (every? (fn [[number color]] (<= (bigint number) (get limit color))))))
         (map (fn [s] (->> s
                           (take-while #(not= % \:))
                           (apply str)
                           (re-seq #"\d+")
                           first
                           bigint)))
         (apply +)))

  ;;Day 2 part 2
  (->> (slurp "src/advent_of_code_2023/day2.txt")
       (str/split-lines)
       (map #(->> %
                  (re-seq #"\d+ (?:red|green|blue)")
                  (map (fn [s] (str/split s #" ")))
                  (reduce (fn [result [number color]] (update result color (fn [cur-min]
                                                                             (if (> (bigint number) cur-min)
                                                                               (bigint number)
                                                                               cur-min)))) {"blue" 0 "green" 0 "red" 0})
                  vals
                  (apply *)))
       (apply +)))
