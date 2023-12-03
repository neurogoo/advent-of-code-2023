(ns advent-of-code-2023.day2
  (:require [clojure.string :as str]))

(defn part1 [input-path]
  (let [limit {"red" 12 "green" 13 "blue" 14}]
    (->> (slurp input-path)
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
         (apply +))))

(defn part2 [input-path]
  (->> (slurp input-path)
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
