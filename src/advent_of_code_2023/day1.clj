(ns advent-of-code-2023.day1
  (:require [clojure.string :as str]))

(defn part1 [input-path]
  (->> (slurp input-path)
       (str/split-lines)
       (map (fn [x]
              (->> x
                   (map #(re-matches #"[1-9]" (str %)))
                   (remove nil?)
                   ((juxt first last))
                   str/join
                   bigint)))
       (apply +)))

(defn part2 [input-path]
  (->> (slurp input-path)
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
       (apply +)))
