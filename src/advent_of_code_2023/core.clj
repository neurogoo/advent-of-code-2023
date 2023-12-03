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
       (apply +))

  ;;Day 3 part 1
  (let [input (->> (slurp "src/advent_of_code_2023/day3.txt")
                   (str/split-lines))
        hotzones (->> (for [i (range 0 (count input))
                            j (range 0 (count (first input)))
                            :when (re-find #"[^0-9\.]" (str (get-in input [i j])))]
                        (for [x (range -1 2)
                              y (range -1 2)
                              :let [new-i (+ i y)
                                    new-j (+ j x)]
                              :when (and (>= new-i 0) (<= new-i (count input))
                                         (>= new-j 0) (<= new-j (count (first input)))
                                         (not= [y x] [new-i new-j])
                                         (re-find #"[0-9]" (str (get-in input [new-i new-j]))))]
                          [new-i new-j]))
                      (apply concat)
                      (into #{}))]
    (->> (for [i (range 0 (count (first input)))]
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
                            ((fn [[indices numbers]] [indices (bigint (apply str numbers))])))))))
         (apply concat)
         (filter (fn [[indices _]] (some hotzones indices)))
         (map second)
         (apply +)))

  ;;Day 3 part 2
  (let [input (->> (slurp "src/advent_of_code_2023/day3.txt")
                   (str/split-lines))
        hotzones (->> (for [i (range 0 (count input))
                            j (range 0 (count (first input)))
                            :when (re-find #"[\*]" (str (get-in input [i j])))]
                        (for [x (range -1 2)
                              y (range -1 2)
                              :let [new-i (+ i y)
                                    new-j (+ j x)]
                              :when (and (>= new-i 0) (<= new-i (count input))
                                         (>= new-j 0) (<= new-j (count (first input)))
                                         (not= [y x] [new-i new-j])
                                         (re-find #"[0-9]" (str (get-in input [new-i new-j]))))]
                          [new-i new-j]))
                      (remove empty?)
                      (map set))
        numbers (->> (for [i (range 0 (count (first input)))]
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
                                        ((fn [[indices numbers]] [indices (bigint (apply str numbers))])))))))
                     (apply concat))]
    (->> (for [gear hotzones]
           (filter (fn [[zones _]]
                     (some gear zones)) numbers))
         (filter #(= 2 (count %)))
         (map #(->> %
                    (map second)
                    (apply *)))
         (apply +))))
