(ns advent-of-code-2023.day7
  (:require [clojure.string :as str]))

(def five-kind #(= 1 (count (set %))))
(def four-kind #(some (fn [[_ c]] (= 4 c)) (frequencies %)))
(def full-house #(= #{2 3} (set (map second (frequencies %)))))
(def three-kind #(some (fn [[_ c]] (= 3 c)) (frequencies %)))
(def two-pair #(= (list 1 2 2) (sort (map second (frequencies %)))))
(def one-pair #(some (fn [[_ c]] (= 2 c)) (frequencies %)))
(def high-card (constantly true))
(def hand-ordering (map-indexed vector [five-kind four-kind full-house three-kind two-pair one-pair high-card]))

(defn part1 [input-path]
  (let [card-ordering (into {} (map-indexed (fn [idx c] [c idx]) "AKQJT98765432"))]
    (->> (slurp input-path)
         (str/split-lines)
         (map #(str/split % #" "))
         (sort-by (fn [[hand _]]
                    (some (fn [[idx hand-type]]
                            (when (hand-type hand)
                              (into [] (concat [idx]
                                               (mapv (fn [c] (get card-ordering c)) hand))))) hand-ordering)))
         reverse
         (map-indexed (fn [idx [_ bid]]
                        (* (+ 1 idx) (bigint bid))))
         (apply +))))

(defn part2 [input-path]
  (let [card-ordering (into {} (map-indexed (fn [idx c] [c idx]) "AKQT98765432J"))
        hand-ordering (map-indexed vector [five-kind four-kind full-house three-kind two-pair one-pair high-card])]
    (letfn [(possible-hands [original-hand]
              (let [j-place (first (map first (filter (fn [[_ c]] (= \J c)) (map-indexed vector original-hand))))]
                (if j-place
                  (apply concat (map possible-hands (for [card "AKQT98765432"]
                                                      (apply str (assoc (into [] original-hand) j-place card)))))
                  [original-hand])))]
      (->> (slurp input-path)
           (str/split-lines)
           (map #(str/split % #" "))
           (sort-by (fn [[hand _]]
                      (first (sort (for [possible-hand (possible-hands hand)]
                               (some (fn [[idx hand-type]]
                                       (when (hand-type possible-hand)
                                         (into [] (concat [idx]
                                                          (mapv (fn [c] (get card-ordering c)) hand))))) hand-ordering))))))
           reverse
           (map-indexed (fn [idx [_ bid]]
                          (* (+ 1 idx) (bigint bid))))
           (apply +)))))

(comment
  (part1 "src/advent_of_code_2023/day7.txt")
  (part2 "src/advent_of_code_2023/day7.txt"))
