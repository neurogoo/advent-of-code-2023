(ns advent-of-code-2023.day6)

(defn solver [time-distance-vector]
  (for [[time distance] time-distance-vector]
    (let [a -1
          b time
          c (* -1 (+ 1 distance))]
      (- (+ 1 (Math/floor (/ (- (* -1 b) (Math/sqrt (- (Math/pow b 2) (* 4 a c)))) (* 2 a))))
         (Math/ceil (/ (+ (* -1 b) (Math/sqrt (- (Math/pow b 2) (* 4 a c)))) (* 2 a)))))))

(defn part1 [v] (apply * (solver v)))
(defn part2 [v] (first (solver v)))
