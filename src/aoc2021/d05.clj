(ns aoc2021.d05
  (:require [aoc2021.util :refer [get-input check]]
            [clojure.string :as str]))

(defn parse-point [point]
  (let [[x y] (str/split point #",")]
    [(parse-long x) (parse-long y)]))

(defn parse-line [line]
  (let [[start end] (str/split line #" -> ")]
    [(parse-point start)
     (parse-point end)]))

(defn parse-input [input]
  (mapv parse-line (str/split-lines input)))

(defn diag-points [sx sy ex ey]
  (mapv vector
        (range sx (if (< sx ex) (inc ex) (dec ex)) (if (< sx ex) 1 -1))
        (range sy (if (< sy ey) (inc ey) (dec ey)) (if (< sy ey) 1 -1))))

(defn line->all-points [with-diags [[sx sy] [ex ey]]]
  (cond (= sx ex)
        (mapv vector (repeat sx) (range (min sy ey) (inc (max sy ey))))
        (= sy ey)
        (mapv vector (range (min sx ex) (inc (max sx ex))) (repeat sy))
        :else
        (if with-diags
          (diag-points sx sy ex ey)
          [])))

(defn all-points [with-diags lines]
  (mapcat (partial line->all-points with-diags) lines))

(defn solve* [with-diag input]
  (->> (parse-input input)
     (all-points with-diag)
     (frequencies)
     (filter #(<= 2 (second %)))
     (count)))

(defn solve-1 [input]
  (solve* false input))

(defn solve-2 [input]
  (solve* true input))

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2 (get-input))))
