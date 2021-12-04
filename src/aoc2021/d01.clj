(ns aoc2021.d01
  (:require [aoc2021.util :refer [get-input check]]
            [clojure.string :as str]))

(defn solve-1 [input]
  (->> input
       (str/split-lines)
       (map parse-long)
       (partition 2 1)
       (filter (fn [[x1 x2]] (< x1 x2)))
       (count)))

(defn solve-2 [input]
  (->> input
       (str/split-lines)
       (map parse-long)
       (partition 3 1)
       (map #(apply + %))
       (partition 2 1)
       (filter (fn [[x1 x2]] (< x1 x2)))
       (count)))

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2 (get-input))))
