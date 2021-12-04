(ns aoc2021.d02
  (:require [aoc2021.util :refer [get-input check]]
            [clojure.string :as str]))

(defn parse-line [line]
  (let [[dir val] (str/split line #" ")]
    {:direction (keyword dir)
     :value (parse-long val)}))

(defn parse-input [input]
  (mapv parse-line (str/split-lines input)))

(defn change-pos1 [[x y] {:keys [direction value]}]
  (case direction
    :forward [(+ x value) y]
    :down [x (+ y value)]
    :up [x (- y value)]))

(defn solve-1 [input]
  (->> (parse-input input)
       (reduce change-pos1 [0 0])
       (apply *)))

(defn change-pos2 [[pos depth aim] {:keys [direction value]}]
  (case direction
    :forward [(+ pos value) (+ depth (* aim value)) aim]
    :down [pos depth (+ aim value)]
    :up [pos depth (- aim value)]))

(defn solve-2 [input]
  (->> (parse-input input)
       (reduce change-pos2 [0 0 0])
       (take 2)
       (apply *)))

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2 (get-input))))
