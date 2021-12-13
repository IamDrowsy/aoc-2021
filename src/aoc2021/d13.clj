(ns aoc2021.d13
  (:require [aoc2021.util :refer [get-input check]]
            [clojure.string :as str]
            [com.rpl.specter :refer :all]))

(defn parse-input [input]
  (let [[grid folds] (str/split input #"\n\n")]
    {:grid  (->> (str/split-lines grid)
                 (map #(str/split % #","))
                 (transform [ALL ALL] parse-long)
                 (set))
     :folds (->> (str/split-lines folds)
                 (mapv (fn [s]
                         (let [[_ dim val] (re-find #"fold along ([x|y])=(\d+)" s)]
                           [(keyword dim) (parse-long val)]))))}))

(defn draw [grid]
  (let [min-x (apply min (map first grid))
        min-y (apply min (map second grid))
        max-x (apply max (map first grid))
        max-y (apply max (map second grid))]
    (str/join "\n"
              (for [y (range min-y (inc max-y))]
                (str/join (for [x (range min-x (inc max-x))]
                            (if (grid [x y]) "#" "-")))))))

(defn fold [grid [dim val]]
  (transform [ALL (if (= dim :x) FIRST LAST) (pred> val)] #(- (* 2 val) %) grid))

(defn solve-1 [input]
  (let [{:keys [grid folds]} (parse-input input)]
    (count (reduce fold grid (take 1 folds)))))

(defn solve-2 [input]
  (let [{:keys [grid folds]} (parse-input input)]
    (draw (reduce fold grid folds))))

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2 (get-input))))
