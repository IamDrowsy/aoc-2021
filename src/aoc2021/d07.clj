(ns aoc2021.d07
  (:require [aoc2021.util :refer [get-input check]]
            [clojure.string :as str]))

(defn parse-input [input]
  (mapv parse-long (str/split input #",")))

(defn median [col]
  (let [sorted (sort col)
        len (count col)
        mid (/ len 2)]
    (if (zero? (mod len 2))
      (* 0.5 (+ (nth sorted (Math/floor mid)) (nth sorted (Math/ceil mid))))
      (nth sorted mid))))

(defn mean [col]
  (double (/ (reduce + col)
             (count col))))

(defn solve-1 [input]
  (let [parsed (parse-input input)
        m (median parsed)]
    (int (reduce + (mapv #(Math/abs (- m %)) parsed)))))

(defn cost [center number]
  (let [dist (Math/abs (- number center))]
    (* 0.5 dist (inc dist))))

(defn costs [col center]
  (->> col
       (mapv (partial cost center))
       (reduce +)))

(defn solve-2 [input]
  (let [parsed (parse-input input)
        mean (long (Math/round (mean parsed)))
        median (long (median parsed))]
    (long (apply min (map (partial costs parsed)
                          (range (- median mean)
                                 (+ median mean)))))))

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2 (get-input))))
