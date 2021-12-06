(ns aoc2021.d06
  (:require [aoc2021.util :refer [get-input check]]
            [clojure.string :as str]
            [com.rpl.specter :refer :all]))

(defn parse [input]
  (let [freqs (frequencies (mapv parse-long (str/split input #",")))]
    (map (fn [index]
           (get freqs index 0))
         (range 9))))

(defn tick [[will-spawn & rest]]
  (->> rest
       (setval [AFTER-ELEM] will-spawn)
       (transform [(nthpath 6)] #(+ will-spawn %))))

(defn tick-for-days [start days]
  (first (drop days (iterate tick start))))

(defn solve-1 [input]
  (reduce + (tick-for-days (parse input) 80)))

(defn solve-2 [input]
  (reduce + (tick-for-days (parse input) 256)))

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2 (get-input))))
