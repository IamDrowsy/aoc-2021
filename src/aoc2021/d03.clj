(ns aoc2021.d03
  (:require [aoc2021.util :refer [get-input check]]
            [clojure.string :as str]))

(defn parse-line [line]
  (mapv parse-long (str/split line #"")))

(defn parse-input [input]
  (mapv parse-line (str/split-lines input)))

(defn add-vec [v1 v2]
  (mapv + v1 v2))

(defn bin-vec->int [v]
  (-> (str/join v)
      (Long/parseLong 2)))

(defn flip [n]
  (mod (inc n) 2))

(defn start-state [input]
  {:position 0
   :mode :most
   :candidates (parse-input input)})

(defn at-pos-fn [position]
  (fn [v] (v position)))

; mode can be :least or :most
(defn needed-bit-at-position [{:keys [candidates position mode]}]
  (let [f (if (= :least mode) flip identity)
        candidate# (double (count candidates))
        sum (reduce + (map (at-pos-fn position) candidates))]
    (f (Math/round ^double (/ sum candidate#)))))

(defn step [{:keys [position candidates] :as state}]
  (let [needed-bit-at-position (needed-bit-at-position state)
        new-candidates (filterv #(= needed-bit-at-position (% position)) candidates)]
    (merge state
           {:position (inc position)
            :candidates new-candidates})))

(defn solution [{candidates :candidates}]
  (case (count candidates)
    0 "No Solution"
    1 (bin-vec->int (first candidates))
    ; not solved yet
    nil))

(defn solve [inital]
  (->> (iterate step inital)
       (map solution)
       (drop-while nil?)
       first))

(defn solve-1 [input]
  (let [parsed (parse-input input)
        line# (double (count parsed))
        number-1 (->> (reduce add-vec parsed)
                      (mapv #(/ % line#))
                      (mapv #(Math/round ^double %)))
        number-2 (mapv flip number-1)]
    (* (bin-vec->int number-1)
       (bin-vec->int number-2))))

(defn solve-2 [input]
  (let [start (start-state input)]
    (* (solve start) (solve (assoc start :mode :least)))))

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2 (get-input))))
