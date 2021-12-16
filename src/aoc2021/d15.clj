(ns aoc2021.d15
  (:require [aoc2021.util :refer [get-input check]]
            [loom.graph :as lg]
            [loom.alg :as la]
            [aoc2021.grid :as g]
            [com.rpl.specter :refer :all]))

(defn neighbors [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn graph-targets [grid source]
  (select-keys grid (neighbors source)))

(defn parse-grid [input]
  (update-vals (g/parse-grid input) (comp parse-long str)))

(defn build-graph [grid]
  (lg/weighted-digraph (zipmap (keys grid)
                               (map (partial graph-targets grid) (keys grid)))))

(defn dim-mul-f [max-x x]
  #(+ (* x max-x) %))

(defn wrap-add-f [x]
  (fn [y]
    (inc (mod (dec (+ x y)) 9))))

(defn extend-grid [grid [x+ y+]]
  (let [max-dim (long (Math/sqrt (count grid)))]
    (multi-transform [(multi-path [MAP-KEYS FIRST (terminal (dim-mul-f max-dim x+))]
                                  [MAP-KEYS LAST (terminal (dim-mul-f max-dim y+))]
                                  [MAP-VALS (terminal (wrap-add-f (+ x+ y+)))])]
                     grid)))

(defn full-grid [grid]
  (->> (for [x (range 0 5)
             y (range 0 5)]
         [x y])
       (map (partial extend-grid grid))
       (reduce merge)))

(defn solve [graph]
  (let [last (last (sort (lg/nodes graph)))]
    (second (la/dijkstra-path-dist graph [0 0] last))))

(defn solve-1 [input]
  (solve (build-graph (parse-grid input))))

(defn solve-2 [input]
  (solve (build-graph (full-grid (parse-grid input)))))

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2 (get-input))))
