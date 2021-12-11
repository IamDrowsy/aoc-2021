(ns aoc2021.grid
  (:refer-clojure :exclude [get set])
  (:require [clojure.string :as str])
  (:import (java.util Map)))

(defprotocol Grid
  (set [this x y v])
  (get [this x y])
  (min-x [this])
  (max-x [this])
  (min-y [this])
  (max-y [this]))

(extend-type Map
  Grid
  (set [this x y v]
    (assoc this [x y] v))
  (get [this x y]
    (this [x y]))
  (min-x [this]
    (first (first (sort (keys this)))))
  (max-x [this]
    (first (last (sort (keys this)))))
  (min-y [this]
    (second (first (sort-by second (keys this)))))
  (max-y [this]
    (second (last (sort-by second (keys this))))))

(defn parse-grid [input]
  (apply merge (map #(into {} %)
                    (map-indexed (fn [y line]
                                   (map-indexed (fn [x cell]
                                                  [[x y] cell])
                                                line))
                                 (str/split-lines input)))))

(defn draw
  ([grid]
   (draw grid identity))
  ([grid mapping]
   (str/join "\n"
             (for [y (range (min-y grid) (inc (max-y grid)))]
               (str/join (for [x (range (min-x grid) (inc (max-x grid)))]
                           (str (mapping (grid [x y])))))))))




