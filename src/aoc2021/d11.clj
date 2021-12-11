(ns aoc2021.d11
  (:require [aoc2021.util :refer [get-input check]]
            [aoc2021.grid :as g]
            [flatland.useful.map :as m]
            [clojure.set :as set]))

(defn parse-input [input]
  (update-vals (g/parse-grid input)
               (comp parse-long str)))

(defn neighbor-keys [[x y]]
  (for [fx [dec identity inc]
        fy [dec identity inc]
        :let [nx (fx x) ny (fy y)]
        :when (and (<= 0 nx 9)
                   (<= 0 ny 9))]
    [nx ny]))

(defn init [input]
  {:index 0
   :flashed# 0
   :all false
   :octopi (parse-input input)})

(defn step [{:keys [index flashed# octopi]}]
  (loop [flashed #{}
         octopi (update-vals octopi inc)]
    (let [about-to-flash (m/filter-keys-by-val #(< 9 %) (m/remove-keys octopi flashed))]
      (if (empty? about-to-flash)
        {:flashed# (+ flashed# (count flashed))
         :index    (inc index)
         :all      (= 100 (count flashed))
         :octopi   (update-vals octopi #(if (< 9 %) 0 %))}
        (let [flash-result (->> about-to-flash
                                (mapcat neighbor-keys)
                                (remove flashed)
                                (frequencies))]
          (recur (set/union flashed (set about-to-flash))
                 (merge-with + octopi flash-result)))))))

(defn solve-1 [input]
  (:flashed# (nth (iterate step (init input)) 100)))

(defn solve-2 [input]
  (:index (first (drop-while #(not (:all %)) (iterate step (init input))))))

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2 (get-input))))
