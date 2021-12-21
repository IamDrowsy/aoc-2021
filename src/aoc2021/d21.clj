(ns aoc2021.d21
  (:require [aoc2021.util :refer [get-input check]]
            [clojure.math.combinatorics :as c]))



(defn state [input]
  (let [[_ p1-start p2-start] (re-matches #"Player 1 starting position: (\d+)\nPlayer 2 starting position: (\d+)" input)]
    {:player1 {:score 0
               :space (parse-long p1-start)}
     :player2 {:score 0
               :space (parse-long p2-start)}
     :turn    :player1
     :turn#   0}))

(def next-player
  {:player1 :player2
   :player2 :player1})

(defn take-player-turn [{:keys [score space]} rolled]
  (let [new-space (inc (mod (dec (reduce + space rolled)) 10))]
    {:score (+ score new-space)
     :space new-space}))

(defn won? [state]
  (or (<= 1000 (:score (:player1 state)))
      (<= 1000 (:score (:player2 state)))))

(defn take-turn [state rolled]
  (if (won? state)
    (reduced state)
    (-> state
        (update :turn# #(+ 3 %))
        (update (:turn state) take-player-turn rolled)
        (update :turn next-player))))

(defn solve-1 [input]
  (let [result (reduce take-turn (state input) (partition 3 (range 1 1001)))]))

(def points->universe#
  (->> (c/selections [1 2 3] 3)
       (map #(reduce + %))
       (frequencies)))

(defn state2 [start-space]
  {:score 0
   :space start-space
   :universe# 1})

(defn step2 [state]
  )

(defn solve-2 [input])

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2 (get-input))))
