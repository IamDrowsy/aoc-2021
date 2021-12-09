(ns aoc2021.d09
  (:require [aoc2021.util :refer [get-input check fix-point]]
            [clojure.string :as str]
            [datascript.core :as d]
            [clojure.set :as set]))

(def schema
  {:point/key {:db/unique :db.unique/identity}
   :point/value {}
   :point/next {:db/cardinality :db.cardinality/many
                :db/valueType :db.type/ref}})

(defn mapcat-indexed [f coll]
  (mapcat f (range) coll))

(defn point->point-transaction [row col value]
  {:point/key   (str row "-" col)
   :point/value (parse-long (str value))
   ;; its easier to query when we have all directions
   :point/next  [{:point/key (str (dec row) "-" col)}
                 {:point/key (str (inc row) "-" col)}
                 {:point/key (str row "-" (dec col))}
                 {:point/key (str row "-" (inc col))}]})

(defn line->point-transaction [row line]
  (map-indexed (partial point->point-transaction row) line))

(defn grid->point-transaction [grid]
  (let [lines (str/split-lines grid)]
    (mapcat-indexed line->point-transaction lines)))

(defn init [grid]
  (let [con (d/create-conn schema)]
    (d/transact! con (grid->point-transaction grid))
    con))

(def rules '[[(low-point ?center)
              (not-join [?center]
                        [?center :point/value ?center-value]
                        [?center :point/next ?next]
                        [?next :point/value ?next-value]
                        [(>= ?center-value ?next-value)])]])

(defn sum-low-points [con]
  (d/q '[:find (sum ?risk) .
         :with ?c
         :in $ %
         :where
         [?c :point/value ?val]
         [(+ ?val 1) ?risk]
         (low-point ?c)]
       @con rules))

(defn solve-1 [input]
  (sum-low-points (init input)))

(defn basin-points* [con current-basin]
  (set/union (into #{} (d/q '[:find [?basin ...]
                              :in $ [?current-basin ...]
                              :where
                              [?current-basin :point/next ?basin]
                              [?basin :point/value ?basin-value]
                              [(not= ?basin-value 9)]]
                            @con current-basin))
             current-basin))

(defn basin-points [con low-point]
  (fix-point (partial basin-points* con) #{low-point}))

(defn low-points [con]
  (d/q '[:find [?low-point ...]
         :in $ %
         :where
         [?low-point :point/value _]
         (low-point ?low-point)]
       @con rules))

(defn solve-2 [input]
  (let [con (init input)]
    (->> (low-points con)
         (map (partial basin-points con))
         (map count)
         (sort)
         (reverse)
         (take 3)
         (apply *))))

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2 (get-input))))
