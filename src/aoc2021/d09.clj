(ns aoc2021.d09
  (:require [aoc2021.util :refer [get-input check fix-point]]
            [clojure.string :as str]
            [datascript.core :as d]
            [clojure.set :as set]))

(def schema
  {:point/key {:db/unique :db.unique/identity}
   :point/value {}
   :point/next {:db/cardinality :db.cardinality/many
                :db/valueType :db.type/ref}
   :point/up {:db/cardinality :db.cardinality/many
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
                        [(>= ?center-value ?next-value)])]
             [(basin-rec ?center ?upper)
              [?center :point/next ?upper]
              [?upper :point/value ?uvalue]
              [(not= ?uvalue 9)]
              [?center :point/value ?cvalue]
              [(< ?cvalue ?uvalue)]]
             [(basin-rec ?center ?upper)
              [?center :point/next ?between]
              [?between :point/value ?bvalue]
              [(not= ?bvalue 9)]
              [?center :point/value ?cvalue]
              [(< ?cvalue ?bvalue)]
              (basin-rec ?between ?upper)]
             [(basin-up ?center ?upper)
              [?center :point/up ?upper]]
             [(basin-up ?center ?upper)
              [?center :point/up ?between]
              (basin-up ?between ?upper)]])


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
  (count (fix-point (partial basin-points* con) #{low-point})))

(defn basin-points-rec [con low-point]
  (inc (d/q '[:find (count ?e) .
              :in $ % ?c
              :where
              (basin-rec ?c ?e)]
            @con rules low-point)))

(defn low-points [con]
  (d/q '[:find [?low-point ...]
         :in $ %
         :where
         [?low-point :point/value _]
         (low-point ?low-point)]
       @con rules))

(defn add-upper-information! [con]
  (d/transact! con (mapv (fn [[e u]]
                           [:db/add e :point/up u])
                         (d/q '[:find ?e ?u
                                :where
                                [?e :point/next ?u]
                                [?e :point/value ?eval]
                                [?u :point/value ?uval]
                                [(< ?eval ?uval)]
                                [(not= ?uval 9)]]
                              @con))))

(defn basin-points-up [con low-point]
  (inc (d/q '[:find (count ?e) .
              :in $ % ?c
              :where
              (basin-up ?c ?e)]
            @con rules low-point)))

(defn basin-points-up-all [con]
  (->> (d/q '[:find ?c (count ?e)
              :in $ %
              :where
              [?c :point/value _]
              (low-point ?c)
              (basin-up ?c ?e)]
            @con rules)
       (map second)
       (sort)
       (reverse)
       (take 3)
       (map inc)))

(defn solve-2 [input]
  (let [con (init input)]
    (->> (low-points con)
         (map (partial basin-points con))
         (sort)
         (reverse)
         (take 3)
         (apply *))))

(defn solve-2-recursive [input]
  (let [con (init input)]
    (->> (low-points con)
         (map (partial basin-points-rec con))
         (sort)
         (reverse)
         (take 3)
         (apply *))))

(defn solve-2-up [input]
  (let [con (init input)]
    (add-upper-information! con)
    (->> (low-points con)
         (map (partial basin-points-up con))
         (sort)
         (reverse)
         (take 3)
         (apply *))))

(defn solve-2-up-all [input]
  (let [con (init input)]
    (add-upper-information! con)
    (apply * (basin-points-up-all con))))

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2-up-all (get-input))))
