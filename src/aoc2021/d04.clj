(ns aoc2021.d04
  (:require [aoc2021.util :refer [get-input check]]
            [clojure.string :as str]
            [datascript.core :as d]))

(def schema {:util/vars {:db/unique :db.unique/identity}
             :util/numbers {}
             :util/last-number {}
             :util/last-won {:db/cardinality :db.cardinality/one
                             :db/valueType :db.type/ref}
             :number/value {:db/unique :db.unique/identity}
             :number/rowcol {:db/cardinality :db.cardinality/many
                             :db/valueType :db.type/ref}
             :number/marked {}
             :rowcol/key {:db/unique :db.unique/identity}
             :rowcol/board {:db/cardinality :db.cardinality/one
                            :db/valueType :db.type/ref}
             :board/index {:db/unique :db.unique/identity}
             :board/won {}
             :board/score {}})

(defn mapcat-indexed [f coll]
  (mapcat f (range) coll))

(defn grid->transact-data [board-index input]
  (mapv (fn [{:keys [row col value]}]
          {:number/value  value
           :number/marked false
           :number/rowcol [{:rowcol/key   (str "board-" board-index "-col-" col)
                            :rowcol/board {:board/index board-index}}
                           {:rowcol/key (str "board-" board-index "-row-" row)
                            :rowcol/board {:board/index board-index}}]})
        (mapcat-indexed (fn [y line]
                          (map-indexed (fn [x cell]
                                         {:row y :col x :value (parse-long cell)})
                                       (str/split (str/trim line) #"\s+")))
                     (str/split-lines input))))

(defn prepare-state [input]
  (let [[numbers & boards] (str/split input #"\n\n")
        con (d/create-conn schema)]
    (d/transact! con [{:util/vars :vars
                       :util/numbers (map parse-long (str/split numbers #","))}])
    (dorun
      (map-indexed #(d/transact! con (grid->transact-data %1 %2)) boards))
    con))

(defn find-numbers [con]
  (first (d/q '[:find [?n ...]
                :where
                [?e :util/vars :vars]
                [?e :util/numbers ?n]]
              @con)))

(defn find-newly-won-boards-and-score [con]
  (d/q '[:find ?board-index ?last-number (sum ?unmarked-value)
         :where
         [_ :util/last-number ?last-number]
         [?rc :rowcol/key ?key]
         (not-join [?rc]
                   [?n :number/rowcol ?rc]
                   [?n :number/marked false])
         [?rc :rowcol/board ?board]
         (not [?board :board/won true])
         [?board :board/index ?board-index]
         [?all-rcs :rowcol/board ?board]
         [?unmarked :number/rowcol ?all-rcs]
         [?unmarked :number/marked false]
         [?unmarked :number/value ?unmarked-value]]
       @con))

(defn mark-next-number! [con]
  (let [open-numbers (find-numbers con)
        this-number (first open-numbers)]
    (d/transact! con [{:util/vars :vars
                       :util/numbers (rest open-numbers)
                       :util/last-number this-number}
                      {:number/value this-number :number/marked true}])
    (let [won-boards (find-newly-won-boards-and-score con)]
      (d/transact! con (mapcat (fn [[board-index last-number unmarked-sum]]
                                 [{:board/index board-index
                                   :board/won   true
                                   :board/score (* last-number unmarked-sum)}
                                  {:util/vars :var
                                   :util/last-won {:board/index board-index}}])
                            won-boards))
      con)))

(defn scores [con]
  (d/q '[:find [?score ...]
         :where
         [?board :board/won true]
         [?board :board/score ?score]]
       @con))

(defn solve-1 [input]
  (let [con (prepare-state input)]
    (while (empty? (scores con))
      (mark-next-number! con))
    (first (scores con))))

(defn all-won? [con]
  (empty? (d/q '[:find [?index ...]
                 :where
                 [?board :board/index ?index]
                 (not [?board :board/won])]
               @con)))

(defn last-won-score [con]
  (d/q '[:find ?score .
         :where
         [_ :util/last-won ?board]
         [?board :board/score ?score]]
       @con))

(defn solve-2 [input]
  (let [con (prepare-state input)]
    (while (not (all-won? con))
      (mark-next-number! con))
    (last-won-score con)))

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2 (get-input))))
