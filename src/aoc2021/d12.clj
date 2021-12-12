(ns aoc2021.d12
  (:require [aoc2021.util :refer [get-input check fix-point]]
            [com.rpl.specter :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.core.reducers :as r]
            [criterium.core :refer [quick-bench]]))

(defn add-entry [cave-map start end]
  (if (or (= end "start")
          (= start "end"))
    cave-map
    (setval [(keypath start) NONE-ELEM] end cave-map)))

(defn add-to-cave-map [cave-map [start end]]
  (-> cave-map
      (add-entry start end)
      (add-entry end start)))

(defn input->pairs [input]
  (->> (str/split-lines input)
       (map #(str/split % #"-"))))

(defn parse-cave-map [input]
  (reduce add-to-cave-map {} (input->pairs input)))

(defn small? [name]
  (= name (str/lower-case name)))

(defn finished? [path]
  (= "end" (peek path)))

(defn step-path [cave-map allow-twice? path]
  (let [current (peek path)
        {:keys [visited-small visited-twice]} (meta path)
        new-visited-twice (or visited-twice (visited-small current))
        possible-next (set/difference (cave-map current #{})
                                      (if (or (not allow-twice?) new-visited-twice) visited-small #{}))
        add-step (if (small? current)
                   (let [new-meta {:visited-small (conj visited-small current)
                                   :visited-twice new-visited-twice}]
                     (fn [next] (with-meta (conj path next) new-meta)))
                   (partial conj path))]
    (r/map add-step possible-next)))

(defn step-paths [cave-map allow-twice? path-set]
  (r/foldcat
    (r/mapcat (partial step-path cave-map allow-twice?) path-set)))

(def start-path
  (with-meta ["start"] {:visited-small #{}
                        :visited-twice false}))

(defn complete-path-count-fn [cave-map allow-twice?]
  (let [path# (volatile! 0)
        f (partial step-paths cave-map allow-twice?)]
    (fn [open-path]
      (if (number? open-path)
        open-path
        (let [new-paths (f open-path)
              unfinished (vec (remove finished? new-paths))
              finished# (count (filter finished? new-paths))]
          (vswap! path# + finished#)
          (if (empty? unfinished)
            @path#
            unfinished))))))

(defn solve [input allow-twice?]
  (fix-point (complete-path-count-fn (parse-cave-map input) allow-twice?)
             [start-path]))

(defn solve-1 [input]
  (solve input false))

(defn solve-2 [input]
  (solve input true))

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2 (get-input))))
