(ns aoc2021.d08
  (:require [aoc2021.util :refer [get-input check]]
            [clojure.string :as str]
            [com.rpl.specter :refer :all]
            [clojure.set :as set]))

(def all #{:a :b :c :d :e :f :g})
(def init [(zipmap all (repeat all))])

(defn restrict-possibility [solutions sources possible-targets]
  (transform [ALL (submap sources) MAP-VALS] #(set/intersection possible-targets %)
          solutions))

(defn safe-mappings [solution]
  (multi-transform [MAP-VALS (if-path [(view count) (pred= 1)] (terminal first)
                                      (terminal-val NONE))]
                   solution))

(defn unsolvable? [solution]
  (or (some #(= 0 %) (map count (vals solution)))
      (not= all (reduce set/union (vals solution)))
      (let [solved-targets (vals (safe-mappings solution))]
        (not= (count solved-targets) (count (dedupe solved-targets))))))

(defn restrict-possibilities [solutions sources possible-targets-vec]
  (mapcat (partial restrict-possibility solutions sources) possible-targets-vec))

(defn restrict [solutions pattern]
  (let [f (partial restrict-possibilities solutions pattern)]
    (->> (case (count pattern)
           2 (f [#{:c :f}])
           3 (f [#{:a :c :f}])
           4 (f [#{:b :c :d :f}])
           5 (f [#{:a :c :d :e :g} #{:a :c :d :f :g} #{:a :b :d :f :g}])
           6 (f [#{:a :b :c :e :f :g} #{:a :b :d :e :f :g} #{:a :b :c :d :f :g}])
           7 solutions)
         (remove unsolvable?))))

(defn reduce-safe-mappings [solution]
  (let [safe-targets (into #{} (vals (safe-mappings solution)))
        unsafe-keys (keys (setval [MAP-VALS (view count) (pred= 1)] NONE solution))]
    (transform [(submap unsafe-keys) MAP-VALS] #(set/difference % safe-targets)
               solution)))

(defn ->solution-mapping [solution]
  (safe-mappings (first (drop-while #(not (= 7 (count (safe-mappings %))))
                                    (iterate reduce-safe-mappings solution)))))

(defn solve [pattern]
  (->> (reduce restrict init pattern)
       (first)
       (->solution-mapping)))

(def set->number
  {#{:a :b :c :e :f :g} 0
   #{:c :f} 1
   #{:a :c :d :e :g} 2
   #{:a :c :d :f :g} 3
   #{:b :c :d :f} 4
   #{:a :b :d :f :g} 5
   #{:a :b :d :e :f :g} 6
   #{:a :c :f} 7
   #{:a :b :c :d :e :f :g} 8
   #{:a :b :c :d :f :g} 9})

(defn signals->number [solution signals]
  (->> (map set signals)
       (transform [ALL ALL] solution)
       (map set->number)
       (apply str)
       (parse-long)))

(defn parse-signal [signal]
  (->> (str/trim signal)
       (map str)
       (mapv keyword)))

(defn parse-signals [signals]
  (->> (str/split (str/trim signals) #" ")
       (mapv parse-signal)))

(defn parse-input-line [input]
  (let [[pattern-string output-string] (str/split input #"\|")]
    {:pattern (parse-signals pattern-string)
     :output (parse-signals output-string)}))

(defn parse-input [input]
  (mapv parse-input-line (str/split-lines input)))

(defn solve-1 [input]
  (->> (parse-input input)
       (mapcat :output)
       (map count)
       (filter #{2 3 4 7})
       (count)))

(defn solve-2-single [{:keys [pattern output]}]
  (let [solution (solve pattern)]
    (signals->number solution output)))

(defn solve-2 [input]
  (->> (parse-input input)
       (mapv solve-2-single)
       (reduce +)))

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2 (get-input))))
