(ns aoc2021.util
  (:require [clj-http.client :as client]
            [clojure.string :as str])
  (:import (java.io File)))


(defn day-from-ns []
  (parse-long (second (re-find #"aoc2021\.d(..)" (str *ns*)))))

(defn- get-input-from-aoc [day]
  (let [session (slurp "session")]
    (:body
      (client/get (format "https://adventofcode.com/2021/day/%s/input" day)
                  {:headers {"Cookie" (str "session=" session)}}))))

(defn- get-input* [day]
  (let [input-file (format "data/d%02d-input.txt" day)]
    (if (.exists (File. input-file))
      (slurp input-file)
      (let [input (str/trim-newline (get-input-from-aoc day))]
        (spit input-file input)
        input))))

(def mem-get-input* (memoize get-input*))

(defn get-input []
  (mem-get-input* (day-from-ns)))

(defn check
  ([part solution]
   (check (day-from-ns) part solution))
  ([day part solution]
   (let [solution-file (format "data/d%02d-part-%d.txt" day part)]
     (if (.exists (File. solution-file))
       (assert (= (slurp solution-file) (str solution)))
       (spit solution-file solution)))))

(defn fix-point [f x]
  "Calculates the fix point of f with respect to x."
  (reduce #(if (= %1 %2) (reduced %1) %2)
          (iterate f x)))

(defn median [col]
  (let [sorted (sort col)
        len (count col)
        mid (/ len 2)]
    (if (zero? (mod len 2))
      (* 0.5 (+ (nth sorted (Math/floor mid)) (nth sorted (Math/ceil mid))))
      (nth sorted mid))))
