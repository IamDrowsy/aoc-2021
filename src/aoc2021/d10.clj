(ns aoc2021.d10
  (:require [aoc2021.util :refer [get-input check median]]
            [clojure.string :as str]))

(def paren-mapping
  {\( \)
   \< \>
   \[ \]
   \{ \}})

(defn open? [char]
  ((set (keys paren-mapping)) char))

(defn matching? [[first & _] char]
  (= char first))

(def error-scores
  {\) 3 \] 57 \} 1197 \> 25137})

(defn step [stack char]
  (cond (open? char) (conj stack (paren-mapping char))
        (matching? stack char) (rest stack)
        :else (reduced {:error-char char})))

(defn error-score [line]
  (let [result (reduce step nil line)]
    (if (map? result)
      (error-scores (:error-char result))
      0)))

(def completion-scores
  {\) 1 \] 2 \} 3 \> 4})

(defn completion-score [line]
  (let [result (reduce step nil line)]
    (if (list? result)
      (reduce (fn [score char]
                (+ (* score 5)
                   (completion-scores char)))
              0
              result)
      0)))

(defn solve-1 [input]
  (->> (str/split-lines input)
       (map error-score)
       (reduce +)))

(defn solve-2 [input]
  (->> (str/split-lines input)
       (map completion-score)
       (remove zero?)
       (median)))

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2 (get-input))))
