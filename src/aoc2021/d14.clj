(ns aoc2021.d14
  (:require [aoc2021.util :refer [get-input check]]
            [com.rpl.specter :refer :all]
            [clojure.string :as str]))

(defn parse-rule [rule]
  (let [[source target] (str/split rule #" -> ")]
    [source [(str (first source) target)
             (str target (last source))]]))

(defn parse-input [input]
  (let [[template rules] (str/split input #"\n\n")]
    {:first-char (first template)
     :last-char (last template)
     :template (frequencies (map (partial apply str) (partition 2 1 template)))
     :rules    (into {} (map parse-rule (str/split-lines rules)))}))

(defn add-pair-parts [result [f l] v]
  (-> result
      (update f #(+ v (or % 0)))
      (update l #(+ v (or % 0)))))

(defn step [rules freqs]
  (reduce-kv (fn [result k v]
               (add-pair-parts result (rules k) v))
             {}
             freqs))

(defn char-freqs [first last freqs]
  (-> (reduce-kv add-pair-parts
                 (if (= first last) {first 2} {first 1 last 1})
                 freqs)
    (update-vals #(/ % 2))))

(defn score [first-char last-char freqs]
  (let [sorted (->> (char-freqs first-char last-char freqs)
                    (sort-by val))]
    (- (val (last sorted)) (val (first sorted)))))

(defn solve* [input steps]
  (let [{:keys [template rules first-char last-char]} (parse-input input)]
    (score first-char last-char
           (nth (iterate (partial step rules) template)
                steps))))

(defn solve-1 [input]
  (solve* input 10))

(defn solve-2 [input]
  (solve* input 40))

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2 (get-input))))
