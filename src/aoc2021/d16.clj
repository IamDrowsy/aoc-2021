(ns aoc2021.d16
  (:require [aoc2021.util :refer [get-input check]]
            [clojure.string :as str]
            [clojure.walk :as w]))

(def hex->binary* {\0 "0000" \1 "0001" \2 "0010" \3 "0011"
                   \4 "0100" \5 "0101" \6 "0110" \7 "0111"
                   \8 "1000" \9 "1001" \A "1010" \B "1011"
                   \C "1100" \D "1101" \E "1110" \F "1111"})

(defn hex->binary [hex]
  (apply str (map hex->binary* hex)))

(defn binary->long [binary]
  (Long/parseLong binary 2))

(declare parse-package*)

(defn ->literal [literal]
  ;we use a list that evals to the number to attach metadata
  (list identity literal))

(defn parse-literal* [payload]
  (let [group (subs payload 0 5)]
    (if (str/starts-with? group "1")
      (let [[parsed unparsed] (parse-literal* (subs payload 5))]
        [(str (subs group 1 5) parsed) unparsed])
      [(subs group 1 5) (subs payload 5)])))

(defn parse-literal [payload]
  #_(println "Literal: " payload)
  (let [[parsed unparsed] (parse-literal* payload)]
    [(->literal (binary->long parsed)) unparsed]))

(defn bool->01 [f]
  (fn [x y]
    (if (f x y) 1 0)))

(def ->opfn
  {0 +
   1 *
   2 min
   3 max
   5 `(bool->01 >)
   6 `(bool->01 <)
   7 `(bool->01 =)})

(defn parse-total-length-operator* [op payload]
  (seq (into [(->opfn op)] (first (parse-package* payload)))))

(defn parse-total-length-operator [op payload]
  (let [length (binary->long (subs payload 0 15))
        this-package (subs payload 15 (+ 15 length))
        unparsed (subs payload (+ 15 length))]
    [(parse-total-length-operator* op this-package) unparsed]))

(defn parse-package-count-params* [packages-left payload]
  (let [[package unparsed] (parse-package* payload)]
    (if (= packages-left 1)
      [package unparsed]
      (let [[next-package next-unparsed] (parse-package-count-params* (dec packages-left) unparsed)]
        [(into package next-package) next-unparsed]))))

(defn parse-package-count-operator [op payload]
  (let [package-count (binary->long (subs payload 0 11))
        [parsed _] (parse-package* (subs payload 11))]
    [(seq (into [(->opfn op)] (take package-count parsed))) (drop package-count parsed)]))

(defn parse-package* [binary-package]
  #_(println "Package" binary-package)
  (if (< (count binary-package) 11)
    []
    (let [version (binary->long (subs binary-package 0 3))
          type (binary->long (subs binary-package 3 6))
          length-type (subs binary-package 6 7)]
      (if (= type 4)
        (let [[literal unparsed] (parse-literal (subs binary-package 6))
              [next-packages next-unparsed] (parse-package* unparsed)]
          [(into [(with-meta literal {:version version})] next-packages) next-unparsed])
        (if (= length-type "0")
          (let [[package unparsed] (parse-total-length-operator type (subs binary-package 7))
                [next-packages next-unparsed] (parse-package* unparsed)]
            [(into [(with-meta package {:version version})] next-packages) next-unparsed])
          (let [[package rest-packages] (parse-package-count-operator type (subs binary-package 7))]
            [(into [(with-meta package {:version version})] rest-packages)]))))))

(defn parse-package [hex-package]
  (ffirst (parse-package* (hex->binary hex-package))))

(declare version-sum)

(defn version-sum* [sum parsed-package]
  (if (seq? parsed-package)
    (reduce + (+ sum (:version (meta parsed-package))) (map version-sum (rest parsed-package)))
    0))

(defn version-sum [parse-package]
  (version-sum* 0 parse-package))

(defn solve-1 [input]
  (version-sum (parse-package input)))

(defn solve-2 [input]
  (eval (parse-package input)))

(defn run []
  (check 1 (solve-1 (get-input)))
  (check 2 (solve-2 (get-input))))
