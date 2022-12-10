(ns adventofcode2022.d1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (->> (io/resource "1.txt") slurp str/split-lines (partition-by empty?) (remove #{[""]}) (map (fn [xs] (map #(parse-long %) xs)))))


(->> input
     (map #(reduce + %))
     (apply max))

;; => 69206


(->> input
     (map #(reduce + %))
     sort
     (take-last 3)
     (reduce +))

;; => 197400
