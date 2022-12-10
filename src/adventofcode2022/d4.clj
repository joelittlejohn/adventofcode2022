(ns adventofcode2022.d4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- parse
  [s]
  (->> (re-find #"(\d+)-(\d+),(\d+)-(\d+)" s)
       rest
       (map parse-long)
       (split-at 2)))

(def input
  (->> (io/resource "4.txt") slurp str/split-lines (map parse)))

(defn fully-contains?
  [[[a b] [x y]]]
  (or (and (<= a x) (>= b y))
      (and (<= x a) (>= y b))))

(->> input
     (filter fully-contains?)
     count)

;; => 602

(defn disjoint?
  [[[a b] [x y]]]
  (or (and (< a x) (< b x))
      (and (> a y) (> b y))))

(->> input
     (remove disjoint?)
     count)
