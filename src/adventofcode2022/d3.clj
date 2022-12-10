(ns adventofcode2022.d3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (->> (io/resource "3.txt") slurp str/split-lines))

(defn- priority
  [c]
  (if (Character/isUpperCase c)
    (- (int c) 38)
    (- (int c) 96)))

(defn- find-duplicate
  [colls]
  (first (apply set/intersection (map set colls))))

(->> input
     (map #(split-at (/ (count %) 2) %))
     (map find-duplicate)
     (map priority)
     (reduce +))

;; => 7903

(->> input
     (partition 3)
     (map find-duplicate)
     (map priority)
     (reduce +))

;; => 2548
