(ns adventofcode2022.d8
  (:require [clojure.java.io :as io]
            [medley.core :refer [take-upto]]
            [clojure.string :as str]
            [clojure.test :refer [is]]))

(defn- parse
  [s]
  (->> s str/split-lines (map (fn [line] (map #(- (int %) 48) line))) (mapv vec)))

(defn- height
  [board x y]
  (get-in board [y x]))

(defn- visible?
  [board x y]
  (let [h (height board x y)
        lower? (fn [[xv yv]] (< (height board xv yv) h))]
    (or
     (= x 0)
     (= y 0)
     (= x (dec (count (first board))))
     (= y (dec (count board)))
     (every? lower? (for [xv (range x)] [xv y]))
     (every? lower? (for [xv (range (inc x) (count (first board)))] [xv y]))
     (every? lower? (for [yv (range y)] [x yv]))
     (every? lower? (for [yv (range (inc y) (count board))] [x yv])))))

(def test-input
  (parse "30373
25512
65332
33549
35390"))

(is (visible? test-input 1 1))
(is (visible? test-input 2 1))
(is (not (visible? test-input 3 1)))
(is (visible? test-input 1 2))
(is (not (visible? test-input 2 2)))
(is (visible? test-input 3 2))
(is (visible? test-input 2 3))
(is (not (visible? test-input 1 3)))
(is (not (visible? test-input 3 3)))

(def input
  (->> (io/resource "8.txt") slurp parse))

(let [cells (for [x (range (count (first input)))
                  y (range (count input))]
              [x y])]
  (->> cells
       (filter (fn [[x y]] (visible? input x y)))
       count))

;; => 1669

(defn scenic-score
  [board x y]
  (let [h (height board x y)
        not-lower? (fn [[xv yv]] (>= (height board xv yv) h))]
   (*
    (count (take-upto not-lower? (for [xv (reverse (range x))] [xv y])))
    (count (take-upto not-lower? (for [xv (range (inc x) (count (first board)))] [xv y])))
    (count (take-upto not-lower? (for [yv (reverse (range y))] [x yv])))
    (count (take-upto not-lower? (for [yv (range (inc y) (count board))] [x yv]))))))

(->> (for [x (range (count (first input)))
           y (range (count input))]
       (scenic-score input x y))
     sort
     last)

;; => 331344
