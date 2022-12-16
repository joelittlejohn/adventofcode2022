(ns adventofcode2022.d9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- parse
  [s]
  (let [[_ d n] (re-find #"^(.) (\d+)" s)]
    [d (parse-long n)]))

(def input
  (->> (io/resource "9.txt") slurp str/split-lines (map parse)))

(defn- add
  [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])

(defn- subtract
  [[ax ay] [bx by]]
  [(- ax bx) (- ay by)])

(def movement-vectors
  {"L" [-1 0] "R" [1 0] "U" [0 1] "D" [0 -1]})

(defn- move
  [states [d n :as _move]]
  (if (zero? n)
    states
    (let [{:keys [head tail]} (last states)
          head-move-vec (movement-vectors d)
          new-head (add head head-move-vec)
          [diffx diffy] (subtract new-head tail)
          new-tail (if (or (> (abs diffx) 1) (> (abs diffy) 1))
                     head
                     tail)]
      (move (conj states {:head new-head :tail new-tail}) [d (dec n)]))))

(->> (reduce move [{:head [0 0] :tail [0 0]}] input)
     (map :tail)
     set
     count)

;; => 5960
