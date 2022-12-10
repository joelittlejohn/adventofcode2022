(ns adventofcode2022.d2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (->> (io/resource "2.txt") slurp str/split-lines (map (fn [[o _ p]] [o p]))))

(def score-shape
  {\X 1 \Y 2 \Z 3})

(def score-outcome
  {[\A \X] 3
   [\A \Y] 6
   [\A \Z] 0
   [\B \X] 0
   [\B \Y] 3
   [\B \Z] 6
   [\C \X] 6
   [\C \Y] 0
   [\C \Z] 3})

(defn score
  [[o p]]
  (+ (score-shape p)
     (score-outcome [o p])))

(score [\A \Y])
(score [\B \X])
(score [\C \Z])

(->> input
     (map score)
     (reduce +))

;; => 9651

(def win
  {\A \Y \B \Z \C \X})

(def lose
  {\A \Z \B \X \C \Y})

(def draw
  {\A \X \B \Y \C \Z})

(def outcome
  {\X lose
   \Y draw
   \Z win})

(defn outcome->shape
  [[o p]]
  [o ((outcome p) o)])

(score (outcome->shape [\A \Y]))
(score (outcome->shape [\B \X]))
(score (outcome->shape [\C \Z]))

(->> input
     (map outcome->shape)
     (map score)
     (reduce +))

;; => 10560
