(ns adventofcode2022.d9
  (:require [clojure.java.io :as io]
            [clojure.math :refer [signum]]
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

(defn- new-tail
  [new-head old-tail]
  (let [[headx heady] new-head
        [old-tailx old-taily] old-tail
        [diffx diffy] (subtract new-head old-tail)]
    (cond
      (and (> (abs diffx) 1) (> (abs diffy) 1)) [(+ old-tailx (int (signum diffx))) (+ old-taily (int (signum diffy)))]
      (> (abs diffx) 1) [(+ old-tailx (int (signum diffx))) heady]
      (> (abs diffy) 1) [headx (+ old-taily (int (signum diffy)))]
      :else old-tail)))

(defn- move
  [states [d n :as _move]]
  (if (zero? n)
    states
    (let [rope (last states)
          head-move-vec (movement-vectors d)
          new-head (add (first rope) head-move-vec)
          new-rope (reduce (fn [new-rope old-tail]
                             (let [new-head (last new-rope)
                                   new-tail (new-tail new-head old-tail)]
                               (conj new-rope new-tail)))
                           [new-head]
                           (next rope))]
      (move (conj states new-rope) [d (dec n)]))))

(->> (reduce move [(repeat 2 [0 0])] input)
     (map last)
     set
     count)

;; => 5960

(->> (reduce move [(repeat 10 [0 0])] input)
     (map last)
     set
     count)

;; => 2327
