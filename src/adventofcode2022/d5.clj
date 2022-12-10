(ns adventofcode2022.d5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (->> (io/resource "5.txt") slurp str/split-lines))

(defn- crates
  [row]
  (->> row next (take-nth 4)))

(defn- parse-stacks
  [lines]
  (let [[stacks] (partition-by #{""} lines)]
    (->> stacks
         butlast
         reverse
         (map crates)
         (apply map vector)
         (mapv #(vec (take-while (fn [stack] (not= \space stack)) %))))))

(defn- parse-move
  [line]
  (->> (re-find #"move (\d+) from (\d+) to (\d+)" line)
       next
       (map parse-long)))

(defn- parse-moves
  [lines]
  (let [[_ _ moves] (partition-by #{""} lines)]
    (map parse-move moves)))

(defn move
  [stacks [n from to]]
  (if (zero? n)
    stacks
    (let [crate (peek (stacks (dec from)))
          new-stacks (-> stacks
                         (update (dec from) pop)
                         (update (dec to) #(conj % crate)))]
      (move new-stacks [(dec n) from to]))))


(let [moves (parse-moves input)
      stacks (parse-stacks input)]
  (->> (reduce move stacks moves)
       (map last)
       (apply str)))

;; => "VJSFHWGFT"

(defn move-9001
  [stacks [n from to]]
  (let [crates (take-last n (stacks (dec from)))]
    (-> stacks
        (update (dec from) #(drop-last n %))
        (update (dec to) #(concat % crates)))))

(let [moves (parse-moves input)
      stacks (parse-stacks input)]
  (->> (reduce move-9001 stacks moves)
       (map last)
       (apply str)))

;; => "LCTQFBVZV"
