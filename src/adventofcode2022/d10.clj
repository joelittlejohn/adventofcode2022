(ns adventofcode2022.d10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- noop
  [cycles]
  (conj cycles (last cycles)))

(defn- addx
  [x cycles]
  (-> cycles
      noop
      (conj (+ x (last cycles)))))

(defn- parse
  [s]
  (let [[_ arg] (str/split s #" ")]
    (if arg
      (partial addx (parse-long arg))
      noop)))

(def input
  (->> (io/resource "d10.txt") slurp str/split-lines (map parse)))

(defn execute
  [cmds]
  (reduce (fn [cycles cmd] (cmd cycles)) [1] cmds))

(let [cycles (execute input)]
  (->> [20 60 100 140 180 220]
       (map #(* % (nth cycles (dec %))))
       (reduce +)))

;; => 16020

(let [cycles (execute input)]
  (doseq [c (range (count cycles))
          :let [x (nth cycles c)
                pos (mod c 40)]]
    (when (zero? pos)
      (println))
    (if (<= (dec pos) x (inc pos))
      (print \█)
      (print \space))))

;; ████  ██  ████ █  █ ████  ██  █    ███
;; █    █  █    █ █  █    █ █  █ █    █  █
;; ███  █      █  █  █   █  █  █ █    █  █
;; █    █     █   █  █  █   ████ █    ███
;; █    █  █ █    █  █ █    █  █ █    █ █
;; ████  ██  ████  ██  ████ █  █ ████ █  █
