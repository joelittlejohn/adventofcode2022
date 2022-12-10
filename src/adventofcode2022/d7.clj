(ns adventofcode2022.d7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [instaparse.core :as insta]
            [clojure.walk :as walk]))

(def ^:private log-parser
  (insta/parser "log-line = command | directory | file
                 directory = 'dir ' directory-name
                 directory-name = #'[a-z]+'
                 file = file-size ' ' file-name
                 file-size = #'\\d+'
                 file-name = #'[a-z.]+'
                 command = '$ ' (list | cd)
                 list = 'ls'
                 cd = 'cd ' target
                 target = '/' | '..' | #'[a-z]+'"))

(def input
  (->> (io/resource "7.txt") slurp str/split-lines (map log-parser)))

(defmulti apply-log-line (fn [_state c] (first c)))

(defmethod apply-log-line :command
  [state [_ _ c]]
  (apply-log-line state c))

(defmethod apply-log-line :cd
  [state [_ _ [_ target]]]
  (case target
    ".." (update state :current-dir pop)
    "/" (assoc state :current-dir ["/"])
    (update state :current-dir #(conj % target))))

(defmethod apply-log-line :list
  [state _]
  state)

(defmethod apply-log-line :directory
  [state _]
  state)

(defmethod apply-log-line :file
  [{:keys [current-dir] :as state} [_ [_ file-size] _ [_ file-name]]]
  (update-in state (cons :tree current-dir) #(assoc % file-name (parse-long file-size))))

(def input
  (->> "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"
       str/split-lines (map log-parser)))

(defn- calulate-directory-sizes
  [tree]
  (->> tree
       (walk/postwalk
        #(if (map? %)
           (let [files-size (->> (vals %)
                                 (filter number?)
                                 (reduce + ))
                 dirs-size (->> (vals %)
                                (filter map?)
                                (map :size)
                                (reduce +))]
             (assoc % :size (+ files-size dirs-size)))
           %))))

(->> input
     (map second)
     (reduce apply-log-line {:tree {} :current-dir []})
     :tree
     calulate-directory-sizes)
