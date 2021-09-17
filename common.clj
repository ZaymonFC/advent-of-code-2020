(ns common
  (:require [clojure.string :refer [split-lines]]))

;; --- Utilities
(defn outer-join [xs]
  (filter (complement nil?)
          (let [xs (keep-indexed vector xs)]
            (for [[idx x] xs
                  [idy y] xs]
              (when (not= idx idy) [x y])))))

(defn take-3 [xs]
  (filter (complement nil?)
          (let [xs (keep-indexed vector xs)]
            (for [[idx x] xs
                  [idy y] xs
                  [idz z] xs]
              (when (not= idx idy idz) [x y z])))))


(defn parse-number
  "Reads a number from a string. Returns nil if not a number."
  [s]
  (if (and ((complement empty?) s) (re-find #"^-?\d+\.?\d*$" s))
    (read-string s)))

(defn lines [file-name]
  (->> (slurp (str "inputs/" file-name))
       (split-lines)))

(defn numbers [file-name]
  (->> (lines file-name)
       (map parse-number)))

(defn char->upper-case [x] (Character/toUpperCase x))
(defn string->upper-case [x] (clojure.string/upper-case x))
(defn isDigit [x] (Character/isDigit x))
