(ns aoc2019.day01
  (:require [clojure.string :as str]))


(def modules-mass (->> (slurp "resources/day01_puzzle1.txt")
                       str/split-lines
                       (map #(Integer/parseInt %))))


(defn calculate-fuel [mass]
  (- (int (/ mass 3)) 2))


(defn puzzle1 []
  (reduce + (map calculate-fuel modules-mass)))


(defn calculate-additional-fuel [fuel-mass]
  (let [recur-fuel (fn [total-fuel leftover-fuel]
                     (let [fuel (calculate-fuel leftover-fuel)]
                       (if (<= fuel 0)
                         total-fuel
                         (recur (+ total-fuel fuel) fuel))))]
    (recur-fuel fuel-mass fuel-mass))) 


(defn puzzle2 []
  (reduce +
          (map #(calculate-additional-fuel (calculate-fuel %)) modules-mass)))
