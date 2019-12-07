(ns aoc2019.day04)


(def low 359282)
(def high 820401)


(def passwords (range low (inc high)))


(defn digits [n]
  (->> n
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))


(defn valid? [n]
  (let [digits (digits n)]
    (and (some #(> % 1) (vals (frequencies digits)))
         (every? #(apply <= %) (partition 2 1 digits)))))


(defn strict-valid? [n]
  (let [digits (digits n)]
    (and (some #(= % 2) (vals (frequencies digits)))
         (every? #(apply <= %) (partition 2 1 digits)))))


(defn puzzle1 [] (count (filter valid? passwords)))


(defn puzzle2 [] (count (filter strict-valid? passwords)))
