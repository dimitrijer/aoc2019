(ns aoc2019.day05
  (:require [clojure.math.numeric-tower :as math]))


(def mem (->> (slurp "resources/day05_puzzle1.txt")
              (re-seq #"-?\d+")
              (map #(Integer/parseInt %))
              vec))


(def opcode->size {1 4
                   2 4
                   3 2
                   4 2
                   99 1})


(def opcode->numops {1 2
                     2 2
                     3 0
                     4 1
                     99 0})


(defn parse-opcode [word] (mod word 100))


(defn read-instr [pc mem]
  (let [opcode (parse-opcode (nth mem pc))
        sz (opcode->size opcode)]
    (subvec mem pc (+ pc sz))))


(defn rev-digits
  "Returns infinite seq of reversed digits with trailing zeros: 321 => (1 2 3 0 0 ...)."
  [n]
  (lazy-cat 

    (->> n
         (iterate #(quot % 10))
         (take-while pos?)
         (mapv #(mod % 10)))

    (repeat 0)))


(defn fetch-ops [instr mem]
  (let [numops (-> instr
                   first
                   parse-opcode
                   opcode->numops)

        adrmodes (->> instr
                      first
                      rev-digits 
                      (drop 2)
                      (take numops))

        fetch-fn (fn [op-src adrmode]
                   (case adrmode
                     0 (nth mem op-src)
                     1 op-src))]

    (map fetch-fn (drop 1 instr) adrmodes)))


(defn execute! [mem opcode [arg1 arg2 :as args] dst]
  (do
    (println "EXEC " opcode " args " args " dst " dst)
    (case opcode
      1 (assoc mem dst (+ arg1 arg2))
      2 (assoc mem dst (* arg1 arg2))
      3 (assoc mem dst (Integer/parseInt (read-line)))
      4 (do (println arg1) mem))))


(defn run [pc mem]
  (let [instr (read-instr pc mem)
        opcode (parse-opcode (first instr))
        ops (fetch-ops instr mem)
        dst (last instr)]
    #_(doall (for [i (range (count mem))]
             (println "[MEM=" i "] = " (nth mem i))))
    (println "[PC=" pc "] instr = " instr)
    (if (= (parse-opcode (first instr)) 99)
      mem
      (recur (+ pc (opcode->size opcode))
             (execute! mem opcode ops dst)))))


(defn puzzle1 [] (run 0 mem))
