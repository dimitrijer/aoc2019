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
                   5 3
                   6 3
                   7 4
                   8 4
                   99 1})


(def opcode->numops {1 2
                     2 2
                     3 0
                     4 1
                     5 2
                     6 2
                     7 2
                     8 2
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


(defn execute! [pc mem opcode [arg1 arg2 :as args] dst]
  (case opcode

    ;; add
    1 [pc (assoc mem dst (+ arg1 arg2))]

    ;; mul
    2 [pc (assoc mem dst (* arg1 arg2))]

    ;; in
    3 [pc (assoc mem dst (Integer/parseInt (read-line)))]

    ;; out
    4 (do (println arg1) [pc mem])

    ;; jnz
    5 (if (not (zero? arg1))
        [arg2 mem]
        [pc mem])

    ;; jz
    6 (if (zero? arg1)
        [arg2 mem]
        [pc mem])

    ;; lt
    7 [pc (assoc mem dst (if (< arg1 arg2) 1 0))]

    ;; eq
    8 [pc (assoc mem dst (if (= arg1 arg2) 1 0))]))


(defn run [[pc mem]]
  (let [instr (read-instr pc mem)
        opcode (parse-opcode (first instr))
        ops (fetch-ops instr mem)
        dst (last instr)
        pc (+ pc (opcode->size opcode))]
    (if (= (parse-opcode (first instr)) 99)
      mem
      (recur (execute! pc mem opcode ops dst)))))


(defn puzzle1 [] (run [0 mem]))
