(ns aoc2019.day02)


(def mem (->> (slurp "resources/day02_puzzle1.txt")
              (re-seq #"\d+")
              (map #(Integer/parseInt %))
              vec))


(def opcode->fn {1 +
                 2 *})


(defn execute
  [mem pc]
  (let [[opcode op1-src op2-src result-dst] (->> mem (drop pc) (take 4))]
    (if (= opcode 99)
      mem

      (let [op1 (nth mem op1-src)
            op2 (nth mem op2-src)
            result ((opcode->fn opcode) op1 op2)]

        (recur (assoc mem result-dst result) (+ pc 4))))))


(defn puzzle1
  []
  (first
    (execute (-> mem (assoc 1 12) (assoc 2 2)) 0)))


(defn puzzle2
  []
  (first (for [verb (range 100)
               noun (range 100)
               :let [next-mem (execute (-> mem (assoc 1 noun) (assoc 2 verb)) 0)
                     result (first next-mem)]
               :when (= result 19690720)]
           (+ (* 100 noun)
              verb) )))
