(ns aoc2019.day03
  (:require [clojure.string :as str]))


(def paths 

  #_["R8,U5,L5,D3"
     "U7,R6,D4,L4"]

  #_["R75,D30,R83,U83,L12,D49,R71,U7,L72"
     "U62,R66,U55,R34,D71,R55,D58,R83"]

  #_["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
     "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]

  (str/split-lines (slurp "resources/day03_puzzle1.txt")))


;; Parsing


(def directions->dir {\R :h \L :h
                      \U :v \D :v})


(defn parse-segments
  [segments [direction & length]]
  (let [length (Integer/parseInt (apply str length))

        [x y :as start] (or (:end (last segments))
                            [0 0])

        end (case direction
              \R [(+ x length) y]
              \L [(- x length) y]
              \U [x (+ y length)]
              \D [x (- y length)])

        next-segment {:dir (directions->dir direction)
                      :start start
                      :end end}]

    (conj segments next-segment)))


(defn path->wire [path] (reduce parse-segments [] (str/split path #",")))


;; Intersections


(defn varinvar
  [segment]
  (let [invariant (case (:dir segment)
                    :h (second (:start segment))
                    :v (first (:start segment)))

        variant (case (:dir segment)
                  :h [(first (:start segment))
                      (first (:end segment))]
                  :v [(second (:start segment))
                      (second (:end segment))])]

    [invariant (sort variant)]))



(defn orthogonal?
  [lhs-segment rhs-segment]
  (or (and (= (:dir lhs-segment) :v) (= (:dir rhs-segment) :h))
      (and (= (:dir lhs-segment) :h) (= (:dir rhs-segment) :v))))


(defn intersect?
  [lhs-segment rhs-segment]
  (let [[lhs-invariant lhs-variant] (varinvar lhs-segment)
        [rhs-invariant rhs-variant] (varinvar rhs-segment)]
    (and (orthogonal? lhs-segment rhs-segment)
         (> lhs-invariant (first rhs-variant))
         (< lhs-invariant (second rhs-variant))
         (> rhs-invariant (first lhs-variant))
         (< rhs-invariant (second lhs-variant)))))


(defn get-intersections
  [red-wire blue-wire]
  (for [red-segment red-wire
        blue-segment blue-wire
        :when (intersect? red-segment blue-segment)]
    (if (= :v (:dir red-segment))
      [(first (varinvar red-segment)) (first (varinvar blue-segment))]
      [(first (varinvar blue-segment)) (first (varinvar red-segment))])))


(defn manhattan [[x y]] (+ (Math/abs x) (Math/abs y)))


(defn puzzle1
  []
  (let [[red-wire blue-wire] (map path->wire paths)]
    (apply min (map manhattan (get-intersections red-wire blue-wire)))))
