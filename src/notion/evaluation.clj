(ns notion.evaluation
  "Methods for evaluating clustering performance."
  (:require [clojure.math.combinatorics :as combo])
  (:use [clojure.set :only [difference intersection union]]))

(defn max-index [x]
  (.indexOf (vec x) (apply max x)))

(defn top-indices [n x]
  (map first (take n (reverse (sort-by second (map-indexed vector x))))))

(defn balanced-error-rate [x y]
  (let [x (set x)
        y (set y)]
    #_(printf "x: %s, y: %s\n" x y)
    (if (or (zero? (count x))
            (zero? (count y)))
      0
      (* 0.5
         (+
          (/ (count (difference x y))
             (count x))
          (/ (count (difference y x))
             (count y)))))))

(defn ber-score [x y] 
  (- 1 (balanced-error-rate x y)))

(defn subset-error-rate [x y]
  (let [x (set x)
        y (set y)]
    (/ (count (intersection x y))
       (count x))))

(defn best-bijection [sim xs ys]
  "Find the mapping of xs to ys by maximizing the similarity function."
  ;;; Note, |xs| is assumed to be larger than |ys|.  If |ys| is larger than not all of the 
  ;;; items in ys will be considered in the scoring.
  (let [xs-perms (combo/permutations xs)
        all-mappings (map vector 
                          (map (partial map sim ys) xs-perms)
                          xs-perms
                          (repeat ys))
        max-idx (max-index (map (comp (partial reduce +) first) all-mappings))]
    (nth all-mappings max-idx)))

(defn best-surjection 
  ([sim xs ys]
     (let [scores (map #(map (partial sim %) xs) ys)]
       (map (juxt (partial apply max) max-index) scores)))
  ([n sim xs ys]
     (let [scores (map #(map (partial sim %) xs) ys)]
       (map (juxt (comp (partial take n) reverse sort) (partial top-indices n)) scores))))

(defn log [x b]
  (/ (Math/log x) (Math/log b)))

(defn entropy [freqs]
  "Takes a vector of category counts and computes entropy."
  (let [sum-freqs (reduce + freqs)
        b (count freqs)]
    (cond 
     (zero? sum-freqs) 0
     (= 1 b) 0
     :else (reduce + (map (fn [freq] 
                       (let [prop (/ freq sum-freqs)]
                         (if (zero? prop)
                           0
                           (- (* prop (log prop b))))))
                     freqs)))))

(defn vec-sum [xs]
  (reduce (fn [acc x]
            (map + acc x))
          xs))

;; TODO: Should we really consider any non-zero to be a binary 1?
(defn entropy-scores [xs]
  "Take a sequence of objects, each object is a vector of feature counts.
  If the object features aren't binary, they are converted to binary features.

  I.e., we are finding the features which the objects seem to have in common.
  Those features should have lower entropy."
  (let [num-objs (count xs)
        xs-bin (for [x xs] (map (fn [val] (if (zero? val) 0 1)) x))]
    (map (comp entropy (fn [x] [x (- num-objs x)]))
         (vec-sum xs-bin))))

(defn jaccard [a b]
  (let [a (set a)
        b (set b)]
    (/ (float (count (intersection a b))) 
       (count (union a b)))))
