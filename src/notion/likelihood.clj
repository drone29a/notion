(ns notion.likelihood
  (:require [schema.core :as sc]
            [schema.macros :as sm]))

;; Compute log of sums
(comment (def ^:private log-sums (lazy-seq (reduce (fn [log-sums i]
                                                     (conj log-sums 
                                                           (+ (last log-sums) (Math/log i))))
                                                   [Double/NaN (Math/log 1)]
                                                   (range 2 Integer/MAX_VALUE)))))

;; TODO: Fix this so it is lazy and can support a larger range of values on demand.
(def ^:private log-sums (reduce (fn [log-sums i]
                                  (conj log-sums 
                                        (+ (last log-sums) (Math/log i))))
                                [Double/NaN (Math/log 1)]
                                (range 2 50000)))

(defn log-multinomial
  [thetas
   x]
  (let [m (count thetas)
        n (reduce + x)
        p (+ (- (nth log-sums n)
                (reduce + (map (comp log-sums int) x)))
             (reduce + (map (fn [x p]
                              (let [p (if (zero? p)
                                        0.000000001
                                        p)]
                                (* x (Math/log p))))
                            x
                            thetas)))]
    p))

(comment (defn log-categorical
           [thetas
            x]
           (let )))
