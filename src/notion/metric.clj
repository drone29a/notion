(ns notion.metric
  "Metrics and some non-metric distance functions."
  (:require [clojure.core.matrix :as mx]
            [clojure.core.typed :as t]
            [munge.matrix :as mmx]
            [notion.evaluation :refer [log]]))

(defn cos-sim
  "Compute the cosine similarity between two vectors."
  [u v]
  (/ (mx/dot u v)
     (* (mmx/l2-norm u) (mmx/l2-norm v))))

(defn kl-cat
  "KL divergence for categorical distribution.
  Measure how the parameter vector q differs from parameter vector p."
  [p q]
  (reduce + (map (fn [p-i q-i]
                   (cond (zero? p-i) 0
                         (zero? q-i) Double/POSITIVE_INFINITY
                         :else (* (Math/log (/ p-i q-i)) p-i)))
                 p q)))

(t/defn kl-error
  "An error function based on KL divergence. The derivation was not provided in the paper,
  and I do not have a justification for using KL divergence (which is only defined for
  probability distributions, not arbitrary collections of values) nor why the second
  and third terms are introduced.

  NB: the log is base 2 as we are attempting to measure the extra number of bits introduced
  by the error.
  
  References:
  K. Henderson, B. Gallagher, T. Eliassi-Rad, H. Tong, S. Basu, L. Akoglu, 
  D. Koutra, C. Faloutsos, and L. Li, RolX: structural role extraction & mining 
  in large graphs. New York, New York, USA: ACM, 2012, pp. 1231–1239."
  [obs-vals :- (t/Seq Num)
   est-vals :- (t/Seq Num)] :- t/Num
  (->> (map (fn [obs est]
              (-> (cond (zero? obs) 0
                        (zero? est) Double/POSITIVE_INFINITY
                        :else (* obs (log (/ obs est) 2)))
                  (- obs)
                  (+ est)))
            obs-vals est-vals)
       (reduce +)))
