(ns notion.metric
  (:require [clojure.core.matrix :as mx]))

(defn kl-cat
  "KL divergence for categorical distribution.
  Measure how the parameter vector q differs from parameter vector p."
  [p q]
  (reduce + (map (fn [p-i q-i]
                   (cond (zero? p-i) 0
                         (zero? q-i) Double/POSITIVE_INFINITY
                         :else (* (Math/log (/ p-i q-i)) p-i)))
                 p q)))
