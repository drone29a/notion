(ns notion.stat
  "Stat functions taken from: https://gist.github.com/scottdw/2960070"
  (:require [clojure.core.typed :as t]
            [clojure.core.matrix :as mx]))

(defn mode [vs]
  (let [fs (frequencies vs)]
    (first (last (sort-by second fs)))))

;; TODO: rename this, is actually percentile
(defn quantile
  ([p vs]
     (let [svs (sort vs)]
       (quantile p (count vs) svs (first svs) (last svs))))
  ([p c svs mn mx]
     (let [pic (* p (inc c))
           k (int pic)
           d (- pic k)
           ndk (if (zero? k) mn (nth svs (dec k)))]
       (cond
        (zero? k) mn
        (= c (dec k)) mx
        (= c k) mx
        :else (+ ndk (* d (- (nth svs k) ndk)))))))

(defn median
  ([vs] (quantile 0.5 vs))
  ([sz svs mn mx] (quantile 0.5 sz svs mn mx)))

(defn mean
  ([vs] (mean (reduce + vs) (count vs)))
  ([sm sz] (float (/ sm sz))))

(defn standard-deviation
  ([vs]
     (standard-deviation vs (count vs) (mean vs)))
  ([vs sz u]
     (Math/sqrt (/ (reduce + (map #(Math/pow (- % u) 2) vs))
                   sz))))

(defn median-absolute-deviation
  ([vs]
     (median-absolute-deviation vs (median vs)))
  ([vs m]
     (median (map #(Math/abs (- % m)) vs))))

(defn lower-adjacent-value
  ([vs]
     (let [q1 (quantile 0.25 vs)
           m (median vs)
           q3 (quantile 0.75 vs)]
       (lower-adjacent-value (sort vs) m (- q3 q1))))
  ([svs m qd]
     (let [l (- m qd)]
       (first (filter (partial < l) svs)))))

(defn upper-adjacent-value
  ([vs]
     (let [q1 (quantile 0.25 vs)
           m (median vs)
           q3 (quantile 0.75 vs)]
       (upper-adjacent-value (reverse (sort vs)) m (- q3 q1))))
  ([rsvs m qd]
     (let [l (+ m qd)]
       (first (filter #(< % l) rsvs)))))

(defn summary
  ([vs]
     (let [sz (count vs)
           svs (sort vs)
           rsvs (reverse svs)
           mn (first svs)
           mx (first rsvs)
           sm (reduce + vs)
           u (mean sm sz)
           mdn (median sz svs mn mx)
           q1 (quantile 0.25 sz svs mn mx)
           q3 (quantile 0.75 sz svs mn mx)
           sd (standard-deviation vs sz u)
           mad (median-absolute-deviation vs mdn)
           qd (- q3 q1)
           lav (lower-adjacent-value svs mdn qd)
           uav (upper-adjacent-value rsvs mdn qd)]
       [[:size sz]
        [:min mn]
        [:max mx]
        [:mean u]
        [:median mdn]
        [:mode (mode vs)]
        [:q1 q1]
        [:q3 q3]
        [:total sm]
        [:sd sd]
        [:mad mad]
        [:lav lav]
        [:uav uav]]))
  ([stat-ks vs]
   (partition 2 (interleave stat-ks
                            (map (into {} (summary vs)) stat-ks)))))

(defn closest-mean-fn [means]
  (fn [v] (reduce (partial min-key #(Math/pow (- v %) 2)) means)))

(defn k-means [k vs]
  (let [vs (map double vs)
        svs (set vs)]
    (if (> k (count svs))
      (sort svs)
      (loop [mns (sort (take k (shuffle svs)))
             pmns (repeat k Double/NaN)]
        (if (= mns pmns)
          mns
          (recur (sort (map mean (vals (group-by (closest-mean-fn mns) vs)))) mns))))))

(t/defn correlation
  []
  nil)
