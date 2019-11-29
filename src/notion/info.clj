(ns notion.info
  "Information theory, compression, and quantization."
  (:require [clojure.core.typed :as t]
            [notion.stat :as stat]))

(t/defalias Table (t/Vec t/Num))
(t/defalias Code t/Num)

;; TODO: consider moving this somewhere for reuse
(t/defn mean-squared-error
  [xs :- (t/Seq Num)
   ys :- (t/Seq Num)] :- t/Num
  {:pre [(= (count xs) (count ys))]}
  (let [num-vals (count xs)]
    (/ (->> (map (comp #(Math/pow % 2) -) xs ys)
            (reduce +))
       num-vals)))

(t/defn quantize-index
  "Return the code index for the given x."
  [table :- Table
   x :- t/Num] :- t/Num
  (let [idx (java.util.Collections/binarySearch table x compare)]
    ;; The result from the binary search will be either a positive number which
    ;; is the index of the value in the collection or a negative number which is
    ;; the index of the largest value it is less than encoded as (- (val - 1)).
    (if (neg? idx)
      (- (inc idx))
      idx)))

(t/defn quantize
  "Return the quantized value for the given x."
  [table :- Table
   codes :- (t/Vec Code)
   x :- t/Num] :- t/Num
  (get codes (quantize-index table x)))

(t/defn lloyd-max
  "Find the optimal table and codes for the given scalar values 
  using the Lloyd-Max algorithm.

  Based off the Octave implementation documented here:
  http://octave.sourceforge.net/communications/function/lloyds.html

  NB: The Octave implementation includes the option to compute the endpoints 
  (table values) as either the center of the surrounding quanta (codes) or as
  centroids of the signatures. The Matlab implementation uses centroids, the 
  Octave implementation uses centroids by default, and thus we will simply
  use centroids here."
  [xs :- (t/Vec t/Num)
   num-codes :- t/Int] :- (t/HMap :mandatory {:table Table
                                              :codes (t/Coll Code)})
  {:post [(= (-> % :table count)
             (dec (-> % :codes count)))]}
  (let [min-x (apply min xs)
        max-x (apply max xs)
        init-codes (vec (map (fn [c]
                               (-> c
                                   (/ (dec num-codes))
                                   (* (- max-x min-x))
                                   (+ min-x)))
                             (range 0 num-codes)))
        init-table (vec (map (comp #(/ % 2) +)
                             (rest init-codes)
                             (drop-last init-codes)))
        tol 1e-7
        eps 2.2204e-16
        stop-val (max (* eps (Math/abs max-x))
                      (Math/abs tol))]
    (loop [q-idxs (map (partial quantize-index init-table) xs)
           dist (mean-squared-error xs (map (partial get init-codes) q-idxs))
           rel-dist dist
           codes init-codes
           table init-table]
      (if (<= rel-dist stop-val)
        {:table table
         :codes codes}
        (let [ints-idxs (->> q-idxs
                             (map-indexed vector)
                            (group-by second)
                            (map (fn [[q-idx vs]]
                                   [q-idx (map first vs)]))
                            (into {}))
              codes* (vec (map (fn [code-idx]
                                 (let [int-vals (map (partial get xs)
                                                     (get ints-idxs code-idx))]
                                   (cond
                                     (not (empty? int-vals)) (stat/mean int-vals)
                                     (= 0 code-idx) (stat/mean [(get table 0) min-x])
                                     (= (dec num-codes) code-idx) (stat/mean [max-x (get table (dec code-idx))])
                                     :else (stat/mean [(get table (dec code-idx)) (get table code-idx)]))))
                               (range num-codes)))
              table* (vec (map (fn [lower-code-val upper-code-val]
                                 (let [int-vals (filter #(and (>= % lower-code-val)
                                                              (< % upper-code-val))
                                                        xs)]
                                   (if (empty? int-vals)
                                     (stat/mean [upper-code-val lower-code-val])
                                     (stat/mean int-vals))))
                               (drop-last codes*)
                               (rest codes*)))
              q-idxs* (map (partial quantize-index table*) xs)
              dist* (mean-squared-error xs (map (partial get codes*) q-idxs*))
              rel-dist* (Math/abs (- rel-dist dist))]
          (recur q-idxs*
                 dist*
                 rel-dist* 
                 codes* 
                 table*))))))

