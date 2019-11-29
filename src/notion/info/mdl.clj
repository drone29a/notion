(ns notion.info.mdl
  "Functions for computing minimum description length."
  (:require [clojure.core.typed :as t]
            [notion.info :refer [quantize lloyd-max]]
            [notion.info.huffman :refer [huffman-encode]]
            [notion.stat :refer [mean]]))

(t/defn huffman-encoded-length
  "Returns the number of bits required to represent a sequence
  of values."
  [xs :- (t/Seq t/Num)] :- t/Int
  (let [num-xs (count xs)
        huff-code (huffman-encode xs)]
    (->> huff-code
         (map (fn [{:keys [weight code]}]
                (* (* num-xs weight)
                   (count code))))
         (reduce +))))

(t/defn huffman-encoded-mean-num-bits
  "Calculate the mean number of bits needed to represent
  an item from the collection."
  [xs :- (t/Seq t/Num)] :- t/Num
  (->> (huffman-encode xs)
       (map (comp count :code))
       mean))

(t/defn quantized-mdl
  [num-quanta :- t/Int
   num-model-vals :- t/Int
   error-length :- (t/Fn [(t/Seq t/Num) -> t/Num])
   xs :- (t/Vec t/Num)
   & {:keys [verbose] :or {verbose false}}] :- t/Num
  (let [{:keys [table codes]} (lloyd-max xs num-quanta)
        q-xs (map (partial quantize table codes) xs)
        ml (* (huffman-encoded-mean-num-bits q-xs) num-model-vals)
        el (error-length q-xs)]
    (if verbose
      {:mdl (+ ml el)
       :model-length ml
       :error-length el}
      (+ ml el))))
