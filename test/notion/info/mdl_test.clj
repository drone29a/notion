(ns notion.info.mdl-test
  (:require [clojure.test :refer :all]
            [notion.info.mdl :refer :all]
            [notion.core-test :refer [close? close-seq?]]))

(deftest huffman-encoded-length-test
  (is (= 103
         (huffman-encoded-length "how many bits does it take?"))))

(deftest huffman-encoded-mean-num-bits-test
  (is (= (/ 72 17)
         (huffman-encoded-mean-num-bits "how many bits on average?"))))

(deftest quantized-mdl-test
  (is (= (/ 21 4)
         (quantized-mdl 4 1 (constantly 3) [0.25 1.3 3.6 3.6 3.75 4.2 6.0]))))
