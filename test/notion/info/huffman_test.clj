(ns notion.info.huffman-test
  (:require [clojure.test :refer :all]
            [notion.info.huffman :refer :all]
            [notion.core-test :refer [close? close-seq?]]))

(deftest huffman-encode-test
  (is (= (sort-by :symbol [{:symbol \t, :weight 2/43, :code "0000"}
                           {:symbol \l, :weight 2/43, :code "0001"}
                           {:symbol \c, :weight 1/43, :code "00100"}
                           {:symbol \k, :weight 1/43, :code "00101"}
                           {:symbol \m, :weight 2/43, :code "0011"}
                           {:symbol \o, :weight 4/43, :code "010"}
                           {:symbol \space, :weight 5/43, :code "011"}
                           {:symbol \e, :weight 3/43, :code "1000"}
                           {:symbol \f, :weight 3/43, :code "1001"}
                           {:symbol \n, :weight 3/43, :code "1010"}
                           {:symbol \p, :weight 1/43, :code "10110"}
                           {:symbol \u, :weight 1/43, :code "101110"}
                           {:symbol \i, :weight 1/43, :code "101111"}
                           {:symbol \s, :weight 7/43, :code "110"}
                           {:symbol \h, :weight 3/43, :code "1110"}
                           {:symbol \a, :weight 2/43, :code "11110"}
                           {:symbol \r, :weight 2/43, :code "11111"}])
         (sort-by :symbol (huffman-encode "thanks huffman for the lossless compression")))))
