(ns notion.info-test
  (:require [clojure.test :refer :all]
            [notion.info :refer :all]
            [notion.core-test :refer [close? close-seq?]]))



(deftest quantize-test
  (let [table [1.0 2.0 3.0 4.0]
        codes [0.5 1.5 2.5 3.5 4.5]]
    (are [actual found] (= actual found)
      0.5 (quantize table codes 0)
      0.5 (quantize table codes 1.0)
      1.5 (quantize table codes 1.01)
      2.5 (quantize table codes 2.2)
      2.5 (quantize table codes 3.0)
      3.5 (quantize table codes 4.0)
      4.5 (quantize table codes 4.01))))


(deftest lloyd-max-test
  (let [{:keys [table codes]} (lloyd-max [0.5 1.2 1.2 1.5 1.75 3.0 3.0 5.0] (Math/pow 2 2))]
    (is (close-seq? 1e-5
                    [1.3
                     1.75
                     3.0]
                    table))
    (is (close-seq? 1e-5
                    [0.96667
                     1.625
                     3.0
                     5.0]
                    codes))))
