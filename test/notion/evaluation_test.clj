(ns notion.evaluation-test
  (:require [clojure.test :refer :all]
            [notion.evaluation :refer :all]
            [notion.core-test :refer [close? close-seq?]]))

(deftest rmse-test
  (is (close? 1e-3 1.323 (rmse [1 2 3 -1] [2 3 4 -3]))))
