(ns compose-test
  (:require [compose :as sut]
            [clojure.test :as t]))

(t/deftest test-compose
  (t/is (= '(foo (bar z))
           ((sut/compose
             (fn [x] (list 'foo x))
             (fn [y] (list 'bar y))
             ) 'z))))
