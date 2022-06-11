(ns core-test
  (:require [core :as sut]
            [clojure.test :as t]))

(t/deftest test-arity
  (t/is (= 2
           (sut/arity (fn [x y] (+ x y))))))
