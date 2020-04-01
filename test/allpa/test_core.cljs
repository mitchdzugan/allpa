(ns allpa.test-core
  (:require [allpa.core :as a]
            [cljs.test :refer-macros [is are deftest testing use-fixtures]]))

(deftest core-api
  (testing "power"
    (is (= (a/power 2 3) 8))
    (is (= (a/power 2 0) 1))
    (is (= (a/power 0 0) 1))
    (is (= (a/power 0 -1) 0))
    (is (= (a/power 2 -1) 0))
    (is (= (a/power -2 0) 1))
    (is (= (a/power -2 -1) 0))
    (is (= (a/power -2 2) 4))
    (is (= (a/power -2 3) -8)))
  (testing "p2"
    (is (= (a/p2 -2) 0))
    (is (= (a/p2 0) 1))
    (is (= (a/p2 2) 4))
    (is (= (a/p2 5) 32))))
