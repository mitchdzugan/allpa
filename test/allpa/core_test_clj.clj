(ns allpa.core-test-clj
  (:require [clojure.test :refer :all]
            [allpa.core :as a]))

(deftest core-api
  (testing "varg#"
    (let [f (a/varg# (conj %3 %1))]
      (is (= (f 1) '(1)))
      (is (= (f 1 :b [0]) [0 1]))
      (is (= (f 1 :b [0] :c :d :e) [0 1]))))
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
    (is (= (a/p2 5) 32)))
  (testing "id"
    (is (= (-> {} a/id) nil))
    (is (= (-> {} (a/set-id 1) a/id) 1)))
  (testing "parse-int"
    (is (= (a/parse-int "5") 5))
    (is (= (a/parse-int "5432") 5432))
    (is (= (a/parse-int "-7") -7))
    (is (= (a/parse-int "asdf") nil))
    (is (= (a/parse-int "asdf" "default") "default")))
  (testing "match"
    (let [a1 (a/mk :a {:val 1})
          b1 (a/mk :b {:val 1})
          a2 (a/mk :a {:val 2})
          b2 (a/mk :b {:val 2})
          f1 (a/match {:a #(-> % :val inc)
                       :b #(-> % :val dec)})
          f2 (a/match {:a [:val
                           {2 (a/varg# "Yay")
                            a/Default (a/varg# "Boo")}]
                       a/Default (a/varg# ":0")})]
      (is (= (f1 a1) 2))
      (is (= (f1 b1) 0))

      (is (= (f2 a1) "Boo"))
      (is (= (f2 b1) ":0"))
      (is (= (f2 a2) "Yay"))
      (is (= (f2 b2) ":0")))))
