(ns allpa.test-core
  (:require
   #?(:clj [clojure.test :refer :all]
      :cljs [cljs.test :refer-macros [is are deftest testing use-fixtures]])
   [wayra.core :as w]
   [allpa.core :as a]
   [allpa.linked-hash-map :as lhm]))

(deftest core-api
  (testing "varg#"
    (let [f (a/varg# (conj %3 %1))]
      (is (= (f 1) '(1)))
      (is (= (f 1 :b [0]) [0 1]))
      (is (= (f 1 :b [0] :c :d :e) [0 1]))))
  (testing "alpha-num"
    (is (= (a/alpha-num 0) "A"))
    (is (= (a/alpha-num 25) "Z"))
    (is (= (a/alpha-num 26) "AA"))
    (is (= (a/alpha-num 701) "ZZ"))
    (is (= (a/alpha-num 702) "AAA")))
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
                       a/Default (a/varg# ":0")})
          f3 (a/match
              {a/Default [(a/varg# (+ 3 (:val %1)))
                          {a/Default [(a/varg# (* 5 (:val %1)))
                                      {a/Default (a/varg# [%1 %2 %3 %4])}]}]})
          f4 (a/match
              {:a [:val {a/Default (a/varg# %2)}]
               a/Default [(a/varg# %1)
                          {a/Default [(a/varg# :xD)
                                      {a/Default [:val
                                                  {a/Default (a/varg# %4)}]}]}]})]
      (is (= (f1 a1) 2))
      (is (= (f1 b1) 0))

      (is (= (f2 a1) "Boo"))
      (is (= (f2 b1) ":0"))
      (is (= (f2 a2) "Yay"))
      (is (= (f2 b2) ":0"))

      (is (= (f3 b2) [b2 5 10 nil]))

      (is (= (f4 a2) 2))
      (is (= (f4 b2) 2)))))

(deftest linked-hash-map
  (testing "append"
    (is (= (-> (lhm/exec (w/mdo (lhm/appendm 1)
                                (lhm/appendm 2)
                                (lhm/appendm 3)))
               lhm/to-vector)
           [1 2 3]))
    (is (= (-> (lhm/exec (w/mdo (lhm/appendm {:v "a"} a/set-id)
                                (lhm/appendm {:v "b"} a/set-id)
                                (lhm/appendm {:v "c"} a/set-id)))
               lhm/to-vector)
           [{:v "a" a/id 1}
            {:v "b" a/id 2}
            {:v "c" a/id 3}])))
  (testing "insert"
    (is (= (-> (lhm/exec (w/mdo id-1 <- (lhm/appendm 1)
                                (lhm/appendm 2)
                                id-4 <- (lhm/appendm 4)
                                (lhm/insertm id-4 3)
                                (lhm/insertm id-1 0)
                                (lhm/appendm 5)))
               lhm/to-vector)
           [0 1 2 3 4 5])))
  (testing "get/set"
    (is (= (-> (lhm/exec (w/mdo (lhm/appendm {:v "a"})
                                id-2 <- (lhm/appendm {:v "b"})
                                id-3 <- (lhm/appendm {:v "c"})
                                v-2 <- (lhm/getm id-2)
                                v-3 <- (lhm/getm id-3)
                                (lhm/setm id-2 v-3)
                                (lhm/setm id-3 v-2)))
               lhm/to-vector)
           [{:v "a"} {:v "c"} {:v "b"}])))
  (testing "remove"
    (is (= (-> (lhm/exec (w/mdo id-1 <- (lhm/appendm 1)
                                id-2 <- (lhm/appendm 2)
                                (lhm/appendm 3)
                                (lhm/removem id-1)
                                id-4 <- (lhm/appendm 4)
                                (lhm/insertm id-2 0)
                                (lhm/removem id-4)
                                (lhm/appendm 5)))
               lhm/to-vector)
           [0 2 3 5])))
  (testing "take-while"
    (is (= (-> (lhm/exec (w/mdo (lhm/appendm 1)
                                (lhm/appendm 2)
                                (lhm/appendm 3)
                                (lhm/appendm 4)
                                (lhm/appendm 5)))
               (lhm/take-while #(< % 3)))
           [1 2])))
  (testing "rebuild"
    (let [v (lhm/exec (w/mdo (lhm/appendm {:v "a"} a/set-id)
                             (lhm/appendm {:v "b"} a/set-id)
                             (lhm/appendm {:v "c"} a/set-id)
                             (lhm/appendm {:v "d"} a/set-id)
                             (lhm/appendm {:v "e"} a/set-id)))]
      (is (= (-> v lhm/to-vector lhm/rebuild)
             v)))
    (let [v (lhm/exec (w/mdo (lhm/appendm {:v "a"} a/set-id)
                             (lhm/appendm {:v "b"} a/set-id)
                             (lhm/appendm {:v "c"} a/set-id)
                             id-e <- (lhm/appendm {:v "e"} a/set-id)
                             (lhm/insertm id-e {:v "d"} a/set-id)
                             ))]
      (is (= (-> v lhm/to-vector lhm/rebuild)
             v))))
  (testing "ignore ops on missing id"
    (is (= (-> (lhm/exec (w/mdo id <- (lhm/appendm {:v "a"})
                                (lhm/setm (inc id) {:v "b"})))
               lhm/to-vector)
           [{:v "a"}]))
    (is (= (-> (lhm/exec (w/mdo id <- (lhm/appendm {:v "a"})
                                (lhm/insertm (inc id) {:v "b"})))
               lhm/to-vector)
           [{:v "a"}]))
    (is (= (-> (lhm/exec (w/mdo id <- (lhm/appendm {:v "a"})
                                (lhm/removem (inc id))))
               lhm/to-vector)
           [{:v "a"}]))))
