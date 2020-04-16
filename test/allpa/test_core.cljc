(ns allpa.test-core
  (:require
   #?(:clj [clojure.test :refer :all]
      :cljs [cljs.test :refer-macros [is are deftest testing use-fixtures]])
   [wayra.core :as w]
   [allpa.core :as a]
   [allpa.test-rec :as tr :refer [->Test3 ->Test4]]
   [allpa.linked-hash-map :as lhm]))

(defrecord A [val])
(defrecord B [val])
(defrecord C [val])

(a/defprotomethod proto [{:keys [val]}]
  !tr/->Test1 1
  !tr/Test2 2
  !->Test3 3
  !Test4 4

  [A B]
  (inc val)
  C
  (dec val))

(deftest core-api
  (testing "defprotomethod"
    (is (= (proto (->A 1)) 2))
    (is (= (proto (->B 1)) 2))
    (is (= (proto (->C 1)) 0))
    (is (= (proto (tr/->Test1)) 1))
    (is (= (proto (tr/->Test2)) 2))
    (is (= (proto (tr/->Test3)) 3))
    (is (= (proto (tr/->Test4)) 4)))
  (testing "snake"
    (is (= (a/snake 2 [1 2 3 4 5 6 7 8])
           [[1 4 5 8] [2 3 6 7]]))
    (is (= (a/snake 2 [1 2 3 4 5 6 7])
           [[1 4 5] [2 3 6 7]]))
    (is (= (a/snake 2 [1 2 3 4 5 6 7 8 9])
           [[1 4 5 8 9] [2 3 6 7]]))
    (is (= (a/snake 4 [1 2 3 4 5 6 7 8 9])
           [[1 8 9] [2 7] [3 6] [4 5]])))
  (testing "unsnake"
    (is (= (a/unsnake [[1 4 5 8] [2 3 6 7]])
           [1 2 3 4 5 6 7 8]))
    (is (= (a/unsnake [[1 4 5] [2 3 6 7]])
           [1 2 3 4 5 6 7]))
    (is (= (a/unsnake [[1 4 5 8 9] [2 3 6 7]])
           [1 2 3 4 5 6 7 8 9]))
    (is (= (a/unsnake [[1 8 9] [2 7] [3 6] [4 5]])
           [1 2 3 4 5 6 7 8 9])))
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
  (testing "map-values"
    (is (= (a/map-values vector {:a 1 :b 2})
           {:a [1 :a] :b [2 :b]})))
  (testing "map-keys"
    (is (= (a/map-keys vector {:a 1 :b 2})
           {[1 :a] 1 [2 :b] 2})))
  (testing "id"
    (is (= (-> {} a/id) nil))
    (is (= (-> {} (a/set-id 1) a/id) 1)))
  (testing "parse-int"
    (is (= (a/parse-int "5") 5))
    (is (= (a/parse-int "5432") 5432))
    (is (= (a/parse-int "-7") -7))
    (is (= (a/parse-int "asdf") nil))
    (is (= (a/parse-int "asdf" "default") "default")))
  (testing "curry"
    (is (= ((a/curry (fn [a b c] [a b c]) 3) 1 2) [1 2 3]))
    (is (= ((a/curry (fn [a b c] [a b c]) 2 3) 1) [1 2 3]))
    (is (= ((a/curry (fn [a b c] [a b c]) 1 2 3)) [1 2 3]))))

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
           [1 2]))
    (is (= (-> lhm/linked-hash-map
               (lhm/take-while #(< % 3)))
           [])))
  (testing "map"
    (is (= (-> (lhm/exec (w/mdo (lhm/appendm 1)
                                (lhm/appendm 2)
                                (lhm/appendm 3)
                                (lhm/appendm 4)
                                (lhm/appendm 5)))
               ((partial lhm/map inc))
               (lhm/to-vector)))
           [2 3 4 5 6]))
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
             v)))
    (let [v (lhm/exec (w/mdo id-a <- (lhm/appendm {:v "a"} a/set-id)
                             (lhm/appendm {:v "b"} a/set-id)
                             (lhm/appendm {:v "c"} a/set-id)
                             (lhm/appendm {:v "d"} a/set-id)
                             (lhm/insertm id-a {:v "_"} a/set-id)
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
