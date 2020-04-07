(ns allpa.test-core
  (:require
   #?(:clj [clojure.test :refer :all]
      :cljs [cljs.test :refer-macros [is are deftest testing use-fixtures]])
   [wayra.core :as w]
   [allpa.core :as a]
   [allpa.dep :as d :refer [Dep]]
   [allpa.linked-hash-map :as lhm]))

(a/deftagged MyA [])
(a/deftagged MyB [v1 v2])
(a/deftagged MyC [v1 v2 [def1 10 def2 20 def3 30]])
(a/deftagged Namespaced [A B c])

(a/defn-match match-f1
  [1 2 3] :a
  [7] :b)

(def match-f2
  (a/fn-match
   [1 2 3] :a
   [7] :b))

(deftest core-api
  (testing "deftagged"
    (is (= (MyA) (a/mk ::MyA {})))
    (is (= (MyB 1 2) (a/mk ::MyB {:v1 1 :v2 2})))
    (is (= (MyC 1 2) (a/mk ::MyC {:v1 1 :v2 2 :def1 10 :def2 20 :def3 30})))
    (is (= (MyC 1 2) (a/mk ::MyC {:v1 1 :v2 2 :def1 10 :def2 20 :def3 30})))
    (is (= (MyC 1 2 11) (a/mk ::MyC {:v1 1 :v2 2 :def1 11 :def2 20 :def3 30})))
    (is (= (MyC 1 2 11 12) (a/mk ::MyC {:v1 1 :v2 2 :def1 11 :def2 12 :def3 30})))
    (is (= (MyC 1 2 11 12 13) (a/mk ::MyC {:v1 1 :v2 2 :def1 11 :def2 12 :def3 13})))
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                          #"allpa.test-core/MyC must have at least 2 arguments"
                          (MyC 1)))

    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                          #"allpa.test-core/MyC can have no more than 5 arguments"
                          (MyC 1 2 11 12 13 14)))
    (is (= (Namespaced 1 2 3) (a/mk ::Namespaced {::A 1 ::B 2 :c 3}))))
  (testing "match"
    (is (= (a/match 1 1 :good 2 :bad) :good))
    (is (= (a/match 2 1 :good 2 :bad) :bad))
    (is (= (a/match (MyB 1 2)
                    (MyB :v1 1 :v2 2) true)
         true))
    (is (= (a/match (MyB 10 20)
                    (MyB :v1 1 :v2 2) true
                    (MyB :v1 10 :v2 20) false)
           false))
    (is (= (a/match (MyB 10 20)
                    (MyB :v1 10 v2) (inc v2)
                    (MyB :v1 100 v2) (dec v2))
           21))
    (is (= (a/match (MyB 100 20)
                    (MyB :v1 10 v2) (inc v2)
                    (MyB :v1 100 v2) (dec v2))
           19))
    (is (= (match-f1 1 2 3) :a))
    (is (= (match-f1 7) :b))
    (is (= (match-f2 1 2 3) :a))
    (is (= (match-f2 7) :b))
    (is (= (a/match (d/Dep) (allpa.dep/Dep) true _ false) true))
    (is (= (a/match (d/Dep) (d/Dep) true _ false) true))
    (is (= (a/match (Dep) (d/Dep) true _ false) true))
    (is (= (a/match (d/Dep) (Dep) true _ false) true))
    )
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
