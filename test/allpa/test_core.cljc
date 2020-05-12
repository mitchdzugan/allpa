(ns allpa.test-core
  (:require
   #?(:clj [clojure.test :refer :all]
      :cljs [cljs.test :refer-macros [is are deftest testing use-fixtures]])
   [wayra.core :as w]
   [allpa.core :as a]
   [allpa.test-rec :as tr :refer [->Test3 ->Test4]]
   #?(:cljs [cognitect.transit :as t])
   [allpa.linked-hash-map :as lhm]))

(a/deftagged A [val])
(a/deftagged B [val])
(a/deftagged C [val])

#?(:cljs
   (let [w (a/writer :json)
         r (a/reader :json)]
     (println (get (t/read r (t/write w {(->A 2) (->B 3)})) (->A 2)))
     (println (t/read r (t/write w (->A 1))))
     (println (t/read r (t/write w (->A (->B (->C (->Test4)))))))))

(extend-protocol a/Simplify
  A
  (simple [{:keys [val]}] (str "A:" val)))

(a/defprotomethod proto [{:keys [val]}]
  !tr/->Test1 1
  !tr/Test2 2
  !->Test3 3
  !Test4 (inc (proto (tr/->Test3)))

  [A B]
  (inc val)
  C
  (dec val))

(a/defprotomethod proto-scnd [a ^this {:keys [val]}]
  !tr/->Test1 (+ 1 a)
  !tr/Test2 (+ 2 a)
  !->Test3 (+ 3 a)
  !Test4 (+ 4 a)

  [A B]
  (+ a (inc val))
  C
  (+ a (dec val)))

(def f-called (atom 0))
(defn f [i]
  (swap! f-called inc)
  (inc i))

(def m-f (a/memoize f 3))

(deftest core-api
  (testing "simplify"
    (is (= (-> lhm/empty
               (lhm/append- 1)
               (lhm/append- (-> lhm/empty
                                (lhm/append- 2)
                                (lhm/append- (->A 3))
                                (lhm/append- 4)))
               (lhm/append- 5)
               a/simplify)
           [1 [2 "A:3" 4] 5])))
  (testing "memoize"
    (is (= 1 (m-f 0))) (is (= 1 @f-called))
    (is (= 2 (m-f 1))) (is (= 2 @f-called))
    (is (= 3 (m-f 2))) (is (= 3 @f-called))
    (is (= 1 (m-f 0))) (is (= 3 @f-called))
    (is (= 2 (m-f 1))) (is (= 3 @f-called))
    (is (= 3 (m-f 2))) (is (= 3 @f-called))
    (is (= 4 (m-f 3))) (is (= 4 @f-called))
    (is (= 3 (m-f 2))) (is (= 4 @f-called))
    (is (= 2 (m-f 1))) (is (= 4 @f-called))
    ;; 0 was LRU at time of the 4th input
    ;; so it was removed from memory
    (is (= 1 (m-f 0))) (is (= 5 @f-called)))
  (testing "queue"
    (is (= (->> a/queue
                ((a/flip conj) 1)
                ((a/flip conj) 2)
                ((a/flip conj) 3)
                ((a/flip conj) 4)
                ((a/flip conj) 5)
                pop
                ((a/flip conj) 6)
                (a/filter-queue #(> % 3))
                ((a/flip conj) 3)
                vec)
           [4 5 6 3])))
  (testing "tag"
    (is (= (a/tag (->A 1)) ::A))
    (is (= (a/tag (tr/->Test1)) ::tr/Test1))
    )
  (testing "defprotomethod"
    (is (= (proto (->A 1)) 2))
    (is (= (proto (->B 1)) 2))
    (is (= (proto (->C 1)) 0))
    (is (= (proto (tr/->Test1)) 1))
    (is (= (proto (tr/->Test2)) 2))
    (is (= (proto (tr/->Test3)) 3))
    (is (= (proto (tr/->Test4)) 4))
    (is (= (proto (->A 1)) (proto-scnd 0 (->A 1))))
    (is (= (proto (->B 1)) (proto-scnd 0 (->B 1))))
    (is (= (proto (->C 1)) (proto-scnd 0 (->C 1))))
    (is (= (proto (tr/->Test1)) (proto-scnd 0 (tr/->Test1))))
    (is (= (proto (tr/->Test2)) (proto-scnd 0 (tr/->Test2))))
    (is (= (proto (tr/->Test3)) (proto-scnd 0 (tr/->Test3))))
    (is (= (proto (tr/->Test4)) (proto-scnd 0 (tr/->Test4)))))
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
  (testing "index-by"
    (is (= (a/index-by :a [{:a 1 :msg "h"} {:a 2 :msg "i"} {:a 3 :msg "!"}])
           {1 {:a 1 :msg "h"}
            2 {:a 2 :msg "i"}
            3 {:a 3 :msg "!"}})))
  (testing "flip"
    (is (= ((a/flip quot) 3 6)
           2))
    (is (= (->> {:a 1 :b 2 :c 3}
                ((a/flip dissoc) :b :c)
                ((a/flip update) :a inc))
           {:a 2})))
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
