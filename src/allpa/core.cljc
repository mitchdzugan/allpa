(ns allpa.core
  (:require [allpa.util :as util]
            [allpa.linked-hash-map :as lhm]
            [clojure.walk :as walk]
            #?(:cljs [cognitect.transit :as t]))
  #?(:clj (:require [clojure.string :as string]
                    [net.cgrand.macrovich :as macros]))
  #?(:cljs (:require-macros [allpa.core :refer [varg# defprotomethod deftagged]]
                            [net.cgrand.macrovich :as macros])))

;; util
(defn compare-ord [v1 v2]
  (cond
    (> v1 v2) :gt
    (< v1 v2) :lt
    :else :eq))

(defn extreme-by [extreme compare items]
  (reduce #(condp = (compare %1 %2)
             extreme %1
             %2)
          (first items)
          items))

(def minimum-by (partial extreme-by :lt))
(def maximum-by (partial extreme-by :gt))

(def map-values util/map-values)
(def map-keys util/map-keys)
(def id util/id)
(def set-id util/set-id)

(defn index-by [f coll]
  (->> coll
       (group-by f)
       (map-values (fn [v _] (first v)))))

(defprotocol Simplify
  (simple [this] "return a simpler version of the object. usually for printing"))

(extend-protocol Simplify
  #?(:clj clojure.lang.PersistentArrayMap :cljs cljs.core.PersistentArrayMap)
  (simple [m]
    (if (::lhm/lhm? m) (lhm/to-vector m) m)))

(defn simplify [obj]
  (walk/postwalk #(if (satisfies? Simplify %) (simple %) %) obj))

(defn curry [f & args2]
  (fn [& args1]
    (apply f (concat args1 args2))))

(def queue
  #?(:clj (clojure.lang.PersistentQueue/EMPTY)
     :cljs #queue []))

(defn filter-queue
  ([p q] (filter-queue p q queue))
  ([p q c]
   (if (empty? q)
     c
     (let [v (peek q)
           next (pop q)]
       (filter-queue p next (if (p v) (conj c v) c))))))

(defn memoize
  ([f] (memoize f 10))
  ([f size]
   (let [has (atom {})
         saved (atom {})
         lru (atom queue)]
     (fn [& args]
       (let [h (hash args)
             has? (get @has h)]
         (cond
           has?
           (do (swap! lru #(-> (filter-queue (curry not= h) %)
                               (conj h)))
               (get @saved h))
           (= size (count @saved))
           (let [res (apply f args)
                 delete (peek @lru)]
             (swap! has #(-> % (dissoc delete) (assoc h true)))
             (swap! saved #(-> % (dissoc delete) (assoc h res)))
             (swap! lru #(-> % pop (conj h)))
             res)
           :else
           (let [res (apply f args)]
             (swap! has #(assoc % h true))
             (swap! saved #(assoc % h res))
             (swap! lru #(conj % h))
             res)))))))

(defn flip [f]
  (fn [& args]
    (apply f (last args) (->> args reverse (drop 1) reverse))))

(defn parse-int
  ([s] (parse-int s nil))
  ([s default]
   (let [tried (try
                 #?(:cljs (js/parseInt s 10)
                    :clj (Integer/parseInt s))
                 #?(:cljs (catch js/Object e default)
                    :clj (catch Exception e default)))]
     (if (int? tried) tried default))))

(defn num-lookup [n]
  (nth ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
        "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"]
       n))

(defn Err [arg]
  #?(:clj (Exception. arg)
     :cljs (js/Error. arg)))

(defn alpha-num [n]
  (if (< n 26)
    (num-lookup n)
    (str (alpha-num (dec (quot n 26))) (num-lookup (mod n 26)))))

(defn ordinal [n]
  (let [mod10 (mod n 10)
        mod100 (mod n 100)]
    (str n (cond
             (and (= 1 mod10)
                  (not= 11 mod100)) "st"
             (and (= 2 mod10)
                  (not= 12 mod100)) "nd"
             (and (= 3 mod10)
                  (not= 13 mod100)) "rd"
             :else "th"))))

(defn snake [buckets a]
  (let [size (count a)
        safe-nth #(if (>= %1 size) nil (nth a %1))
        hm (reduce (fn [hm i]
                     (reduce (fn [hm j]
                               (update hm i #(-> %1
                                                 (conj (safe-nth (+ (* 2 buckets j) i)))
                                                 (conj (safe-nth (+ (* 2 buckets j)
                                                                    (- (* 2 buckets) i 1)))))))
                             hm
                             (range (inc (quot (quot size buckets) 2)))))
                   {}
                   (range buckets))]
    (->> (range buckets)
         (map #(->> (get hm %1) (remove nil?) reverse vec))
         vec)))

(defn unsnake [aa]
  (let [nnth (fn [i j]
               (let [a (if (>= i (count aa)) nil (nth aa i))]
                 (if (>= j (count a)) nil (nth a j))))
        outer (count aa)
        inner (reduce #(max %1 (count %2)) 0 aa)]
    (->> (range (inc (quot inner 2)))
         (mapcat (fn [j]
                   (concat (->> (range (inc outer))
                                (map (fn [i] (nnth i (* 2 j))))
                                (remove nil?))
                           (->> (range outer -1 -1)
                                (map (fn [i] (nnth i (inc (* 2 j)))))
                                (remove nil?)))))
         vec)))


;; macros

#?(:clj
   (defmacro varg# [& statements]
     (let [args (gensym "args")]
       `(fn [& ~args]
          ~@(->> statements
                 (walk/postwalk (fn [sym]
                                  (let [sym-name (if (symbol? sym) (name sym) "")]
                                    (if (string/starts-with? sym-name "%")
                                      `(nth ~args ~(-> sym-name (subs 1) (parse-int -1) dec) nil)
                                      sym)))))))))

;; math

(defn power [b e]
  (if (< e 0) 0
      #?(:clj (int (Math/floor (Math/pow b e)))
         :cljs (.round js/Math (.floor js/Math (.pow js/Math b e))))))

(def p2 (partial power 2))

;; other

(defn ensure-vec [mv] (if (vector? mv) mv [mv]))

(defprotocol Tagged
  (tag [_] "keyword representing record's type"))

(def w-handlers (atom {}))
(def r-handlers (atom {}))

#?(:cljs
   (deftype TaggedHandler []
     Object
     (tag [this v] (str (allpa.core/tag v)))
     (rep [this v] (into {} v))
     (stringRep [this v] nil)))

#?(:cljs
   (defn writer
     ([t] (writer t {}))
     ([t opt] (t/writer t (update opt :handlers (partial merge @w-handlers))))))

#?(:cljs
   (defn reader
     ([t] (reader t {}))
     ([t opt] (t/reader t (update opt :handlers (partial merge @r-handlers))))))

#?(:clj
   (defmacro deftagged [sym args]
     (let [curr-ns (str *ns*)
           as-kw (keyword curr-ns (name sym))

           update-handlers
           [`(swap! w-handlers
                    (fn [curr#]
                      (assoc curr# ~sym (->TaggedHandler))))
            `(swap! r-handlers
                    (fn [curr#]
                      (assoc curr#
                             ~(str as-kw)
                             (fn [map#]
                               (~(symbol curr-ns (str "map->" (name sym)))
                                map#)))))]]
       `(do
          (defrecord ~sym ~args
            Tagged
            (tag [_#] ~as-kw))
          ~@(macros/case :clj []
                         :cljs update-handlers)))))

#?(:clj
   (defmacro defprotomethod [method args & defs]
     (let [proto-sym (gensym (str "proto-" (name method)))
           proto-fn (gensym (str "fn-" (name method)))
           this-ind (->> args
                         (map #(-> {:tag (-> %2 meta :tag) :ind %1}) (range))
                         (filter #(= 'this (:tag %)))
                         first
                         (#(get % :ind 0)))]
       `(do (defprotocol ~proto-sym
              ~(clojure.core/list proto-fn [(gensym "_")]))
            (defn ~method [& args#]
              (apply (~proto-fn (nth args# ~this-ind)) args#))
            (extend-protocol ~proto-sym
              ~@(mapcat (fn [[types body]]
                          (mapcat (fn [type]
                                    (let [stype (str type)
                                          type
                                          (if (string/starts-with? stype "!")
                                            (let [tname (name type)
                                                  tname (cond
                                                          (string/starts-with? tname "->") (subs tname 2)
                                                          (string/starts-with? tname "!->") (subs tname 3)
                                                          (string/starts-with? tname "!") (subs tname 1)
                                                          :else tname)
                                                  cons (symbol (when (namespace type)
                                                                 (subs (namespace type) 1))
                                                               (str "->" tname))]
                                              (macros/case :clj (symbol (str
                                                                         (-> cons resolve meta :ns str
                                                                             (string/replace #"-" "_"))
                                                                         "."
                                                                         tname))
                                                           :cljs (symbol (or (namespace cons)
                                                                             (str (get (-> &env :ns :uses)
                                                                                       (symbol (name cons)))))
                                                                         (subs (name cons) 2))))
                                            type)]
                                      [type `(~proto-fn [_#] (fn ~args ~body))]))
                                  (ensure-vec types)))
                  (partition 2 defs)))))))

(deftagged Ok [result])
(deftagged Fail [error])
