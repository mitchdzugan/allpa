(ns allpa.core
  #?(:clj (:require [clojure.string :as string]
                    [clojure.walk :as walk]
                    [clojure.core.match]
                    [net.cgrand.macrovich :as macros])
     :cljs (:require [cljs.core.match]))
  #?(:cljs (:require-macros [allpa.core :refer [varg# defprotomethod]]
                            [net.cgrand.macrovich :as macros]
                            [cljs.core.match])))

(def clj? #?(:clj true :cljs false))

;; util

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

(defn curry [f & args2]
  (fn [& args1]
    (apply f (concat args1 args2))))

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

(defn map-values [f m] (reduce-kv #(assoc %1 %2 (f %3 %2)) {} m))
(defn map-keys   [f m] (reduce-kv #(assoc %1 (f %3 %2) %3) {} m))

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

(def id ::id)

(def set-id #(assoc %1 ::id %2))

(defrecord Ok [result])
(defrecord Fail [error])

(defn ensure-vec [mv] (if (vector? mv) mv [mv]))

#?(:clj
   (defmacro defprotomethod [method args & defs]
     `(do (defprotocol ~(symbol (str "proto-" (name method)))
            ~(clojure.core/list method (vec (map (fn [_] (gensym "arg")) args))))
          (extend-protocol ~(symbol (str "proto-" (name method)))
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
                                    [type `(~method ~args ~body)]))
                                (ensure-vec types)))
                (partition 2 defs))))))

