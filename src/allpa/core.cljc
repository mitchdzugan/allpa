(ns allpa.core
  #?(:clj (:require [clojure.string :as string]
                    [clojure.walk :as walk])
     :cljs (:require-macros [allpa.core :refer [varg#]])))

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

(defn alpha-num [n]
  (if (< n 26)
    (num-lookup n)
    (str (alpha-num (dec (quot n 26))) (num-lookup (mod n 26)))))

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


;; i hate defrecord/defprotocol

(def mk (varg# (assoc %2 ::type %1)))

(def Default ::Default)

(def type ::type)

(def id ::id)

(def set-id #(assoc %1 ::id %2))

(defn match [funcs & last-args]
  (fn [obj]
    (let [val (or (type obj) obj)
          args (conj (vec last-args) obj)
          f (or (get funcs val) (Default funcs))]
      (if (fn? f)
        (apply f args)
        (let [[get-drill-obj drill-funcs] f
              drill-obj (if (and (keyword? get-drill-obj)
                                 (not (empty? last-args)))
                          (get-drill-obj (nth last-args 0))
                          (apply get-drill-obj args))]
          ((apply match drill-funcs args) drill-obj))))))

