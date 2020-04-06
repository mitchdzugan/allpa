(ns allpa.core
  #?(:clj (:require [clojure.string :as string]
                    [clojure.walk :as walk]
                    [clojure.core.match]
                    [net.cgrand.macrovich :as macros])
     :cljs (:require [cljs.core.match]))
  #?(:cljs (:require-macros [allpa.core :refer [varg# deftagged match defn-match fn-match]]
                            [net.cgrand.macrovich :as macros]
                            [cljs.core.match]
                            )))

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

#?(:clj
   (defmacro deftagged [label argv-raw]
     (let [required (remove vector? argv-raw)
           defaults (->> argv-raw
                         (filter vector?)
                         (mapcat #(-> %1))
                         (map #(-> [%1 %2]) (range))
                         (filter (fn [[id _]] (odd? id)))
                         (map #(nth %1 1)))
           argv (->> argv-raw
                     (filter vector?)
                     (mapcat #(-> %1))
                     (map #(-> [%1 %2]) (range))
                     (filter (fn [[id _]] (even? id)))
                     (map #(nth %1 1))
                     (concat required))
           splat (gensym "splat")
           mkkw #(if (Character/isUpperCase (first (str %1)))
                   (keyword (str *ns*) (str %1))
                   (keyword %1))]
       `(def ~label
          (with-meta
            (fn [& ~splat]
              (when (< (count ~splat) ~(count required))
                (throw (Err ~(str *ns* "/" label " must have at least " (count required) " arguments"))))
              (when (> (count ~splat) ~(count argv))
                (throw (Err ~(str *ns* "/" label " can have no more than " (count argv) " arguments"))))
              (mk ~(keyword (str *ns*) (name label))
                  ~(if (empty? argv) '{}
                       `(let [[~@argv] (concat ~splat (drop (- (count ~splat) ~(count required))
                                                            [~@defaults]))]
                          (hash-map ~@(mapcat #(-> [(mkkw %1) %1]) argv))))))
            {:allpa-type ~(keyword (str *ns*) (name label))})))))

#?(:clj
   (defmacro match [v & specs]
     {:style/indent 1}
     `(~(macros/case :clj 'clojure.core.match/match
                     :cljs 'cljs.core.match/match)
       ~v
       ~@(->> specs
              (map (fn [id form]
                     (if (odd? id) form
                         (walk/postwalk
                          (fn [form]
                            (if (not (list? form)) form
                                (let [t (gensym "t")
                                      pass (gensym "pass")
                                      [x & xs] form
                                      tagged? (and (symbol? x)
                                                   (Character/isUpperCase (first (name x))))]
                                  (if (not tagged?) form
                                      (merge {::type `(~pass :guard (fn [~t] (= ~t (-> ~x meta :allpa-type))))}
                                             (apply hash-map (->> xs
                                                                  (reduce
                                                                   (fn [{:keys [kw? forms]} curr]
                                                                     (cond
                                                                       (not kw?) {:kw? true
                                                                                  :forms (conj forms curr)}
                                                                       (keyword? curr) {:forms (conj forms curr)
                                                                                        :kw? false}
                                                                       :else {:kw? true
                                                                              :forms (-> forms
                                                                                         (conj (keyword curr))
                                                                                         (conj curr))}))
                                                                   {:kw? true :forms []})
                                                                  :forms)))))))
                                        form)))
                   (range))))))

#?(:clj
   (defmacro defn-match
     {:style/indent 1}
     [label & specs]
     (let [splat (gensym "splat")]
       `(defn ~label [& ~splat]
          (match (vec ~splat) ~@specs)))))

#?(:clj
   (defmacro fn-match
     [& specs]
     (let [splat (gensym "splat")]
       `(fn [& ~splat]
          (match (vec ~splat) ~@specs)))))
