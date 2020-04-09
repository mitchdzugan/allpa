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
                                      tagless? (= '__ x)
                                      tagged? (or tagless?
                                                  (and (symbol? x)
                                                       (Character/isUpperCase (first (name x)))))
                                      {:keys [wrapper forms]} (reduce
                                                               (fn [{:keys [wrapper done? next? forms] :as agg}
                                                                    curr]
                                                                 (cond
                                                                   done? (update agg :forms #(conj %1 curr))
                                                                   next? {:wrapper #(-> `(~%1 :as ~curr))
                                                                          :done? true
                                                                          :forms forms}
                                                                   (= :as curr) {:done? false
                                                                                 :next? true
                                                                                 :forms forms}
                                                                   :else (update agg :forms #(conj %1 curr))))
                                                               {:wrapper identity
                                                                :forms []}
                                                               xs)]
                                  (if (not tagged?) form
                                      (wrapper
                                       (merge (if tagless? {}
                                                  {::type
                                                   `(~pass :guard (fn [~t] (= ~t (-> ~x meta :allpa-type))))})
                                              (apply hash-map (->> forms
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
                                                                   :forms))))))))
                                        form)))
                   (range))))))

#?(:clj
   (defmacro defn-match
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
