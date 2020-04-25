(ns allpa.linked-hash-map
  (:require [allpa.util :as a]
            [wayra.core :as w :refer [defnm]]))

;; linked-hash-map

(def linked-hash-map {::lhm? true ::next-id 1 ::hash-map {}})

(def next-id ::next-id)

(def empty linked-hash-map)

(defn empty? [lhm] (-> lhm (get-in [::hash-map (::first lhm)]) nil?))

(defn get [lhm id] (get-in lhm [::hash-map id ::val]))

(defn set [lhm id v]
  (let [to-set (get-in lhm [::hash-map id])]
    (if (not to-set) lhm
        (assoc-in lhm [::hash-map id ::val] v))))

(defn append-unsafe [lhm v id]
  (let [last (::last lhm)]
    (if (empty? lhm)
      {:id id
       :lhm (-> lhm
                (assoc-in [::hash-map id] {::val v ::prev nil ::next nil})
                (assoc ::next-id (inc id))
                (assoc ::first id)
                (assoc ::last id))}
      {:id id
       :lhm (-> lhm
                (assoc-in [::hash-map last ::next] id)
                (assoc-in [::hash-map id] {::val v ::prev last ::next nil})
                (assoc ::next-id (inc id))
                (assoc ::last id))})))

(defn map [f lhm]
  (update lhm ::hash-map #(a/map-values (fn [v _] (update v ::val f)) %1)))

(defn rebuild
  ([v] (rebuild v a/id))
  ([v get-id]
   (let [next-id (inc (reduce #(max %1 (get-id %2)) 0 v))]
     (-> #(-> %1 (append-unsafe %2 (get-id %2)) :lhm)
         (reduce linked-hash-map v)
         (assoc ::next-id next-id)))))

(defn append
  ([lhm v] (append lhm v (fn [a _] a)))
  ([lhm v-raw add-id]
   (let [next-id (::next-id lhm)
         v (add-id v-raw next-id)]
     (append-unsafe lhm v next-id))))

(defn insert
  ([lhm before v] (insert lhm before v (fn [a _] a)))
  ([lhm before v-raw add-id]
   (let [to-prepend (get-in lhm [::hash-map before])
         {::keys [prev]} to-prepend
         {::keys [next-id]} lhm
         v (add-id v-raw next-id)
         set-prevs-next (if prev
                          #(assoc-in % [::hash-map prev ::next] next-id)
                          #(assoc % ::first next-id))]
     (if (not to-prepend) {:lhm lhm}
         {:id next-id
          :lhm (-> lhm
                   set-prevs-next
                   (assoc-in [::hash-map before ::prev] next-id)
                   (assoc-in [::hash-map next-id] {::val v
                                                   ::prev prev
                                                   ::next before})
                   (assoc ::next-id (inc next-id)))}))))

(defn remove [lhm id]
  (let [to-remove (get-in lhm [::hash-map id])
        {::keys [prev next]} to-remove
        set-prevs-next (if prev
                         #(assoc-in % [::hash-map prev ::next] next)
                         #(assoc % ::first next))
        set-nexts-prev (if next
                         #(assoc-in % [::hash-map next ::prev] prev)
                         #(assoc % ::last prev))]
    (if (not to-remove) lhm
        (-> lhm set-prevs-next set-nexts-prev (dissoc id)))))

(def append- (comp :lhm append))
(def insert- (comp :lhm insert))

(defn take-while [lhm pred]
  (if (empty? lhm) []
      (letfn [(build-rec [node]
                (let [{::keys [next val]} node]
                  (cond
                    (not (pred val)) '()
                    (not next) (list val)
                    :else (conj (build-rec (get-in lhm [::hash-map next]))
                                val))))]
        (-> lhm (get-in [::hash-map (::first lhm)]) build-rec vec))))

(defn to-vector [lhm] (take-while lhm (fn [_] true)))

(defnm appendm [& args]
  lhm <- w/get
  let [res (apply append lhm args)]
  (w/put (:lhm res))
  (w/pure (:id res)))

(defnm insertm [& args]
  lhm <- w/get
  let [res (apply insert lhm args)]
  (w/put (:lhm res))
  (w/pure (:id res)))

(defnm getm [& args]
  lhm <- w/get
  (w/pure (apply get lhm args)))

(defnm setm [& args]
  lhm <- w/get
  (w/put (apply set lhm args)))

(defnm removem [& args]
  lhm <- w/get
  (w/put (apply remove lhm args)))

(defn exec
  ([m] (exec linked-hash-map m))
  ([lhm m] (:state (w/exec {:init-state lhm} m))))

