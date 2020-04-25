(ns allpa.util)

(defn map-values [f m] (reduce-kv #(assoc %1 %2 (f %3 %2)) {} m))
(defn map-keys   [f m] (reduce-kv #(assoc %1 (f %3 %2) %3) {} m))
(def id ::id)
(def set-id #(assoc %1 ::id %2))
