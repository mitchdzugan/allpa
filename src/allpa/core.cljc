(ns allpa.core)

(defn power [b e]
  (if (< e 0) 0
      #?(:clj (int (Math/floor (Math/pow b e)))
         :cljs (.round js/Math (.floor js/Math (.pow js/Math b e))))))

(def p2 (partial power 2))
