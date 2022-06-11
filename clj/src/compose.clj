(ns compose)

(defn compose [f g]
  (fn [h]
    (f (g h))))

