(ns monnit.impl.util)

(defmacro typecase [[x expr] & clauses]
  (let [v (gensym)]
    `(let [~v ~expr]
       (condp instance? ~v
         ~@(->> (partition 2 clauses)
                (mapcat (fn [[type body]]
                          [type `(let [~(vary-meta x assoc :tag type) ~v] ~body)])))))))

