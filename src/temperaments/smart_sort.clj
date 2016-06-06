;(ns eu.algoholic.smart_sort)

(def n_lst_cmp (comp
	(fn [a] (if (empty? a) false (< (first (first a)) (second (first a)))))
	(fn [a] (drop-while #(== (first %) (second %)) a))
	(fn [a,b] (map vector a b))
))

(def smart_sort (comp
	(fn [a] (sort n_lst_cmp a))
	(fn [a] (reduce concat (map second (sort a))))
	(fn [a] (zipmap (map #(if (== % 0) Integer/MAX_VALUE %) (keys a)) (map #(sort n_lst_cmp %) (vals a))))
	(fn [a] (group-by count a))
	(fn [a] (map-indexed (fn [b,c] (with-meta (map #(Integer/parseInt %) c) {:path (get a b) :idx b})) (map #(re-seq #"[0-9]+" %) a)))
	vec
	sort
))

(defn get_meta [a,k] (map #(get (meta %) k) a))

(defn sort-keywords [kwlist]
  (let [kwstrs (map str kwlist)
        kwstrs (map #(subs % 1) kwstrs)
        sorted (get_meta (smart_sort kwstrs) :path)]
    (map keyword sorted)))

(defn compare-keywords [a b]
  (let [[c d] (sort-keywords [a b])]
    (= a c)))
