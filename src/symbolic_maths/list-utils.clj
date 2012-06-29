(ns symbolic-maths.core)

(defn to-list [expr]
  (cond (list? expr) expr
	(coll? expr) (seq expr)
	:default (list expr)))

(defn keep-list [expr]
  (if (and (coll? expr) (not (= 1 (count expr)))) (list expr) expr))

(defn unchunk [s]
  (when (seq s)
    (lazy-seq
      (cons (first s)
            (unchunk (next s))))))


(defn is-in? [coll key similar]
  (loop [col coll]
    (cond (= col ()) false
	  (similar key (first col)) true
	  :default (recur (rest col)))))

(defn get-unique-counts [lst similar]
  ;;(println lst)
  (loop [elts (if (coll? lst) lst (list lst))
	 tmp (first elts)
	 uniques ()
	 res ()]
    (cond (= () elts) res
	  (is-in? uniques tmp similar) (recur
				   (rest elts)
				   (second elts)
				   uniques
				   res)
	  :default (recur
		    (rest elts)
		    (second elts)
		    (conj uniques tmp)
		    (conj res {:unique tmp :count (count (second (first (group-by #(similar tmp %) elts))))})))))



(defn === 
  "returns true if x and y are the same modulo permutations, false otherwise"
  [x y]
  (if (not (seq? x))
    (if (seq? y)
      false
      (= x y))
    (let [x-unique-counts (get-unique-counts x ===)
	  y-unique-counts (get-unique-counts y ===)]
      (loop [x-elts x-unique-counts
	     tmp-elt ((first x-unique-counts) :unique)
	     tmp-count ((first x-unique-counts) :count)]
	(cond (= x-elts ()) true
	      (reduce #(or %1 %2) (map #(and (=== tmp-elt (% :unique)) (= tmp-count (% :count))) y-unique-counts))
	      (recur (rest x-elts)
		     (if (nil? (keys (second x-elts))) nil ((second x-elts) :unique))
		     (if (nil? (keys (second x-elts))) nil ((second x-elts) :count)))
	      :default false)))))

(defn proportional? 
  "returns true if x = A*k1 and y = B*k2, with A and B numbers, and k1 and k2 similar"
  [x y similar op]
  (let [need-split? #(and (seq? %) (= op (first %)))]
    (cond (and (not (need-split? x)) (not (need-split? y)))
	  (similar x y)
	  (not (need-split? y)) (is-in? x y similar)
	  (not (need-split? x)) (is-in? y x similar)
	  :default
	  (let [symb-x (seq ((group-by number? (if (seq? x) x (list x))) false))
		symb-y (seq ((group-by number? (if (seq? y) y (list y))) false))]
	    (similar symb-x symb-y)))))

(defn operator? [symbol]
  (reduce #(or %1 %2) (map #(= symbol %) '(+ * - /))))

(defn function? [symbol]
  (reduce #(or %1 %2) (map #(= symbol %) '(cos sin tan ln log exp))))


(defn xor [& args]
  (and (not (reduce #(and %1 %2) args)) (reduce #(or %1 %2) args)))