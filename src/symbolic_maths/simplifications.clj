(ns symbolic-maths.core)

(defn product? [x]
  (or (not (list? x)) (= (first x) '*)))


(defn reduce-associative
  "associatively expands a function starting with '+ or '*"
  [expr]
  (let [op (first expr)
	members (rest expr)]
    (loop [res (list op)
	   remaining members
	   tmp (first remaining)]
      (cond (= remaining ()) res
	    (and (seq? tmp) (= op (first tmp)))
	    (recur (concat res (rest (reduce-associative tmp)))
		   (rest remaining)
		   (second remaining))
	    :default
	    (recur (concat res (list tmp))
		   (rest remaining)
		   (second remaining))))))

(defn simplify-product [lst]
  (cond (not (seq? lst)) lst
	(is-in? lst 0 =) 0
	:default
	(let [members (rest (reduce-associative lst))
	      split (group-by number? members)
	      numbers (split true)
	      value (apply * numbers)
	      non-nums (seq (split false))]
	  (if (= 1 value)
	    (if (= 1 (count non-nums))
	      (first non-nums) (conj non-nums '*))
	    (conj (conj non-nums value) '*)))))

(defn simplify-sum [lst]
  ;;rewrite algo here using ===...
  (let [op (first lst)
	operand (eval op)
	members (rest (reduce-associative lst))
	split (group-by number? members)
	numbers (split true)
	symbols (map identity (split false))
	number (apply operand numbers)
	unique-symbols (get-unique-counts symbols ===) ;; shouldnt be === but a more compicated function
	factors (map #(list '* (% :count) (% :unique)) unique-symbols)
	result (conj factors  number '+)
	]
    (if (not (= op '+)) nil ;;op should be +
	result)))


(defn simplify
  "simplifies a mathematical expression"
  [expr]
  (cond (or (symbol? expr) (number? expr)) expr
	;;(and (list? expr) (= 2 (count expr)) (operator? (first expr)) (number? (second expr))) (eval expr)
	(and (seq? expr) (operator? (first expr)) (reduce #(and %1 %2) (map number? (rest expr)))) (eval expr)
	(seq? expr) 
	(loop [unsimplified expr
	       simplified (conj (map simplify (rest expr)) (first expr))]
			   (println (class simplified))
			   ;;(println unsimplified)
			   (println (str "simplified: " simplified))
			   (println (str "unsimplified: " unsimplified))
			   (if (= unsimplified simplified)
			     (if (= (first simplified) '*) (simplify-product simplified) simplified)
			     (recur simplified
		       	      (conj (map simplify (rest simplified)) (first simplified)))))
	:default expr))
