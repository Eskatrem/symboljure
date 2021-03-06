(ns symbolic-maths.core)

;;utility
(defn contains-type? [type col]
  "returns true if col contains at least one element that verifies type, and false otherwise."
  (loop [elt (first col)
	 lst col]
    (cond (= lst ()) false
	  (type elt) true
	  :default (recur (first (rest lst)) (rest lst)))))

;;utility
(defn is-in? [col x]
  (loop [elts col]
    (cond (= () elts) false
	  (= (first elts) x) true
	  :default (recur (rest elts)))))

;;utility
(defn contains-similar? [col x comparator]
  (loop [elts col
	 elt (first col)]
    (cond (= () elts) false
	  (comparator x elt) true
	  :default (recur (rest elts) (first (rest elts))))))

;;utility
(defn merge-list [a b]
  (loop [res a
	 to-merge b]
    (if (= to-merge ())
      res
      (recur (conj res (first to-merge)) (rest to-merge)))))

;;utility
(defn count-occurencies [x expr]
  (loop [current-expr expr
	count 0]
  (cond (= () current-expr) (list count x)
	(= (first current-expr) x) (recur (rest current-expr) (inc count))
	:default (recur (rest current-expr) count))))

;;utility
(defn get-each-count [expr]
  (loop [current-expr expr
	 list-unique ()
	 tmp (first expr)
	 res ()]
    (cond (= () current-expr) (reverse res)
	  (= () list-unique) (recur (rest current-expr)
				    (cons tmp list-unique)
				    (first (rest current-expr))
				    (cons (count-occurencies tmp expr) res))
	  (not (reduce #(or %1 %2) (map #(= tmp %) list-unique)))
	  (recur (rest current-expr)
		 (cons tmp list-unique)
		 (first (rest current-expr))
		 (cons (count-occurencies tmp expr) res))
	  :default (recur (rest current-expr)
			  list-unique
			  (first (rest current-expr))
			  res))))
		 

(defn categorize [expr criterion]
"split expr into two list: first the ones that satisfy criterion, and then the ones that don't"
(loop [tmp expr
       ok ()
       not-ok ()
       elt (first expr)]
  (cond (= () tmp) (list ok not-ok)
	(criterion elt) (recur (rest tmp)
			       (cons elt ok)
			       not-ok
			       (first (rest tmp)))
	:default (recur (rest tmp)
			ok
			(cons elt not-ok)
			(first (rest tmp))))))

(defn expand-sum [expr]
  "expands a sum, like '(+ a (+ b c)) becomes '(+ a b c),
the function has to be recursive, like '(+ a (+ b (+ c d)))
becomes '(+ a b c d), on the other hand , sub-elements that
are lists not starting by '+ should not be expanded."
  ;;TODO: make that function recursive
  (loop [current-elt (first (rest expr))
	 current-expr (rest expr)
	 res ()]
    (cond (= () current-expr) (cons '+ (reverse res))
	  (or (not (list? current-elt)) (not (= '+ (first current-elt))))
	      (recur (first (rest current-expr))
				     (rest current-expr)
				     (conj res current-elt))
	      :default (recur (first (rest current-expr))
			      (rest current-expr)
			      (merge-list res (rest current-elt))))))

(defn set-multiply-by-one [expr]
  (loop [elts (rest expr)
	 elt (first (rest expr))
	 res ()]
    (cond (= () elts) (conj (reverse res) (first expr))
	  ;if elt is a number simply skip it. Otherwise replace elt by (* 1 elt)
	  (or (number? elt) (and (list? elt) (contains-type? number? elt)))
	  (recur (rest elts)
		 (first (rest elts))
		 (conj res elt))
	  :default (recur (rest elts)
			  (first (rest elts))
			  (conj res (list '* 1 elt))))))

(defn are-same [x y]
  "returns true if x and y are the same modulo permutations i.e they contain the same elements, disregarding in what order."
					;TODO: this implementations is currently wrong, e.g it will return true for x = (a a b) and y = (a b b)
					;will have to correct that later, for now the only decent stuff I came up with.
  (do ;;(println x)
      ;;(println y)
  (if (not (= (count x) (count y))) false
      (loop [elts x]
	(cond (= elts ()) true
	      (not (is-in? y (first elts))) false
	      :default (recur (rest elts)))))))

(defn split-expr [expr criterion]
  "transform a list of type: '(func ...) into '(func (stuff that verify criterion) (stuf that dont verify criterion))"
  ;;function to be used like (split-expr '(* 1 2 x x y 3) number?)
  (let [func (first expr),
	core (rest expr),
	cats (categorize core criterion)]
    (list func (first cats) (first (rest cats)))))

(defn simplify-simplifyable [expr]
  "takes an expression of the form '(func (numbers) (non-numbers)) and returns (func (func numbers) (non-numbers))"
  (let [func (first expr),
	simplifyable (first (rest expr)),
	remaining (first (rest (rest expr)))]
    (list func (eval (cons func simplifyable)) remaining)))

  
(defn split-members [exprs criterion]
  (let [rests (map #(rest %) exprs)]
    (map #(categorize % criterion) rests)))


;;(aggregate-same '((1 (x)) (1 (x)) (1 (y))) '(x) '+ '*)
(defn aggregate-same [lst elt func-1 func-2]
  "returns in the results the aggregated list of all the elements of lst that are 'like' elt"
  (loop [cur-lst lst
	 cur-elt (first lst)
	 res-1 ()
	 res-2 ()]
    (cond (= () cur-lst)  (list (conj res-1 func-1) (first res-2))
			   ;;(conj (list (conj res-1 func-1) (first res-2)) func-2))
	  (not (are-same elt (first (rest cur-elt))))
	  (recur (rest cur-lst)
		 (first (rest cur-lst))
		 res-1
		 res-2)
	  :default
	  (recur (rest cur-lst)
		 (first (rest cur-lst))
		 (conj res-1 (first cur-elt))
		 (conj res-2 (first (rest cur-elt)))))))
	 

(defn simplify-splits [expr func]
  "deprecated?"
  (map #(cons (eval (cons func (first %))) (rest %)) expr))



;;TODO for this function:
;;replace is-in? by something like contains-similar?
;;(instead of having an exact match, we use a function like
;;to know if there is a similar element (modulo permutations))
;;(simplify-product '((1 (+ 1 x)) (2 (+ x 1)) (1 (x)) (2.5 (x))))
(defn simplify-product [prods]
  (loop [res ()
	 already-seen ()
	 all-prods prods
	 elt (first prods)]
    (cond (= all-prods ()) res
	  ;;in this case we already have one of the kind of elt.
	  ;;so we simply skip it.
	  (contains-similar? already-seen (first (rest elt)) are-same)
	  (recur res
		 already-seen
		 (rest all-prods)
		 (first (rest all-prods)))
	  :default
	  ;;in this case, the current element has not been reached yet,
	  ;;so we find all of its kind, aggregate them together and add them to res
	  (recur (conj res (aggregate-same prods (first (rest elt)) '+ '*))
		 (conj already-seen (first (rest elt)))
		 (rest all-prods)
	    	 (first (rest all-prods))))))

(defn factor-sum [expr]
  "factors a sum. Assumes elements like 'x have been replaced by '(* 1 x) beforehand."
  (let [expanded-expr (expand-sum expr)
	with-prods (set-multiply-by-one expanded-expr)
	trimmed-prods (map #(list (rest %)) (rest with-prods))
	factored-expr (do
			(println trimmed-prods)
			(simplify-product trimmed-prods))]
    factored-expr))


(defn simplify-sum [expr]
  "simplifies a list representing a sum (adding up the numbers together and factoring the same quantities), assume that elements like 'x have already been converted into '(* 1 x)"
  (if (not (= '+ (first expr))) nil ;should throw an exception here...
      (let [rest-expr (rest expr),
	    splits (categorize rest-expr number?),
	    total-sum (apply + (first splits)),
	    count-symbols (get-each-count (first (rest splits))),
	    symbols (map #(cons '* %) count-symbols)
	    simple-symbols (map #(if (= (first (rest %)) 1) (first (rest (rest %))) %) symbols),
	    tmp-res1 (if (= 0 total-sum) (cons '+ simple-symbols)
			 (cons '+ (cons total-sum simple-symbols)))]
	tmp-res1)))
		       
