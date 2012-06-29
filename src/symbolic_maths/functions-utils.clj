(ns symbolic-maths.functions-utils)

;; '** for operator ^ (somehow it is forbiden...)

(defn int? [x]
  "true if x is an integer, false otherwise. Must be able to take assumptions on x into account"
  (integer? x)) ;;for now...

(defn contains-variable? [f x]
  "true if f is a function of x"
  (let [cont? #(contains-variable? % x)]
    (cond (number? f) false
	  (symbol? f) (= f x)
	  (coll? f) (some cont? f)
	  :default false)))
	
(defn polynom? [f var]
  "true if f is a polynom, false otherwise"
  (let [pol? #(polynom? % var)]
  (cond ;;(empty? f) true
	(number? f) true
	(symbol? f) true
	(coll? f)
	(let [a (first f)]
	  (cond (= '/ a) (and (pol? (second f)) (not (contains-variable? (second (rest f)) var)))
		(= a '**) (or (and (pol? (second f)) (int? (nth f 2)) (> (nth f 2) 0))
			      (and (not (contains-variable? (second f) var)) (not (contains-variable? (nth f 2) var))))
		;;still incomplete. need to avoid case like (** number (g var))
		(some #(= % a) '(+ - *)) (every? pol? (rest f))
		:default (not (contains-variable? f var))))
	
	
	:default false)))