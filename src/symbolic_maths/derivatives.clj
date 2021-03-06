(ns symbolic-maths.core)

(defn derivative [expr variable]
  ;;not simplified derivative calculations 
    (cond (number? expr) 0;;(list 0)
	  (symbol? expr) (if (= variable expr) 1 0);;(list 1) (list 0))
	  :default
	  (let [op (first expr)
	       args (rest expr)
	       der #(derivative % variable)]
	   (cond (= '+ (first expr)) (conj (map #(derivative % variable) (rest expr)) '+)
		 (= '* (first expr)) (conj (let [elts (rest expr)]
					     (loop [members elts
						    res ()
						    tmp (first elts)
						    previous ()
						    next (rest elts)]
					       (cond (= () members) res
						     :default
						     (recur
						      (rest members)
						      (conj res (concat '(*) previous (keep-list (to-list (derivative tmp variable))) next)) ;;inelegant
						      (second members)
						      (conj previous tmp)
						      (rest next)))))
					   '+)
	(= '- op) (list '- (derivative (second expr) variable) (map #(derivative % variable) (rest (rest expr))))
	(= '/ op) (if (= 2 (count args)) (let [u (first args)
					       v (second args)]
					     (list '/ (list '- (list '* (der u) v) (list '* u (der v))) (list '* v v)))
			(der (list '/ (first args) (concat '(*) (rest args)))))
	(= 'sin (first expr)) (list '* (derivative (second expr) variable) (list 'cos (second expr)))
	(= 'cos op) (list '* -1 (der (second expr)) (list 'sin (second expr)))
	(= 'exp op) (list '* (der (first args)) (list 'exp (first args)))
	:default (do (println (first expr) "not handled yet...") nil)))))