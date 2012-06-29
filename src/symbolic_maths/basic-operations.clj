(ns symbolic-maths.basic-operations)

(defn expand-product [f]
  (if (or (not (seq? f)) (not= '* (first f))) (throw "input has to be a product!")
      nil))

(defn expand [f]
  (cond (symbol? f) f
	(number? f) f
	(vector? f) (vec (map expand f))
	(not= '* (first f)) f
	:default (expand-product f)))