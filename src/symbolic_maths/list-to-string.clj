(ns symbolic-maths.core
  (:use [clojure.string :only (split join blank?)]))

(def symbols '( + - * /))

(defn higher-priority
  "true if op1 has a greater or equal priority as op2, false otherwise"
  [op1 op2]
  (if (= op1 op2) true
      (loop [s symbols
	     cur (first s)]
	(cond (empty? s) true
	      (= cur op2) false
	      (= cur op1) true
	      :default (recur (rest s) (second s))))))

(defn list-to-string [expr & add-parenthesis]
  (cond (not (coll? expr)) (str expr)
	(operator? (first expr)) (join (str (first expr)) (map list-to-string (rest expr)))
	:default (concate (str (first expr)) "(" (join "," (map list-to-string (rest expr))) ")")))