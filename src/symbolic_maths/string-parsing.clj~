(ns symbolic-maths.core
  (:use [clojure.string :only (split join blank?)]))

(def operators '(= + - * /))

(def operators-char '(\= \+ \- \* \/))


(def instructions '("factor"
		    "expand"
		    "derivative"
		    "primitive"))

(def functions '(cos
		 sin
		 tan
		 cotan
		 ln
		 log
		 exp))
;;it will be always possible to add new instructions or operators later...


(defn symb [x]
  (if (char? x) (symbol (str x)) (symbol x)))


(defn concate [a b & more]
  "concatenate strings"  
  (join (concat (list a b) more)))

(defn is-outside-parenthesis
  "true if there is an op as indicated that is outside the parenthesis, false otherwise."
  [expr op]
  (loop [parenthesis 0
	 cur-expr expr
	 cur (first expr)]
    (cond (empty? cur-expr) false
	  (= cur \() (recur (inc parenthesis)
			    (rest cur-expr)
			    (second cur-expr))
	  (= cur \)) (recur (dec parenthesis)
			    (rest cur-expr)
			    (second cur-expr))
	  (and (= 0 parenthesis) (= cur op)) true
	  :default (recur parenthesis
			  (rest cur-expr)
			  (second cur-expr)))))

(defn main-operator
  "find the main operator of an expression. operator are sorted by priority"
  [expr]
  (loop [ops operators-char
	 cur (first operators-char)]
    (cond (empty? ops) nil
	  (is-outside-parenthesis expr cur) cur
	  :default (recur (rest ops) (second ops)))))


(defn split-by-main-operator
  "splits a function by operator. The operators that are taken into account are the ones outside the parenthesis."
  [expr operator]
  (loop [parenthesis-count 0
	 str expr
	 res (list (symb operator))
	 cur (first str)
	 node ""]
    (cond (empty? str) (reverse (conj res node))
	  (= cur \)) (recur (dec parenthesis-count)
			    (rest str)
			    res
			    (second str)
			    (concate node \)))
	  (= cur \() (recur (inc parenthesis-count)
			    (rest str)
			    res
			    (second str)
			    (concate node \())
	  (and (= parenthesis-count 0)
	       (= operator cur))
	  (recur 0
		 (rest str)
		 (conj res node)
		 (second str)
		 "")
	  :default (recur parenthesis-count
			  (rest str)
			  res
			  (second str)
			  (concate node cur)))))

(defn start-with
  "true if 'expr' starts with 'start', false otherwise"
  [expr start]
  (loop [main expr
	 beginning start]
    (cond (and (= beginning ()) (= (first main) \()) true
	  (empty? beginning) false
	  (= main ()) false
	  (not= (first main) (first beginning)) false
	  :default (recur (rest main) (rest beginning)))))

(defn close-parenthesis
  "given an expression that starts by a '(', closes that parenthesis, by making sure the sub-parenthesis are taken into account"
  [expr]
  (if (not= \( (first expr)) nil ;;should throw an exception
      (loop [res ()
	     parenthesis-count 0
	     cur-str (rest expr)]
	(cond (and (= (first cur-str) \)) (= 0 parenthesis-count)) res
	      (= (first cur-str) \)) (recur (concat res (list \)))
					    (dec parenthesis-count)
					    (rest cur-str))
	      (= (first cur-str) \() (recur (concat res (list \())
					    (inc parenthesis-count)
					    (rest cur-str))
	      (= () cur-str) nil
	      ;;should throw an exception here: incorrect expression
	      :default (recur (concat res (list (first cur-str)))
			      parenthesis-count
			      (rest cur-str))))))


(defn split-function-and-args
  "transforms a string of the form func(...) into a list (func (...)). expr has to start with 'func('"
  [expr func]
  (println (class func))
  (println func)
  (let [func-str (str func)
	func-symb (symbol func)]
    (loop [res ()
	   current expr
	   start func-str
	   cur (first expr)
	   cur-func (first func-str)]
      (cond
       (empty? current) (conj (reverse res) func-symb)
       (empty? start) (recur (conj res cur)
			     (rest current)
			     nil
			     (second current)
			     nil)
       (= cur cur-func) (recur res
			       (rest current)
			       (rest start)
			       (second current)
			       (second start))
       :default nil))))
      

(defn find-node
  "find main node of an expression. Could be an operator or a function."
  [expr]   
  (if (= \( (first expr)) (find-node (close-parenthesis expr))
      (let [op (main-operator expr)]
	(if (nil? op) (let [func (first (keep #(if (start-with expr %) % nil) (map str functions)))]
			(if (nil? func) nil (split-function-and-args expr func)))
	    (split-by-main-operator expr op)))))
	

(defn find-blocks
  "find the main blocks of a mathematical expression, respects operator priority"
  [expr]
  )



(defn string-to-list
  "converts a string into a list that clojure can evaluate"
  [command]
  )

(defn remove-whitespaces
  "remove whitespaces of a string"
  [s]
  (loop [res ""
	 rem s
	 cur (str (first s))]
    (cond (empty? rem) res
	  (blank? cur) (recur res (rest rem) (str (second rem)))
	  :default (recur (join (list res cur)) (rest rem) (str (second rem))))))



(defn irreductible? 
  "true if a function doesnt contain neither operator, neither parenthesis, neither coma, false otherwise"
  [expr]
  (if (not (string? expr)) true 
      (let [chars '(\+ \- \* \/ \\ \( \) \[ \] \, \^)]
	(loop [s expr
	       cur (first s)]
	  (cond (empty? s) true
		(is-in? chars cur =) false
		:default (recur (rest s) (second s)))))))

(defn remove-extra-parenthesis
  "remove extra parenthesis from an expression. E.g (a+b) ->a+b ((a+b)) a+b"
  [expr]
  (if (not= \( (first expr)) expr
      (loop [chars (rest expr)
	     par 1
	     cur (first chars)]
	(cond
	 (and (= 0 par) (not (empty? (rest chars)))) expr
	 (= \( cur) (recur (rest chars) (inc par) (second chars))
	 (= \) cur) (recur (rest chars) (dec par) (second chars))
	 (and (empty? (rest chars)) (= 0 par))
	 (reduce concate (reverse (rest (reverse (rest expr)))))
	 :default (recur (rest chars) par (second chars))))))

(defn kill-extra-parenthesis
  "remove all the extra parenthesis wrapping an expression"
  [expr]
  (if (= expr (remove-extra-parenthesis expr)) expr
      (kill-extra-parenthesis (remove-extra-parenthesis expr))))

(defn split-resp-parenthesis
  "split an expression, only when the splitters are outside parenthesis"
  [expr splitters]
  (loop [res ()
	 s expr
	 cur (first s)
	 parenthesis-count 0
	 cur-str ""
	 splitter nil]
    (cond (empty? s) (concat res (list cur-str))
	  (and (= 0 parenthesis-count) (is-in? splitters cur =) (nil? splitter))
	  (recur (concat res (list cur-str) (list (symbol (str cur))) )
		 (rest s)
		 (second s)
		 0
		 ""
		 (read-string (str cur)))
	  (and (= 0 parenthesis-count) (is-in? splitters cur =))
	  (recur (concat res (list cur-str) (list (symbol (str cur))))
		 (rest s)
		 (second s)
		 0
		 ""
		 (read-string (str cur)))
	  (= \( cur)
	  (recur res
		 (rest s)
		 (second s)
		 (inc parenthesis-count)
		 (concate cur-str \()
		 splitter)
	  (= \) cur)
	  (recur res
		 (rest s)
		 (second s)
		 (dec parenthesis-count)
		 (concate cur-str \))
		 splitter)
	  :default
	  (recur res
		 (rest s)
		 (second s)
		 parenthesis-count
		 (concate cur-str cur)
		 splitter))))

(defn get-function [expr]
  "extract func in 'func(...)'"
  (loop [res ""
	 s expr
	 cur (first expr)]
    (cond (empty? s) nil
	  (= \( cur) res
	  :default (recur (concate res cur) (rest s) (second s)))))

(defn get-core
  "get ... in 'func(...)'"
  [expr]
  (loop [res ""
	 in-func false
	 s expr
	 cur (first s)
	 parenthesis-count 0]
    (cond
     (and (not in-func) (not= cur \())
     (recur res false (rest s) (second s) 0)
     (and (not in-func) (= cur \())
     (recur res true (rest s) (second s) 1)
     (and (= cur \)) (= 1 parenthesis-count)) res
     (= cur \()
     (recur (concate res \() true (rest s) (second s) (inc parenthesis-count))
     (= cur \))
     (recur (concate res \)) true (rest s) (second s) (dec parenthesis-count))
     (empty? s) nil ;;shouldnt happen...
     :default
     (recur (concate res cur) true (rest s) (second s) parenthesis-count))))

(defn parse-string
  "converts a string into a list, respecting the parenthesis..."
  [expr]
  ;;(println expr)
  (let [expr-wo-p (kill-extra-parenthesis expr)
	s (remove-whitespaces expr-wo-p)]
    (if (irreductible? s) (symb s)
	(let [op (main-operator s)]
	  (if (not (nil? op))
	    (let [splits (split-by-main-operator s op)
		  members (rest splits)]
	      ;(println (class (first members)))
	      ;(println (first members))
					;(map #(println (class %)) splits)
	    
	      (conj (map parse-string members) (symb op)))
	    (let [func (symb (get-function s))
		  core (get-core s)]
		  ;;core (if (seq? tmp) tmp (list tmp))]
	      ;(println (concate "func = " (str func)))
	      ;;(println core)
	      (conj (list (parse-string core)) func)))))))
    
  