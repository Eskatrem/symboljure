(defn conr[ col item ]
       (lazy-seq
               (if (seq col)
                       (cons (first col) (conr (rest col) item))
                       (list item))))


(defn find-next-permutation [lst]
  (let [my-lst (reverse lst)]
    (loop [left '()
	   first-elt (first my-lst)
	   second-elt (first (rest my-lst))
	   right (rest (rest my-lst))]
      (cond (> first-elt second-elt) (reverse (flatten (list
						      left
						      second-elt
						      first-elt
						      right)))
	    (not (= () right))
	    (recur (conr left first-elt)
		 second-elt
		 (first right)
		 (rest right))
	    :default nil))))

(defn find-nth-permutation [n lst]
  (loop [k 0
	 current-lst lst]
    (cond (= k n) current-lst
	  (= current-lst nil) nil
	  :default (recur (inc k) (find-next-permutation current-lst)))))