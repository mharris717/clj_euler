(defn ae [a b] (if (= a b) (println "equal" a) (println "not equal" a b)))

(defn div-even? 
  ([n d] (zero? (rem n d)))
  ([d] #(div-even? % d)))

(def nums (iterate inc 0))
(def s-for 
  (for [x nums :when (div-even? x 4)] (inc x)))
  
(def s-man
  (map inc (filter #(div-even? % 4) nums)))
  
(def s-man-partial
  (map inc (filter (div-even? 4) nums)))
  
(def s-man-thrush
  (->> (filter #(div-even? % 4) nums) (map inc)))
  
(println (first nums))

(println 
  (take 10 (for [x (range 100000000) y (range 1000000) :while (< y x)] [x y])))
  
(println
  (take 10 (for [x nums :let [y (inc x)] :when (> x 10)] (* y 3))))
  
(ae 
  [1 2 2 4]
  (mapcat #(list % (* % 2)) [1 2]))
  
(ae [1 :x 2 :x 3] (interpose :x [1 2 3]))

(ae [[1 2 3] [2 3 4]] (partition 3 1 [1 2 3 4]))

(ae [[0 1 2] [3 4 5] [6 7]] (partition-by #(int (/ % 3)) (range 8)))

(ae [1 2 10 4] (replace {3 10} [1 2 3 4]))

(ae 0 (when-first [a nums] a))

(ae {1 3 2 4} (zipmap [1 2] [3 4]))

(ae [2 3] (map #(inc %) [1 2]))

(declare fib)

(defn fib* [n]
  (if (>= 2 n)
    1
    (+ (fib (dec n)) (fib (- n 2)))))
    
(def fib (memoize fib*))

(println (fib 100))

(defmacro memoized-funcx [name args body]
  `(defn ~(str name "*") ~args ~body))
  
(defmacro memoized-func [name f]
  `(do 
  (def ~name (memoize ~f))))

(println (macroexpand-1 
`(memoized-func fib2
  (fn [n] (if (>= 2 n)
    1
    (+ (fib2 (dec n)) (fib2 (- n 2))))))))
    
(memoized-func fib2
  (fn [n] (if (>= 2 n)
    1
    (+ (fib2 (dec n)) (fib2 (- n 2))))))
    

(println (fib2 1000))

    
`(clojure.core/defn fib2* [user/n] (if (clojure.core/>= 2 user/n) 1 (clojure.core/+ (user/fib2 (clojure.core/dec user/n)) (user/fib2 (clojure.core/- user/n 2)))))