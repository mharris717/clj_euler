(ns bowling)

(defn sum [x]
  (reduce + x))  
  
(defn map-with-rest 
  ([f s] (map-with-rest f s []))
  ([f s acc]
    (if (empty? s)
      acc
      (recur f (rest s) (concat acc [(f (first s) (rest s))])))))
  
(defn strike? [f]
  (= (first f) 10))
  
(defn spare? [f]
  (= (sum f) 10))
      
(defn frame-over? [f]
  (if (or (= (count f) 2) (strike? f))
    true
    false))
    
(defn in-progress-frame [fs]
  (if (frame-over? (last fs))
    nil
    (last fs)))
    
(defn add-roll [fs r]
  (let [prog (in-progress-frame fs)]
    (if prog
      (concat (butlast fs) [(concat prog [r])])
      (concat fs [[r]]))))
      
(defn frame-score [f,rfs]
  (let [follow (flatten rfs)]
    (if (strike? f)
      (+ 10 (sum (take 2 follow)))
    (if (spare? f)
      (+ 10 (sum (take 1 follow)))
      (sum f)))))
      
(defn frame-scores [fs]
  (map-with-rest frame-score fs))
  
(defn scores [fs]
  (take 10 (reductions (fn [a b] (sum [a b])) (frame-scores fs))))

(defn score [fs]
  (last (scores fs)))
  
(defn construct-game [rolls]
  (reduce add-roll [] rolls))
  
(println (map-with-rest (fn [a b] (count b)) (range 1 10)))

(println (scores (construct-game [10 8 2 10 10 10 10 10 10 10 10 10 9])))
    
