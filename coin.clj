(def coins [1 2 5 10 20 50 100 200])
(defn combs-ofx [total]
  (if (= total 0) 
    [[]]
    (let [
      smaller-coins (filter #(> total %) coins)]
      (map #(concat [%] (combs-ofx (- total %))) smaller-coins))))
      
(defn combs-of 
  ([total] (combs-of total []))
  ([total acc] 
    (let [
      all-coins [1 2 5 10 20 50 100 200]
      smaller-coins (filter #(> total %) all-coins)
      combs-with-addl-coin
        (fn [addl]
          (map #(concat % [addl]) acc))]
      (if (= total 0)
        acc
        (mapcat #(combs-of (- total %) (combs-with-addl-coin %)) smaller-coins)))))
      
     
    
(defproblem 391 nil
  (let [
    coins [1 2 5 10 20 50 100 200]
    combs-of 
      (fn [total]
        (if (= total 0) 
          [[]]
          (let [
            smaller-coins (filter #(> total %) coins)]
            (map #(concat [%] (combs-of (- total %))) smaller-coins))))]
    (combs-of 10)))
    
(println (combs-of 10))