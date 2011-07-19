(ns main
  (:require [clojure.contrib.string :as s])
  (:require [clojure.contrib.combinatorics :as cb]))
  
(defn ae [a b] (if (= a b) (println "equal") (println "not equal" a b)))

(defmacro retlist [l] `~l)


(defmacro retlistapp [l] `(conj ~l 4))



(def problems {})

(defn current_problem [] 31)

(def problems (atom {}))

(defn add-problem [n answer body]
  (let [
    ph {:n n :answer answer :body body}
    mh {n ph}
    swap-func #(merge % mh)]
    (swap! problems swap-func)))
    
(defn run-problem [n]
  (let [
    ph (get @problems n)
    body (:body ph)
    eval-answer (eval body)
    exp-answer (:answer ph)]
    (if (= eval-answer exp-answer)
      (println "problem" n "correct" eval-answer)
      (println "problem" n "incorrect!!!!" "expected" exp-answer "got" eval-answer))))

(defmacro defproblem-old [n answer body] 
  `(if (= ~n (current_problem)) (println "problem" `n ~body)))
  
(defmacro defproblem [n answer body] 
  `(add-problem ~n ~answer (quote ~body)))
  
(defn factor? [n,mult] (if (= (mod mult n) 0) true false))

(defn factors [mult] 
  (let [
    bottom
      (filter #(factor? % mult) (range 2 (+ (Math/sqrt mult) 1)))
    top
      (map #(/ mult %) bottom)
    all
      (concat bottom top)]
  (distinct all)))

(defn sudm [x]
  (if (= x []) 0 (+ (first x) (sudm (rest x)))))

(defn sum [x]
  (reduce + x))

(defproblem 1 233168
  (let [mults (filter #(or (factor? 3 %) (factor? 5 %)) (range 1 1000))]
    (sum mults)))

(defn fib
  ([] (fib 1 2))
  ([a b] (lazy-seq (cons (+ a b) (fib b (+ a b))))))

(defproblem 2 4613732 
  (let [fibs (take-while #(> 4000000 %) (fib))]
    (sum (filter even? fibs))))
    
(defn prime? [n]
  (if (= n 2) 
    true
    (= (factors n) [])))

(defproblem 3 6857
  (let [nums (reverse (factors 600851475143))]
    (first (filter prime? nums))))
    
(defn most [list]
  (map #(get list %) (range 0 (- (count list) 1))))
  
(defn nstrlist [n]
  (seq (str n)))

(defn palindrome? [s]
  (if (<= (count s) 1)
    true
    (if (= (first s) (last s))
      (palindrome? (rest (most s)))
      false)))
      
(defn one? [l] (= (count l) 1))
      
(defn combinationss [a b]
  (if (or (= a []) (= b []))
    []
    (if (one? a)
      (map #(concat a [%]) b)
      (if (one? b)
        (map #(concat b [%]) a)))))
        
(defn combinations [a b]
  (cb/cartesian-product a b))
        
        
(defn combinationsx [a b]
  (if (or (= a []) (= b []))
    []
    (concat
      (map #(concat [(first a)] [%]) b)
      (combinationsx (rest a) b))))

(defn product [l]
  (if (empty? l) 
    1
    (* (first l) (product (rest l)))))

(defproblem 44 nil
  (let [combs (combinations (range 100 1000) (range 100 1000))
        nums (map product combs)
        num_lists (map nstrlist nums)]
    (first (reverse (filter palindrome? num_lists)))))

(defn all-factors? [fs,n]
  (every? #(factor? % n) fs))

(defproblem 5 232792560
  (let [nums (map #(* 19 17 13 11 7 5 %) (range 1 1000))]
    (first (filter #(all-factors? (reverse (range 2 21)) %) nums))))

(defn square [n] (* n n))

(defproblem 6 25164150
  (let [sum-squares (sum (map square (range 1 101)))
        square-sums (square (sum (range 1 101)))]
      (- sum-squares square-sums)))
      
(defn primes [] (filter prime? (range 2 1000000000000000)))

(defn primes-under [n]
  (take-while #(> n %) (primes)))

(defproblem 7 104743 
  (last (take 10001 (primes))))


(defn p8-nums []
  (map #(Integer. (str %)) (seq "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")))

(defn windows [l,window-size]
  (if (> window-size (count l))
    []
    (concat 
      [(take window-size l)]
      (windows (rest l) window-size))))

(defn product [x]
  (if (= x []) 1 (* (first x) (product (rest x)))))

(defproblem 8 40824
  (let [ws (windows (p8-nums) 5)
        products (map product ws)]
      (last (sort products))))
      
(defn square? [n]
  (= (mod (Math/sqrt n) 1) 0))
  
(defn hypo [a,b]
  (Math/sqrt (+ (square a) (square b))))
      
(defn pyth-triplesd []
  (let [all-poss (combinations (range 1 500) (range 1 500))
        trips (map (fn [x] (concat x [(hypo (nth x 0) (nth x 1))])) all-poss)
        is-trip #(square? (last %))
        good-trips (filter is-trip trips)]
      (filter #(= 1000 (sum %)) good-trips)))
      
(defn pyth-triples []
  (let [all-poss (combinations (range 1 500) (range 1 500))
        trips (map (fn [x] (concat x [(hypo (nth x 0) (nth x 1))])) all-poss)
        is-trip #(= (mod (last %) 1) 0)
        good-trips (filter is-trip trips)]
      (filter #(> (nth % 1) (nth % 0)) good-trips)))
      
(defn pyth-triples2 []
  (let [all-poss (combinations (range 1 10) (range 1 10))
        trips (map (fn [x] (concat x [(hypo (nth x 0) (nth x 1))])) all-poss)]
      trips))

(defproblem 9 318750000
  (product (first (filter #(= (sum %) 1000) (pyth-triples)))))
  

(defproblem 10 nil
  (sum (primes-under 2000000)))
  
(defproblem 11 70600674
  (let [all [8 2 22 97 38 15 0 40 0 75 4 5 7 78 52 12 50 77 91 8
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 4 56 62 0
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 3 49 13 36 65
52 70 95 23 4 60 11 42 69 24 68 56 1 32 56 71 37 2 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 3 45 2 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 2 62 12 20 95 63 94 39 63 8 40 91 66 49 94 21
24 55 58 5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 9 75 0 76 44 20 45 35 14 0 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 3 80 4 62 16 14 9 53 56 92
16 39 5 42 96 35 31 47 55 58 88 24 0 17 54 24 36 29 85 57
86 56 0 48 35 71 89 7 5 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 5 94 47 69 28 73 92 13 86 52 17 77 4 89 55 40
4 52 8 83 97 35 99 16 7 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 3 46 33 67 46 55 12 32 63 93 53 69
4 42 16 73 38 25 39 11 24 94 72 18 8 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 4 36 16
20 73 35 29 78 31 90 1 74 31 49 71 48 86 81 16 23 57 5 54
1 70 54 71 83 51 54 69 16 92 33 48 61 43 52 1 89 19 67 48]
  spot-indexes (fn [i] 
    (concat 
      [[i (+ i 1) (+ i 2) (+ i 3)]]
      [[i (+ i 20) (+ i 40) (+ i 60)]]
      [[i (+ i 21) (+ i 42) (+ i 63)]]
      [[i (+ i 19) (+ i 38) (+ i 57)]]))
  all-indexes
    (reduce concat (map spot-indexes (range 0 400)))
  line
    (fn [l] (map #(get all %) l))
  all-lines
    (map line all-indexes)
  good-lines
    (filter #(not (= (last %) nil)) all-lines)
  products
    (map product good-lines)]
  (last (sort products))))
  
(defn triangle-nums []
  (reductions + 1 (range 2 100000000000)))
  
(defproblem 12 76576500
  (let [
    good? #(>= (count (factors %)) 498)]
  (first (filter good? (triangle-nums)))))

(defproblem 13 5537376230
  (let [nums [37107287533902102798797998220837590246510135740250
46376937677490009712648124896970078050417018260538
74324986199524741059474233309513058123726617309629
91942213363574161572522430563301811072406154908250
23067588207539346171171980310421047513778063246676
89261670696623633820136378418383684178734361726757
28112879812849979408065481931592621691275889832738
44274228917432520321923589422876796487670272189318
47451445736001306439091167216856844588711603153276
70386486105843025439939619828917593665686757934951
62176457141856560629502157223196586755079324193331
64906352462741904929101432445813822663347944758178
92575867718337217661963751590579239728245598838407
58203565325359399008402633568948830189458628227828
80181199384826282014278194139940567587151170094390
35398664372827112653829987240784473053190104293586
86515506006295864861532075273371959191420517255829
71693888707715466499115593487603532921714970056938
54370070576826684624621495650076471787294438377604
53282654108756828443191190634694037855217779295145
36123272525000296071075082563815656710885258350721
45876576172410976447339110607218265236877223636045
17423706905851860660448207621209813287860733969412
81142660418086830619328460811191061556940512689692
51934325451728388641918047049293215058642563049483
62467221648435076201727918039944693004732956340691
15732444386908125794514089057706229429197107928209
55037687525678773091862540744969844508330393682126
18336384825330154686196124348767681297534375946515
80386287592878490201521685554828717201219257766954
78182833757993103614740356856449095527097864797581
16726320100436897842553539920931837441497806860984
48403098129077791799088218795327364475675590848030
87086987551392711854517078544161852424320693150332
59959406895756536782107074926966537676326235447210
69793950679652694742597709739166693763042633987085
41052684708299085211399427365734116182760315001271
65378607361501080857009149939512557028198746004375
35829035317434717326932123578154982629742552737307
94953759765105305946966067683156574377167401875275
88902802571733229619176668713819931811048770190271
25267680276078003013678680992525463401061632866526
36270218540497705585629946580636237993140746255962
24074486908231174977792365466257246923322810917141
91430288197103288597806669760892938638285025333403
34413065578016127815921815005561868836468420090470
23053081172816430487623791969842487255036638784583
11487696932154902810424020138335124462181441773470
63783299490636259666498587618221225225512486764533
67720186971698544312419572409913959008952310058822
95548255300263520781532296796249481641953868218774
76085327132285723110424803456124867697064507995236
37774242535411291684276865538926205024910326572967
23701913275725675285653248258265463092207058596522
29798860272258331913126375147341994889534765745501
18495701454879288984856827726077713721403798879715
38298203783031473527721580348144513491373226651381
34829543829199918180278916522431027392251122869539
40957953066405232632538044100059654939159879593635
29746152185502371307642255121183693803580388584903
41698116222072977186158236678424689157993532961922
62467957194401269043877107275048102390895523597457
23189706772547915061505504953922979530901129967519
86188088225875314529584099251203829009407770775672
11306739708304724483816533873502340845647058077308
82959174767140363198008187129011875491310547126581
97623331044818386269515456334926366572897563400500
42846280183517070527831839425882145521227251250327
55121603546981200581762165212827652751691296897789
32238195734329339946437501907836945765883352399886
75506164965184775180738168837861091527357929701337
62177842752192623401942399639168044983993173312731
32924185707147349566916674687634660915035914677504
99518671430235219628894890102423325116913619626622
73267460800591547471830798392868535206946944540724
76841822524674417161514036427982273348055556214818
97142617910342598647204516893989422179826088076852
87783646182799346313767754307809363333018982642090
10848802521674670883215120185883543223812876952786
71329612474782464538636993009049310363619763878039
62184073572399794223406235393808339651327408011116
66627891981488087797941876876144230030984490851411
60661826293682836764744779239180335110989069790714
85786944089552990653640447425576083659976645795096
66024396409905389607120198219976047599490197230297
64913982680032973156037120041377903785566085089252
16730939319872750275468906903707539413042652315011
94809377245048795150954100921645863754710598436791
78639167021187492431995700641917969777599028300699
15368713711936614952811305876380278410754449733078
40789923115535562561142322423255033685442488917353
44889911501440648020369068063960672322193204149535
41503128880339536053299340368006977710650566631954
81234880673210146739058568557934581403627822703280
82616570773948327592232845941706525094512325230608
22918802058777319719839450180888072429661980811197
77158542502016545090413245809786882778948721859617
72107838435069186155435662884062257473692284509516
20849603980134001723930671666823555245252804609722
53503534226472524250874054075591789781264330331690]]
    (sum nums)))
    
(defn seq-14 [start]
  (let [
    next-num
      (if (odd? start)
        (+ (* 3 start) 1)
        (/ start 2))]
    (if (= next-num 1)
      [start 1]
      (concat [start] (seq-14 next-num)))))
      


      
(defproblem 149 nil
  (let [
    raw
      (map seq-14 (range 1 100000))
    counts
      (map #(concat [(count %)] [(first %)]) raw)
    sorted
      (sort-by first counts)]
    (time (last sorted))))
  
(defn number-word [n]
  (let [
    base {1 "one" 2 "two" 3 "three" 4 "four" 5 "five" 6 "six" 7 "seven" 8 "eight" 9 "nine" 10 "ten" 
          11 "eleven" 12 "twelve" 13 "thirteen" 14 "fourteen" 15 "fifteen" 16 "sixteen" 17 "seventeen" 18 "eighteen" 19 "nineteen" 20 "twenty"
          30 "thirty" 40 "forty" 50 "fifty" 60 "sixty" 70 "seventy" 80 "eighty" 90 "ninety" 100 "onehundred" 1000 "onethousand"}
    hundreds (int (Math/floor (mod (/ n 100.0) 10)))
    hundreds-rem (- n (* hundreds 100))
    tens (- n (mod n 10))
    ones (- n tens)]
    (if (base n)
      (seq (base n))
    (if (> n 100)
      (if (= (mod n 100) 0)
         (concat (get base hundreds) "hundred")
         (concat (get base hundreds) "hundredand" (number-word hundreds-rem)))
      (concat (base tens) "" (number-word ones))))))

(defproblem 17 21124
 (count (flatten (map number-word (range 1 1001)))))
 
(defn call-repeatedly [obj f n]
  (if (= n 0)
    obj
    (call-repeatedly (f obj) f (- n 1))))
    
(defn single-by [l,uniq-f,max-f]
  (let [
    grouped (group-by uniq-f l)
    best
      (fn [vs]
        (last (sort-by max-f vs)))]
    (map best (vals grouped))))
    


(defproblem 18 1074
  (let [
    tri [[75]
        [95 64]
        [17 47 82]
        [18 35 87 10]
        [20 4 82 47 65]
        [19 1 23 75 3 34]
        [88 2 77 73 7 63 67]
        [99 65 4 28 6 16 70 92]
        [41 41 26 56 83 40 80 70 33]
        [41 48 72 33 47 32 37 16 94 29]
        [53 71 44 65 25 43 91 52 97 51 14]
        [70 11 33 28 77 73 17 78 39 68 17 57]
        [91 71 52 38 17 14 91 43 58 50 27 29 48]
        [63 66 4 68 89 53 67 30 73 16 69 87 40 31]
        [4 62 98 27 23 9 70 98 73 93 38 53 60 4 23]]
    next-paths
      (fn [path]
        (let [
          nodes (path :nodes)
          last-node (last (path :nodes))
          left-index last-node
          right-index (+ last-node 1)
          next-row (tri (count (path :nodes)))
          left-num (next-row left-index)
          right-num (next-row right-index)]
          [{:nodes (concat nodes [left-index]) :sum (+ (path :sum) left-num)}
           {:nodes (concat nodes [right-index]) :sum (+ (path :sum) right-num)}]))
    all-next-paths
      (fn [paths]
        (flatten (map next-paths paths)))
    pruned-next-paths
      (fn [paths]
        (let [
          all 
            (all-next-paths paths)
          pruned
            (single-by all #(last (% :nodes)) #(% :sum))]
          pruned))
    first-path
      {:nodes [0] :sum 75}
   final-paths
     (call-repeatedly [first-path] pruned-next-paths 14)]
   ((last (sort-by #(% :sum) final-paths)) :sum)))
   
(defn reduce-while2 [f init while-f]
  (if (while-f init)
    (cons (f init) (reduce-while2 f (f init) while-f))))

(defn reduce-while [f init while-f]
  (loop [curr init acc []]
    (if (not (while-f curr))
      acc
      (recur (f curr) (cons (f curr) acc)))))
   
(defproblem 19 171
  (let [
    months {1 31 3 31 4 30 5 31 6 30 7 31 8 31 9 30 10 31 11 30 12 31}
    leap? 
      (fn [dt]
        (if (not (= (mod (dt :year) 4) 0))
          false
        (if (= (mod (dt :year) 400) 0)
          true
        (if (= (mod (dt :year) 100) 0)
          false
          true))))
    month-days
      (fn [dt] 
        (if (= (dt :month) 2)
          (if (leap? dt) 29 28)
          (months (dt :month))))
    end-of-month?
      #(= (% :day) (month-days %))
    end-of-year?
      #(and (= 12 (% :month)) (= 31 (:day %)))
    next-weekday
      (fn [day] 
        (if (= day 7) 1 (inc day)))
    month-start-sunday?
      #(and (= (% :weekday) 1) (= (% :day) 1) (>= (% :year) 1901))
    next-day
      (fn [dt]
        (merge
          (if (end-of-year? dt)
            {:day 1 :month 1 :year (inc (dt :year))}
          (if (end-of-month? dt)
            {:day 1 :month (inc (dt :month)) :year (dt :year)}
            (merge dt {:day (inc (dt :day))})))
          {:weekday (next-weekday (dt :weekday))}))
    start-date
      {:year 1900 :month 1 :day 1 :weekday 2}
    end-date
      {:year 2000 :month 12 :day 31}
    date-equal?
      (fn [a b]
        (and (= (a :year) (b :year)) (= (a :month) (b :month)) (= (a :day) (b :day))))
    days
      (reduce-while next-day start-date #(not (date-equal? % end-date)))]
    (count (filter month-start-sunday? days))))
        
(load-file "67.clj")   

(defproblem 67 7273
  (let [
    tri tri-67
    next-paths
      (fn [path]
        (let [
          nodes (path :nodes)
          last-node (last (path :nodes))
          left-index last-node
          right-index (+ last-node 1)
          next-row (tri (count (path :nodes)))
          left-num (next-row left-index)
          right-num (next-row right-index)]
          [{:nodes (concat nodes [left-index]) :sum (+ (path :sum) left-num)}
           {:nodes (concat nodes [right-index]) :sum (+ (path :sum) right-num)}]))
    all-next-paths
      (fn [paths]
        (flatten (map next-paths paths)))
    pruned-next-paths
      (fn [paths]
        (let [
          all 
            (all-next-paths paths)
          pruned
            (single-by all #(last (% :nodes)) #(% :sum))]
          pruned))
    first-path
      {:nodes [0] :sum 59}
   final-paths
     (call-repeatedly [first-path] pruned-next-paths 99)]
   ((last (sort-by #(% :sum) final-paths)) :sum)))
   
(defn fact [n]
  (if (= 1 n)
    n
    (* n (fact (dec n)))))
    
(defn digits [n]
  (if (> 10 n)
    [(int n)]
    (concat
      (digits (Math/floor (/ n 10)))
      [(mod n 10)])))
      
      
(defproblem 209 nil
  (digits (fact 100)))



(defn factor-hash []
  (reduce
    (fn [h,n] (merge h {n (factors n)}))
    {}
    (range 1 100)))

(defn amicable? [n]
  (let [
    factor-sum (inc (sum (factors n)))
    other-sum (inc (sum (factors factor-sum)))]
    (and 
      (not (= factor-sum other-sum))
      (not (prime? n)) 
      (= other-sum n))))

(defproblem 21 31626
  (sum (filter amicable? (range 2 10000))))
  
(defn letter-value [s]
  (let [
    h {"A" 1 "B" 2 "C" 3 "D" 4 "E" 5 "F" 6 "G" 7 "H" 8 "I" 9 "J" 10 "K" 11 "L" 12 "M" 13 "N" 14 "O" 15 "P" 16 "Q" 17 "R" 18 "S" 19 "T" 20 "U" 21 "V" 22 "W" 23 "X" 24 "Y" 25 "Z" 26}]
    (h s)))

(defn letters [s]
  (map str (seq s)))
    
(defn string-value [s]
  (sum (map letter-value (letters s))))

(defn include? [arr,n]
  (not (empty? (filter #(= n %) arr))))
  


  
(defproblem 22 871198282
  (let [
    unsorted (s/split #"," (slurp "names.txt"))
    sorted (sort unsorted)
    total-val
      (fn [i] 
        (* (inc i) (string-value (nth sorted i))))]
    (sum (map total-val (range 0 (count sorted))))))

(let [
    deficient? 
      (fn [n]
        (> n (inc (sum (factors n)))))
    abundant? 
      (fn [n]
        (< n (inc (sum (factors n)))))
    abundant
      (filter abundant? (range 1 16000))]
    (count abundant))
    
(defproblem 23 nil
  (let [
    deficient? 
      (fn [n]
        (> n (inc (sum (factors n)))))
    abundant? 
      (fn [n]
        (< n (inc (sum (factors n)))))
    abundant
      (filter abundant? (range 1 28124))
    sums
      (map + (combinations abundant abundant))
    all
      (filter #(not (include? sums %)) (range 1 28124))]
    (sum all)))
    

    
(defproblem 31 73682
  (let [
    all-coins [1 2 5 10 20 50 100 200]
    fresh-naked-solution {1 0 2 0 5 0 10 0 20 0 50 0 100 0 200 0}
    
    fresh-solution
      (fn [r]
        (merge fresh-naked-solution {:remaining r}))
        
    coin-present?
      (fn [sol c] 
        (> (get sol c) 0))
        
    finished?
      (fn [sol] (zero? (:remaining sol)))
        
    smallest-coin
      (fn [sol]
        (let [
          present-coins (filter #(coin-present? sol %) all-coins)]
          (if (empty? present-coins)
            999
            (first present-coins))))
            
    fill-with-ones
      (fn [sol]
        (let [
          exist (get sol 1)
          new-one (+ exist (:remaining sol))]
          (merge sol {1 new-one :remaining 0})))
    
    add-coin
      (fn [sol c]
        (let [
          new-sum (inc (get sol c))
          new-rem (- (:remaining sol) c)
          h {c new-sum :remaining new-rem}]
          (if (= c 1)
            (fill-with-ones sol)
            (merge sol h))))
        
    next-solutions
      (fn [sol]
        (let [
          smallest (smallest-coin sol)
          next-coins-inner (filter #(>= smallest %) all-coins)
          next-coins (filter #(>= (:remaining sol) %) next-coins-inner)]
          (if (= (:remaining sol) 0)
            []
            (map #(add-coin sol %) next-coins))))
          
    ultimate-solutions
      (fn [sols]
        (let [
          finished (filter finished? sols)
          progress (filter #(not (finished? %)) sols)
          prog-next (flatten (map next-solutions progress))
          all-next (concat finished prog-next)]
          (if (zero? (count progress))
            finished
            (recur all-next))))
          
    solve-rem
      (fn [r]
        (ultimate-solutions [(fresh-solution r)]))]
          
    (count (solve-rem 200))))
    
(defn -main [& args]   
  (println "main-start")
  (run-problem 31) 
  (println "main over"))   
    
    
    
    
    
    
    
    