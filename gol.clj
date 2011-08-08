(ns gol
  (:require [clojure.contrib.combinatorics :as cb])
  (:require [clojure.contrib.string :as st])
  (:require [clojure.contrib.str-utils :as stu]))

(defn sum [x]
  (reduce + x))
  
(defn get-cell [cord game]
  (let [
    v (get game cord)]
    (if (nil? v)
      0
      v)))

(defn neighbors [{x :x y :y :as cord} game]
  (let [
    dirs (remove #(= [0 0] %) (cb/cartesian-product [0 1 -1] [0 1 -1]))
    new-cord
      (fn [[x2 y2]] {:x (+ x x2) :y (+ y y2)})
    raw-cords
      (map new-cord dirs)
    bad-cord?
      (fn [{x :x y :y}] (or (= x -1) (= y -1)))
    cords
      (remove bad-cord? raw-cords)
    score
      (sum (map #(get-cell % game) cords))]
    score))
    
(defn new-status [{x :x y :y :as cord} game]
  (let [nb (neighbors cord game)
    res (if (= get-cell 1)
      (if (or (= nb 2) (= nb 3))
        1
        0)
      (if (= nb 3)
        1
        0))]
    res))

(defn num-alive [game]
  (sum (vals game)))
  
(defn next-game [game]
  (println "next-game")
  (let [res (reduce
    (fn [h [cord v]] (assoc h cord (new-status cord game)))
    {}
    game)]
  (println (num-alive res))
  res))
    
(defn fill-cells [cords game]
  (reduce (fn [h,c] (assoc h c 1)) game cords))
  
(def grid-size 50)
  
(defn rand-cord []
  {:x (int (rand grid-size)) :y (int (rand grid-size))})
  
(defn new-game []
  (println "newgame")
  (let [
    arr (cb/cartesian-product (range 0 grid-size) (range 0 grid-size))
    cords (map (fn [[x y]] {:x x :y y}) arr)
    blank-game
      (reduce (fn [h c] (merge h {c 0})) {} cords)
    pop-cords
      (map (fn [x] (rand-cord)) (range 0 (* grid-size grid-size 0.15)))]
    (fill-cells pop-cords blank-game)))
    

(defn print-game [game]
  (let [
    cell-str
      #(if (= 1 %) "1" " ")
    print-row
      (fn [x]
        (stu/str-join "" (map #(cell-str (get-cell {:x x :y %} game)) (range 0 grid-size))))]
    (stu/str-join "\n" (map print-row (range 0 grid-size)))))    

(def mid-games (take 10 (iterate next-game (new-game))))

(println (print-game mid-games))

(+ 2 2)