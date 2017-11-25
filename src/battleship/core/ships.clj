(ns battleship.core.ships
  (:require [clojure.set :refer [intersection]]
            [battleship.core.boards :refer :all]))

(def ship-types
  #{:carrier :battleship :cruiser :destroyer :submarine})

(def ship-sizes
  {:carrier 5
   :battleship 4
   :cruiser 3
   :destroyer 2
   :submarine 1})

(def ship-counts
  {:carrier 1
   :battleship 1
   :cruiser 1
   :destroyer 2
   :submarine 2})

(def starting-ships
  (flatten
    (map (fn [[ship-type n]]
           (repeat n ship-type))
         ship-counts)))

(defn get-ship-coords
  [ships]
  (map :coords ships))

(defn new-ship
  [ship-type coords]
  {:type ship-type
   :coords coords})

(defn overlap?
  [ship-a ship-b]
  (not (empty? (intersection (set ship-a) (set ship-b)))))

(defn increments-of?
  [n coll]
  (reduce (fn [prev curr]
            (if (= n (- curr prev))
              curr
              (reduced false)))
          coll))

(defn horizontal?
  [sorted-coords]
  (and
    (apply = (map first sorted-coords))
    (increments-of? 1 (map second sorted-coords))))

(defn vertical?
  [sorted-coords]
  (and
    (apply = (map second sorted-coords))
    (increments-of? 1 (map first sorted-coords))))

(defn valid-ship-coords?
  [ship-type coords]
  (let [sorted-coords (sort coords)]
    (and
      (= (ship-sizes ship-type) (count coords))
      (or (horizontal? sorted-coords) (vertical? sorted-coords)))))

(defn mark-ship
  [board ship pos]
  (reduce (fn [b coord]
            (assoc-in b coord ship))
          board
          pos))

; Ship placement helpers ---
(defn lift
  [coll]
  (reduce (fn [acc sub-coll]
            (apply conj acc sub-coll))
          []
          coll))

(def row-partitions
  (memoize
    (fn [partition-size board]
      (reduce (fn [acc row]
                (apply conj acc (partition partition-size 1 row)))
              []
              board))))

(def col-partitions
  (memoize
    (fn [partition-size board]
      (distinct
        (lift
          (apply map
                 (fn [& col]
                   (partition partition-size 1 col))
                 board))))))

(defn possible-placements
  [board-size ship-len]
  (let [row-placements (row-partitions ship-len (to-positions (empty-board board-size)))
        col-placements (col-partitions ship-len (to-positions (empty-board board-size)))]
    (distinct (concat row-placements col-placements))))
; ---

(defn rand-ships
  ([]
   (rand-ships *board-size*))
  ([board-size]
   (reduce
     (fn [placed ship-type]
       (let [all-pos (possible-placements board-size (ship-sizes ship-type))]
         (loop [pos (rand-nth all-pos)]
           (if (empty? (filter (partial overlap? pos) (get-ship-coords placed)))
             (conj placed (new-ship ship-type pos))
             (recur (rand-nth all-pos))))))
     #{}
     starting-ships)))

(defn ship-board
  ([ships]
   (ship-board ships *board-size*))
  ([ships board-size]
   (reduce (fn [new-board {:keys [type coords]}]
             (mark-ship new-board type coords))
           (empty-board board-size)
           ships)))

