(ns battleship.core.boards)

; Let's try this dynamic stuff out, see how it feels
(def ^:dynamic *board-size* 10)

(defn empty-board
  ([]
   (empty-board *board-size*))
  ([board-size]
   (vec
     (for [i (range board-size)]
       (vec
         (for [j (range board-size)]
           :empty))))))

(def to-positions-flat
  (memoize
    (fn [board]
      (for [i (range (count board))
            j (range (count (first board)))]
        [i j]))))

(def to-positions
  (memoize
    (fn [board]
      (for [i (range (count board))]
        (for [j (range (count (first board)))]
          [i j])))))

