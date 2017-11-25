(ns battleship.ui.cli
  (:require [ansi.core :refer [ansi-str]]
            [battleship.core.ships :refer [new-ship
                                           ship-board
                                           ship-sizes
                                           starting-ships
                                           valid-ship-coords?]]
            [clojure.string :as string]))

; ---- Rendering ----

(defn block-str
  [width color]
  (let [block-char \u2588]
    (ansi-str (apply str (repeat width block-char)) color)))

(defn colored-cell
  [color]
  (let [block (if (nil? color) "___" (block-str 3 color))]
    (str block "|")))

(defn top-row-str
  [row-size]
  (apply str
         (let [max-len (count (str row-size))]
           (for [i (range row-size)]
             (let [label (str (+ 1 i))
                   len (count label)]
               (str (if (= 0 i) "   " "_")
                    (apply str (repeat (- max-len len) "_"))
                    (ansi-str label :blue)
                    "_"))))))

(defn row->string
  [color-map index row]
  (apply str
         (ansi-str (char (+ (int \A) index)) :blue)
         " |"
         (map #(colored-cell (color-map %)) row)))

(defn legend->str
  [legend]
  (apply str
         (interpose \newline
                    (map (fn [[k v]]
                           (str (block-str 1 v) " " (name k)))
                         legend))))

(defn board->string
  [legend board]
  (let [num-cols (count (first board))]
    (str (top-row-str num-cols)
         \newline
         (apply str
                (interpose \newline
                           (map-indexed (partial row->string legend) board)))
         \newline
         (legend->str legend))))

(def enemy-board-legend
  {:hit :red
   :miss :white})

(def enemy-board->string (partial board->string enemy-board-legend))

(def ship-board-legend
  {:carrier :magenta
   :battleship :green
   :submarine :blue
   :cruiser :yellow
   :destroyer :cyan})

(def ship-board->string (partial board->string ship-board-legend))

(def player-board->string (partial board->string (merge ship-board-legend
                                                        enemy-board-legend)))

(defn render-ships
  [ships]
  (ship-board->string (ship-board ships)))

; ---- End Rendering ----

; ---- User input ----

(defn user-coord->game-coord
  [coord-str]
  (let [normalized (string/upper-case coord-str)
        row (first normalized)
        col (apply str (rest normalized))]
    [(- (int row) (int \A)) (- (Integer/parseInt (str col)) 1)]))

(defn game-coord->user-coord
  [[row col]]
  (str (char (+ (int \A) row)) (str (+ 1 col))))

(defn expand-coord-range
  [[[r1 c1] [r2 c2]]]
  (let [r-diff (Math/abs (- r2 r1))
        c-diff (Math/abs (- c2 c1))
        min-r (min r1 r2)
        min-c (min c1 c2)]
    (cond
      (not= 0 r-diff) (mapv (fn [x] [(+ min-r x) min-c]) (range (+ 1 r-diff)))
      (not= 0 c-diff) (mapv (fn [x] [min-r (+ min-c x)]) (range (+ 1 c-diff))))))

(defn user-coord-range->game-coords
  [user-range]
  (let [parts (string/split user-range #"-")
        game-coords (map user-coord->game-coord parts)]
    (if (= 1 (count game-coords))
      game-coords
      (expand-coord-range game-coords))))

(defn input-ship
  [ship-type]
  ; TODO - Verify ship-type
  (let [size (ship-sizes ship-type)]
    (loop []
      (print (str "input " (name ship-type) " coordinates (size " size "): "))
      (flush)
      (let [coords (user-coord-range->game-coords (read-line))]
        (if (valid-ship-coords? ship-type coords)
          (new-ship ship-type coords)
          (do
            (println "Invalid ship coordinates")
            (recur)))))))

(defn input-ships
  []
  (let [ships (atom #{})] ; Using some state here so we can show a ship-board after each placement
    (doall (map (fn [ship-type]
                  (println (str \newline (render-ships @ships) \newline))
                  (let [ship (input-ship ship-type)]
                    (swap! ships conj ship)))
                starting-ships))
    (println \newline (render-ships @ships))
    @ships))

(defn input-player-name
  []
  (println "hello.  who are you?")
  (print "> ")
  (flush)
  (read-line))

(defn input-coord
  []
  (print "enter coordinate: ")
  (flush)
  (user-coord->game-coord (read-line)))

; ---- End User input ----

