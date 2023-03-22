(ns battleship.core
  (:require [battleship.core.boards :as boards]
            [battleship.core.ships :as ships]
            [battleship.ui.cli :as ui.cli]
            [battleship.ui.cli.effects :as cli.effects]
            #_[clojure.pprint :refer [pprint]])
  (:gen-class))

; TODO
; - Show the ship sizes in the ship board legend
; - Show enemies sunken/remaining ships (list w/ counts, not actual positions)
; - Clear screen after player turn?
; - Option to skip preamble
; - Handle bad coord like '3c'
; - How to show sunken ships better?
; - Show how many ships are left

(defn ^:private new-player
  [plr-name]
  {:name plr-name
   :boards {:player nil :enemy (boards/empty-board)}
   :ships #{}})

(defn ^:private new-user
  [user-name]
  (assoc (new-player user-name) :type :user))

(defn ^:private new-cpu
  [cpu-name]
  (assoc (new-player cpu-name) :type :cpu))

(defn ^:private user?
  [player]
  (= :user (:type @player)))

(defn ^:private set-ships!
  [player ships]
  (swap! player assoc :ships ships)
  (swap! player assoc-in [:boards :player] (ships/ship-board ships)))

(defn ^:private record-outcome!
  [player enemy [row col] outcome]
  (swap! player assoc-in [:boards :enemy row col] outcome)
  (swap! enemy assoc-in [:boards :player row col] outcome)
  outcome)

(defn ^:private hit?
  [coord board]
  (ships/ship-types (get-in board coord)))

(defn ^:private sunk?
  [ship player]
  (let [coords (:coords ship)
        board (get-in @player [:boards :player])
        ship-state (map #(get-in board %) coords)
        hits-left (count (filter #(not= :hit %) ship-state))]
    (= 0 hits-left)))

(defn ^:private avail-target?
  [coord board]
  (= :empty (get-in board coord)))

(defn ^:private rand-coord
  ([player]
   (rand-coord player boards/*board-size*))
  ([player board-size]
   (let [prev-shots (get-in @player [:boards :enemy])]
     (loop [row (rand-int board-size)
            col (rand-int board-size)]
       (if (avail-target? [row col] prev-shots)
         [row col]
         (recur (rand-int board-size) (rand-int board-size)))))))

(defn ^:private ship-at
  [coord player]
  (let [ships (:ships @player)]
    (first (filter (fn [ship]
                     (let [coords (:coords ship)]
                       (> (count (filter #(= coord %) coords)) 0)))
                   ships))))

(defn ^:private fire-at!
  [coord player enemy]
  (record-outcome!
    player
    enemy
    coord
    (if (hit? coord (get-in @enemy [:boards :player]))
      :hit
      :miss)))

;; ---- player turn output ----

(def ^:private print-char-delay-ms 32)

(def ^:private print-line-delay-ms 500)

(def ^:private println-block-delayed
  (partial cli.effects/println-block-delayed
           print-char-delay-ms
           print-line-delay-ms))

(defn ^:private print-player-boards
  [player]
  (println \newline "ships")
  (println (ui.cli/player-board->string (get-in @player [:boards :player])))
  (println \newline "enemy")
  (println (ui.cli/enemy-board->string (get-in @player [:boards :enemy]))))

(defn ^:private print-firing-at-coord
  [coord]
  (println-block-delayed
    (str "firing at " (ui.cli/game-coord->user-coord coord))))

(defn ^:private print-hit-outcome
  [player enemy coord]
  (let [ship (ship-at coord enemy)
        verb (if (sunk? ship enemy) " sunk " " hit ")]
    (println-block-delayed
      "HIT!"
      (str (:name @player) verb (:name @enemy) "'s " (name (:type ship))))))

(defn ^:private print-outcome
  [player enemy coord outcome]
  (if (= :hit outcome)
    (print-hit-outcome player enemy coord)
    (println-block-delayed "MISS!")))

;; ---- END player turn output ----

(def ^:private turn-delay 500)

(defn ^:private player-turn!
  [player enemy]
  (println (str \newline (:name @player) "'s turn"))
  (let [get-coord (if (user? player) ui.cli/input-coord #(rand-coord player))]
    (when (user? player)
      (print-player-boards player))
    (let [coord (get-coord)]
      (print-firing-at-coord coord)
      (print-outcome player enemy coord (fire-at! coord player enemy))
      (Thread/sleep turn-delay))))

(defn ^:private ships-left
  [player]
  (filter ships/ship-types (flatten (get-in @player [:boards :player]))))

(defn winner?
  [player enemy]
  (cond
    (empty? (ships-left player)) enemy
    (empty? (ships-left enemy)) player
    :else nil))

(defn ^:private game-loop
  [user cpu]
  (loop [player user
         enemy cpu]
    (if-let [winner (winner? user cpu)]
      (println-block-delayed (:name @winner) "wins! Game over")
      (do
        (player-turn! player enemy)
        (recur enemy player)))))

(defn ^:private print-place-ships-preamble
  [user]
  (println)
  (println-block-delayed
    (str "well " (:name @user) " you're about to feel the white hot rage,")
    "of a twelve year old boy."
    "ever hear of a game called BATTLESHIP?"
    "well you'd better get to know it guy."
    "place your ships."))

(defn ^:private print-start-game-preamble
  []
  (println)
  (println-block-delayed
    "satisfied with you're placements?  I hope so."
    "now that we've gotten that out of the way we can get down to business."
    "you see the truth is, I don't like you."
    "I just don't."
    "so here's what's gonna happen."
    "I'm gonna bomb you're ships back to hell."
    "then, I'm gonna eat you're family."
    "but since I'm a nice guy,"
    "since I'm a cool dude,"
    "I'll let you go first."
    "but it doesn't matter."
    "because I will destroy you!")
  (println))

(defn ^:private start-game
  []
  (let [user (atom (new-user (ui.cli/input-player-name)))
        cpu (atom (new-cpu "charlie"))]
    (print-place-ships-preamble user)
    (set-ships! user (ui.cli/input-ships))
    (set-ships! cpu (ships/rand-ships))
    (print-start-game-preamble)
    (game-loop user cpu)))

(defn -main
  [& _args]
  (start-game))
