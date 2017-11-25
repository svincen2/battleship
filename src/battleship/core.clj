(ns battleship.core
  (:require [battleship.core.boards :refer :all]
            [battleship.core.ships :refer :all]
            [battleship.ui.cli :refer :all]
            [battleship.ui.cli.effects :refer [println-block-delayed]]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

; TODO
; - Game:
;   - check sink
;   - check win

(defn- new-player
  [plr-name]
  {:name plr-name
   :boards {:player nil :enemy (empty-board)}
   :ships #{}})

(defn new-user
  [user-name]
  (assoc (new-player user-name) :type :user))

(defn new-cpu
  [cpu-name]
  (assoc (new-player cpu-name) :type :cpu))

(defn user?
  [player]
  (= :user (:type @player)))

(defn set-ships!
  [player ships]
  (swap! player assoc :ships ships)
  (swap! player assoc-in [:boards :player] (ship-board ships)))

(defn- record-outcome!
  [player enemy [row col] outcome]
  (swap! player assoc-in [:boards :enemy row col] outcome)
  (swap! enemy assoc-in [:boards :player row col] outcome)
  outcome)

(defn hit?
  [coord board]
  (ship-types (get-in board coord)))

(defn avail-target?
  [coord board]
  (= :empty (get-in board coord)))

(defn fire-at!
  [[row col :as coord] player enemy]
  (if (hit? coord (get-in @enemy [:boards :player]))
    (do
      (println-block-delayed
        32
        500
        "HIT!"
        (str (:name @player) " hit " (:name @enemy) "'s "
             (name (get-in @enemy [:boards :player row col]))))
      (record-outcome! player enemy coord :hit))
    (do
      (println-block-delayed 32 500 "MISS!")
      (record-outcome! player enemy coord :miss))))

(defn rand-coord
  ([player]
   (rand-coord player *board-size*))
  ([player board-size]
   (let [prev-shots (get-in @player [:boards :enemy])]
     (loop [row (rand-int board-size)
            col (rand-int board-size)]
       (if (avail-target? [row col] prev-shots)
         [row col]
         (recur (rand-int board-size) (rand-int board-size)))))))

(defn ship-at
  [coord player]
  (let [ships (:ships @player)]
    (first (filter (fn [ship]
                     (let [coords (:coords ship)]
                       (> (count (filter #(= coord %) coords)) 0)))
                   ships))))

(defn sunk?
  [ship player]
  (let [coords (:coords ship)
        board (get-in @player [:boards :player])
        ship-state (map #(get-in board %) coords)
        hits-left (count (filter #(not= :hit %) ship-state))]
    (= 0 hits-left)))

(def ^:private turn-delay 500)

(defn player-turn!
  [player enemy]
  (println (str \newline (:name @player) "'s turn"))
  (let [get-coord (if (user? player) input-coord #(rand-coord player))]
    (when (user? player)
      (println \newline "ships")
      (println (player-board->string (get-in @player [:boards :player]))))
    (println \newline "enemy")
    (println (enemy-board->string (get-in @player [:boards :enemy])))
    (let [coord (get-coord)]
      (println-block-delayed
        32
        500
        (str "firing at " (game-coord->user-coord coord)))
      (let [result (fire-at! coord player enemy)]
        (when (= :hit result)
          (let [ship (ship-at coord enemy)]
            (if (sunk? ship enemy)
              (println-block-delayed
                32
                500
                (str (:name @player) " sunk " (:name @enemy) "'s " (name (:type ship)))))))
        (Thread/sleep turn-delay)))))

(defn winner?
  [player enemy]
  (let [player-ships-left (filter ship-types (flatten (get-in @player [:boards :player])))
        enemy-ships-left (filter ship-types (flatten (get-in @enemy [:boards :player])))]
    (or
      (if (empty? player-ships-left) enemy)
      (if (empty? enemy-ships-left) player))))

(defn -main
  [& args]
  (let [user (atom (new-user (input-player-name)))
        cpu (atom (new-cpu "charlie"))]
    (println)
    (println-block-delayed
      32
      500
      (str "well " (:name @user) " you're about to feel the white hot rage,")
      "of a twelve year old boy."
      "ever hear of a game called BATTLESHIP?"
      "well you'd better get to know it guy."
      "place your ships.")
    (set-ships! user (input-ships))
    (set-ships! cpu (rand-ships))
    (println)
    (println-block-delayed
      32
      500
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
    (println)
    (loop [player user
           enemy cpu]
      (if-let [winner (winner? user cpu)]
        (println-block-delayed 32 500  (:name @winner) "wins! Game over")
        (do
          (player-turn! player enemy)
          (recur enemy player))))))

