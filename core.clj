; TicTacToe on the command line
(ns tictactoe-cli.core)

; Setting the starting board
(def starting-board
  "The default board, for when the game starts"
  [1 2 3 4 5 6 7 8 9])

(defn triples
  "These are all the lines of the board that could win."
  [board]
  (concat
   (partition-all 3 board)                       ; the rows of the board
   (list
    (take-nth 3 board)                           ; first column
    (take-nth 3 (drop 1 board))                  ; second column
    (take-nth 3 (drop 2 board))                  ; third column
    (take-nth 4 board)                           ; top-left to bottom-right diagonal
    (take-nth 2 (drop-last 2 (drop 2 board)))))) ; top-right to bottom-left diagonal

(defn full-board?
  "Makes sure that every spot has been filled."
  [board]
  (every? #{:x :o} board))

#_(defn display-board
  "This displays the state of the current board."
  [board]
  (let [board (map
               #(if (keyword? %)
                  (subs (str %) 1)
                  %)
               board)]
    (println (nth board 0) (nth board 1) (nth board 2))
    (println (nth board 3) (nth board 4) (nth board 5))
    (println (nth board 6) (nth board 7) (nth board 8))))

(defn display-board
  "This displays the state of the current board"
  [board]
  (let [board (map
               #(if (keyword? %)
                  (clojure.string/upper-case (name %))
                  %)
               board)]
    (println (nth board 0) "|" (nth board 1) "|" (nth board 2))
    (println "---------")
    (println (nth board 3) "|" (nth board 4) "|" (nth board 5))
    (println "---------")
    (println (nth board 6) "|" (nth board 7) "|" (nth board 8))))

#_(defn player-name
  "Converts player input to either o or x."
  [player]
  (subs (str player) 1))

(name :x)

(defn player-name
  "Converts player input to either o or x."
  [player]
  (clojure.string/upper-case (name player)))

;; Winner detection
(defn triple-winner?
  "If a line contains three of the same player, return the player."
  [triple]
  (if (every? #{:x} triple)
    :x
    (if (every? #{:o} triple)
      :o)))

(triple-winner? [1 2 3])
;; => nil
(triple-winner? [:x 2 3])
;; => nil
(triple-winner? [:x :x 3])
;; => nil
(triple-winner? [:x :x :x])
;; => :x
(triple-winner? [:o 2 3])
;; => nil
(triple-winner? [:o :o 3])
;; => nil
(triple-winner? [:o :o :o])
;; => :o

#_(defn triple-winner?
  "If a line contains three of the same player, return the player."
  [triple]
  (cond
    (every? #{:x} triple) :x
    (every? #{:o} triple) :o))

(defn winner?
  "Returns winner if there is one, otherwise nil"
  [board]
  (first
   (filter #{:x :o} (map triple-winner? (triples board)))))

;; Taking turns in the game
(def player-sequence
  (cycle [:x :o]))

#_(take 10 (cycle [:x :o]))
;; => (:x :o :x :o :x :o :x :o :x :o)

(defn next-move
  [board]
  (let [keyboard-input
        (try
          (. Integer parseInt (read-line))
          (catch Exception e nil))]
    (if (some #{keyboard-input} board)
      keyboard-input
      nil)))

(defn take-turn
  "Asks the players to make a move and informs them when they make an incorrect move."
  [player board]
  (println (str (player-name player) ":") "Select the move you would like to make (press a number between 1 and 9 then press enter)")
  (loop [move (next-move board)]
    (if move
      (assoc board (dec move) player)

      ;; else
      (do
        (println (str (player-name player) ":") "The move you entered is unavailable, please select a different move")
        (recur (next-move board))))))

(defn play-game
  "This is the game loop. We iterate through the player sequence until there is a winner or the board is full."
  [starting-board player-sequence]
  (loop [board starting-board
         player-sequence player-sequence]
    (let [winner (winner? board)]
      (println "Current board:")
      (display-board board)
      (cond
        winner              (println "Player " (player-name winner) " wins!")
        (full-board? board) (println "The game is a draw.")
        :else
        (recur
         (take-turn (first player-sequence) board)
         (rest player-sequence))))))

(play-game starting-board player-sequence)