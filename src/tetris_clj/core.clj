(ns tetris-clj.core
  "This is part of a tutorial - Clojure for Software Engineers, which can be found at ..."
  (:gen-class)
  (:require [lanterna.terminal :as t]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [clojure.spec.gen.alpha :as gen]))

(def board-width 8)
(def board-depth 12)

(s/def ::positive-or-zero (s/and integer? #(>= % 0)))

;;
;; Piece definitions. `true` indicates filled square
;;
(def l-piece
  [[true false]
   [true false]
   [true true]])

(def l2-piece
  [[false true]
   [false true]
   [true true]])

(def square-piece
  [[true true]
   [true true]])

(def s-piece
  [[false true true]
   [true true false]])

(def z-piece
  [[true true false]
   [false true true]])

(def line-piece
  [[true]
   [true]
   [true]
   [true]])

(defn empty-piece
  [size-x size-y]
  (vec (for [_ (range size-y)]
         (vec (for [_ (range size-x)]
                false)))))

(defn width
  "Retrieve the width of a piece"
  [piece]
  (count (first piece)))

(defn height
  "Retrieve the height of a piece"
  [piece]
  (count piece))

(defn column
  "Retrieve a single column from a piece or board definition"
  [piece-or-board x]
  (vec (for [row piece-or-board]
         (get row x))))

(defn columns
  "Transforms a row or piece into column format (as opposed to row-format)"
  [piece-or-board]
  (for [x (range (width piece-or-board))]
    (column piece-or-board x)))

(defn no-empty-rows?
  "Do all rows have at least one filled cell?"
  [piece]
  (every? #(some true? %) piece))

(def no-empty-columns?
  (comp no-empty-rows? columns))

(defn all-rows-same-length?
  [piece]
  (let [width (width piece)]
    (every? #(= width (count %)) piece)))


(s/def ::piece-size #{1 2 3 4})
(s/def ::piece-row (s/coll-of boolean? :min-count 1 :max-count 4))
(s/def ::piece
  (s/with-gen
    ;; Spec
    (s/and (s/coll-of ::piece-row :min-count 1 :max-count 4)
           no-empty-rows?
           no-empty-columns?
           all-rows-same-length?)
    ;; Generator
    #(gen/fmap (fn [[rows cols]]
                 ;; Create a completely empty piece
                 (let [piece (empty-piece cols rows)]
                   ;; Keep looping - fill a cell at random, then check for our constraints
                   ;; Once all rows and columns are filled, exit
                   (loop [piece piece]
                     (let [row (rand-int rows)
                           col (rand-int cols)
                           piece (assoc-in piece [row col] true)]
                       (if (and (no-empty-rows? piece)
                                (no-empty-columns? piece))
                         piece
                         (recur piece))))))
               (gen/tuple (s/gen ::piece-size) (s/gen ::piece-size)))))


(def pieces
  [l-piece square-piece s-piece l2-piece z-piece line-piece])

(defn piece-coordinates
  "Produces a sequence of coordinates for each square in a piece"
  [piece]
  (for [y (range (height piece))
        x (range (width piece))]
    [x y]))



(defn rotate-right
  "Returns a new piece which has been rotated 90 degrees clockwise"
  [piece]
  (let [width (width piece)
        height (height piece)]
    (vec (for [y (range width)]
           (vec (for [x (range height)]
                  (let [target-y (- height x 1)
                        target-x y]
                     (get-in piece [target-y target-x]))))))))

(s/fdef rotate-right :args (s/cat :piece ::piece) :ret ::piece)

(def empty-row
  "Create a new row and fill it with `false` values"
  (vec (take board-width (repeat false))))

(def empty-board
  "Create a new board with no pieces. `true` represents filled squares"
  (vec (for [_ (range board-depth)]
         empty-row)))

(defn valid-x?
  "Does the x-coordinate lie within the bounds of the play board?"
  [x]
  (<= 0 x (dec board-width)))

(defn rightmost-drop-position
  "Given a piece with a certain width, how far right can it be place before it goes off the board?"
  [piece]
  (- board-width (width piece)))

(defn valid-drop-position?
  "Can the piece be dropped from this x-coordinate?"
  [piece x]
  (<= 0 x (rightmost-drop-position piece)))

(defn game-over?
  "Has any of the topmost row been filled?"
  [board]
  (boolean (some true? (first board))))


(defn board-height-at
  "What is the maximum 'height' of filled squares, on the supplied board, for a particular column "
  [board x]
  (let [column (column board x)]
    (or
      (some->> column
               (map-indexed (fn [index filled?] (when filled? index)))
               (filter identity)
               seq
               (apply min)
               (- board-depth))
      0)))

(defn drop-position-y
  "Given a board, a piece, and an x-coordinate to drop the piece from: at what height will the piece land
   given the currently-filled squares on the board?"
  [board piece drop-position-x]
  ;; For each column in the piece
  (->> (for [x (range (width piece))]
         (let [piece-column (column piece x)
               ;; Find the lowest filled cell within that piece
               column-depth (->> piece-column
                                 (map-indexed (fn [index filled?] (when filled? index)))
                                 (filter identity)
                                 (apply max))

                ;; Find the highest "peak" on the board
                board-height-at-column (board-height-at board (+ drop-position-x x))
                ;; Find the position Y for this column
                position-y (- board-depth board-height-at-column column-depth 1)]
           position-y))
       (apply min)))

(defn place-piece-on-board
  "Add a piece to the board (fill the squares on the board that correspond with the piece at the supplied
   location)"
  [board piece x y]
  (reduce
    (fn [board [piece-x piece-y]]
      (if (get-in piece [piece-y piece-x])
          (let [target-x (+ x piece-x)
                target-y (+ y piece-y)]
            (when (get-in board [target-y target-x])
              (throw (Exception. "Overlap!")))
            (if (and (>= target-x 0) (>= target-y 0))
              (assoc-in board [target-y target-x] true)
              board))
          board))
    board
    (piece-coordinates piece)))


(defn drop-piece-on-board
  "Drop a piece from location x on the board, fill the squares at the landing spot, and return the new board"
  [board piece x]
  (let [y (drop-position-y board piece x)]
    (place-piece-on-board board piece x y)))

(defn clear-lines-on-board
  "Remove any completely filled lines on the board, and replace with blank lines from the top. Return the
   new board, and the number of lines we cleared (used for scoring)"
  [board]
  (loop [board board
         y 0
         lines-cleared 0]
    (if-let [row (get board y)]
      (if (every? true? row)
        (let [board (vec (concat [empty-row]
                                 (subvec board 0 y)
                                 (subvec board (inc y) (count board))))]
          (recur board y (inc lines-cleared)))
        (recur board (inc y) lines-cleared))
      [board lines-cleared])))




;;
;; Game state
;;

(defn new-state
  "The state map tracks the game state: the current board and piece, the left-right position of the hovering
   piece, and current score"
  ([board piece position]
   {:board board
    :piece piece
    :position position
    :score 0})
  ([board piece]
   (new-state board piece 0))
  ([board]
   (new-state board (rand-nth pieces) 0))
  ([]
   (new-state empty-board (rand-nth pieces) 0)))

(defn valid-state?
  [state]
  (valid-drop-position? (:piece state) (:position state)))

(s/def ::board-row (s/coll-of boolean? :count board-width))
(s/def ::board (s/coll-of ::board-row :count board-depth))

(s/def ::position (set (range board-width)))
(s/def ::score ::positive-or-zero)
(s/def ::state (s/and (s/keys :req-un [::board ::piece ::position ::score])
                      valid-state?))

(s/def ::unfinished-game-state
  (s/with-gen
    ;; Spec
    (s/and ::state
           #(every? false? (-> % :board first)))
    ;; Generator
    #(gen/fmap (fn [generated-value]
                 (assoc-in generated-value [:board 0] empty-row))
               (s/gen ::state))))

(defn rotate-state
  "Update the game state by rotating the current piece 90 degrees"
  [state]
  (let [state (update state :piece rotate-right)]
    (if-not (valid-drop-position? (:piece state) (:position state))
      (assoc state :position (rightmost-drop-position (:piece state)))
      state)))

(s/fdef rotate-state :args (s/cat :state ::state) :ret ::state)

(defn -move-state
  "Update the game state by moving the current piece left of right"
  [movement-size state]
  (let [current-position (:position state)
        target-position (+ current-position movement-size)
        target-position (cond
                          ;; Valid - use the target position
                          (valid-drop-position? (:piece state) target-position) target-position
                          ;; Too far left - use zero
                          (< target-position 0) 0
                          ;; Too far right - use the rightmost valid position
                          :else (rightmost-drop-position (:piece state)))]

    (assoc state :position target-position)))

(s/fdef -move-state :args (s/cat :size integer? :state ::state) :ret ::state)

(def move-left (partial -move-state -1))
(def move-right (partial -move-state 1))
;; Don't move the piece, just ensure it's in a valid position
(def reset-position (partial -move-state 0))

(defn drop-state
  "Update the game state by dropping the current piece onto the board, clearing any filled lines, and
   updating the score"
  [state]
  (let [{:keys [board piece position]} state
        board (drop-piece-on-board board piece position)
        [board lines-cleared] (clear-lines-on-board board)]
    (-> state
        (assoc :board board)
        (assoc :piece (rand-nth pieces))
        (reset-position)
        (update :score #(+ % lines-cleared)))))

(s/fdef drop-state :args (s/cat :state ::state) :ret ::state)

(def actions #{:left :right :rotate :drop})
(s/def ::action actions)

(defn do-action
  [state action]
  (try
    (-> action
        (case
          :left (move-left state)
          :right (move-right state)
          :rotate (rotate-state state)
          :drop (drop-state state)))
        ;(assoc :score -1))
    (catch Exception e
      (do (printf "Could not do action %s on %s" action state)
          (.printStackTrace e)))))


(s/fdef do-action
        :args (s/cat :state ::unfinished-game-state :action ::action)
        :ret ::state)
;;
;; Rendering
;;

(defn -render-board-or-piece!
  "Render the piece / board as text"
  ([board? board left-pad & {:keys [fixed-height]}]
   (let [height (height board)
         top-pad (let [fixed-height (max height (or fixed-height 0))]
                   (- fixed-height height))]
     (dotimes [_ top-pad]
       (println ""))
     (doseq [row board]
       (doseq [_ (range left-pad)]
         (print "   "))
       (doseq [cell row]
         (let [char (cond
                      cell "X"
                      board? "."
                      :else " ")]
           (print char " ")))
       (println ""))))
  ([board? board]
   (-render-board-or-piece! board? board 0)))

(def render-board! (partial -render-board-or-piece! true))
(def render-piece! (partial -render-board-or-piece! false))

(defn render-state
  "Render the entire game state"
  [{:keys [board piece position score] :as state}]
  (println "")
  (render-piece! piece position :fixed-height 4)
  (println "")
  (render-board! board)
  (println "")
  (println "Score: " score)
  state)


;;
;; "Game"
;;

(defn play!
  []
  (let [term (t/get-terminal :text)]
    (t/start term)
    (loop [game-state (new-state)]
      (render-state game-state)
      (if (game-over? (:board game-state))
        (println "Game over!")
        (let [c (str (t/get-key-blocking term))]
          (t/clear term)
          (case c
            "x" (println "Exit")
            "i" (recur (rotate-state game-state))
            "j" (recur (move-left game-state))
            "l" (recur (move-right game-state))
            "k" (recur (drop-state game-state))
            (recur game-state)))))))


(defn -main
  [& _]
  (play!))
