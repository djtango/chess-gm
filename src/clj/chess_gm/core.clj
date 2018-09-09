(ns chess-gm.core
  (:require [clojure.spec.alpha :as s]))

(def white-king   \u2654)
(def white-queen  \u2655)
(def white-rook   \u2656)
(def white-bishop \u2657)
(def white-knight \u2658)
(def white-pawn   \u2659)
(def black-king   \u265A)
(def black-queen  \u265B)
(def black-rook   \u265C)
(def black-bishop \u265D)
(def black-knight \u265E)
(def black-pawn   \u265F)

(def glyphs
  {:black {:king   \u265A
           :queen  \u265B
           :rook   \u265C
           :bishop \u265D
           :knight \u265E
           :pawn   \u265F}
   :white {:king   \u2654
           :queen  \u2655
           :rook   \u2656
           :bishop \u2657
           :knight \u2658
           :pawn   \u2659}
   :moves {:marker \u2610
           :take-piece \u2612}})

(defn new-board
  "Function for creating a blank 8x8 board"
  []
  [[nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil]])

(defn board-with-markers
  "Function for creating a blank 8x8 board"
  []
  [[:1 :b :c :d :e :f :g :h]
   [:2 :b :c :d :e :f :g :h]
   [:3 :b :c :d :e :f :g :h]
   [:4 :b :c :d :e :f :g :h]
   [:5 :b :c :d :e :f :g :h]
   [:6 :b :c :d :e :f :g :h]
   [:7 :b :c :d :e :f :g :h]
   [:8 :b :c :d :e :f :g :h]])

(defn real-board
  []
  (let [row-of (fn [thing] (vec (take 8 (repeat thing))))
        empty-row (row-of nil)]
    [[white-rook white-knight white-bishop white-queen white-king white-bishop white-knight white-rook]
     (row-of white-pawn)
     empty-row
     empty-row
     empty-row
     empty-row
     (row-of black-pawn)
     [black-rook black-knight black-bishop black-queen black-king black-bishop black-knight black-rook] ]))

(defn find-piece [board [row col]]
  (get-in board [row col]))

(defn- map-board [f board]
  (map-indexed
    (fn f-mapped-to-rows [column-index row]
      (map-indexed (partial f column-index)
                   row))
    board))

(defn print-board [board]
  (let [black-tile (fn [i] (str "| " (or i ".") ))
        white-tile (fn [i] (str "| " (or i " ") ))
        linebr (apply str (take 33 (repeat "-")))
        print-rows (fn [rows]
                     (doseq [r rows]
                      (println linebr)
                      (apply println (concat r ["| "]))))]
    (->> board
         reverse
         (map-board
           (fn [col-idx row-idx item]
             (if (odd? (+ row-idx col-idx)) ;; this yields a checkerboard pattern
               (white-tile item)
               (black-tile item))))
       print-rows)
    (println linebr)))

(defn render-tile [empty-tile-marker point]
  (let [piece (:piece point)
        colour (:colour point)
        glyph (get-in glyphs [colour piece])]
    (str "| " (or glyph empty-tile-marker) )))

(defn visualize-board [{:as state :keys [board]}]
  (let [black-tile (partial render-tile ".")
        white-tile (partial render-tile " ")
        linebr (apply str (take 33 (repeat "-")))
        print-rows (fn [rows]
                     (doseq [r rows]
                       (println linebr)
                       (apply println (concat r ["| "]))))]
    (->> board
         reverse
         (map-board
           (fn [col-idx row-idx item]
             (if (odd? (+ row-idx col-idx)) ;; this yields a checkerboard pattern
               (white-tile item)
               (black-tile item))))
         print-rows)
    (println linebr)))

(defn see-available-moves [state point]
  (let [moves (get-moves state point)
        take-or-move
        (fn [acc to]
          (if (and (get-piece acc to)
                   (not (same-colour? (get-piece acc to) (get-piece acc point))))
            :take-piece
            :marker))]
    (-> state
        (update
          :board
          (fn [board]
            (reduce (fn [acc to]
                      (assoc-in acc
                                to
                                {:piece (take-or-move acc to)
                                 :colour :moves}))
                    board
                    moves)))
        visualize-board)))

(defn update-board [board point value]
  (assoc-in board point value))

(defn- get-piece [board point]
  (get-in board point))

(defn- add-offset [[row col] [row-offset col-offset]]
  [(+ row row-offset)
   (+ col col-offset)])

(def up-down-left-right
  "Points for up down left right"
  {:u  [ 1  0]
   :d  [-1  0]
   :l  [ 0 -1]
   :r  [ 0  1]})

(def four-diagonals
  "Points for up-right down-right down-left up-left"
  {:dl [-1 -1]
   :dr [-1  1]
   :ul [ 1 -1]
   :ur [ 1  1]})

(def all-directions
  (merge up-down-left-right
         four-diagonals))

(defn multiply-direction [direction x]
  (mapv (partial * x) direction))

(defn extend-moves
  "produces a continuous lazy seq of moves for a given direction from 1 to n times"
  [n direction]
  (mapv (partial multiply-direction direction)
        (range 1 (inc n))))

(defn directions->moves [directions n]
  "helper for generating a set of moves from a set of directions. Moves are grouped by direction [[moves...] ...]"
  (mapv (partial extend-moves n)
        (vals directions)))

(defn make-L-move [move-twice move-once]
  (let [m2 (all-directions move-twice)
        m1 (all-directions move-once)]
    (-> m2
        (add-offset m2)
        (add-offset m1))))

(def knight-moves {:ur (make-L-move :u :r)
                   :ru (make-L-move :r :u)
                   :rd (make-L-move :r :d)
                   :dr (make-L-move :d :r)
                   :ul (make-L-move :u :l)
                   :lu (make-L-move :l :u)
                   :ld (make-L-move :l :d)
                   :dl (make-L-move :d :l)})


(defn- ->black [piece]
  (assoc piece :colour :black))

(def white-king-piece {:piece :king
                       :colour :white
                       :moves (directions->moves all-directions 1)})

(def white-queen-piece {:piece :queen
                        :colour :white
                        :moves (directions->moves all-directions 8)})

(def white-rook-piece {:piece :rook
                       :colour :white
                       :moves (directions->moves up-down-left-right 8)})

(def white-bishop-piece {:piece :bishop
                         :colour :white
                         :moves (directions->moves four-diagonals 8)})

(def white-knight-piece {:piece :knight
                         :colour :white
                         :moves (directions->moves knight-moves 1)})

(def white-pawn-piece {:piece :pawn
                       :colour :white
                       :moves (directions->moves (select-keys up-down-left-right [:u]) 1)})

(def black-king-piece (->black white-king-piece))
(def black-queen-piece (->black white-queen-piece))
(def black-rook-piece (->black white-rook-piece))
(def black-bishop-piece (->black white-bishop-piece))
(def black-knight-piece (->black white-knight-piece))

;; black pawns can only go in the opposite direction of a white pawn...
(def black-pawn-piece {:piece :pawn
                       :colour :black
                       :moves (directions->moves (select-keys up-down-left-right [:d]) 1)})

(defn starting-board
  []
  (let [row-of (fn [thing] (vec (take 8 (repeat thing))))
        empty-row (row-of nil)]
    [[white-rook-piece
      white-knight-piece
      white-bishop-piece
      white-queen-piece
      white-king-piece
      white-bishop-piece
      white-knight-piece
      white-rook-piece]
     (row-of white-pawn-piece)
     empty-row
     empty-row
     empty-row
     empty-row
     (row-of black-pawn-piece)
     [black-rook-piece
      black-knight-piece
      black-bishop-piece
      black-queen-piece
      black-king-piece
      black-bishop-piece
      black-knight-piece
      black-rook-piece]]))

(defn starting-state
  []
  {:board (starting-board)
   :history []})

(defn out-of-bounds? [[row col]]
  (or (> row 7) (> 0 row)
      (> col 7) (> 0 col)))

(defn- find-first-piece [board line-of-points]
  "Returns index of either the first piece or the end of the sequence"
  (->> line-of-points
       (map (partial get-piece board))
       (take-while nil?)
       count))

(defn- same-colour? [{colour-a :colour} {colour-b :colour}]
  (= colour-a colour-b))

(defn remove-blocked-moves [board [current-point & points-in-a-line]]
  (let [end-of-line (find-first-piece board points-in-a-line)
        unobstructed-line-and-maybe-piece (take (inc end-of-line) points-in-a-line)
        last-allowed-point (get (vec points-in-a-line) end-of-line)
        maybe-first-piece (find-piece board last-allowed-point)]
    (if (and maybe-first-piece
             (same-colour? (find-piece board current-point) maybe-first-piece))
      (drop-last unobstructed-line-and-maybe-piece)
      unobstructed-line-and-maybe-piece)))

;; alternative impl
;; (defn remove-blocked-moves [board [current-point & points-in-a-line]]
;;   (let [clear-path (take-while (comp nil? (partial get-piece board)) points-in-a-line)
;;         next-point (get (vec points-in-a-line)
;;                         (count clear-path))]
;;     (if-let [next-piece (get-piece board next-point)]
;;       (if (same-colour? next-piece
;;                         (get-piece board current-point))
;;         clear-path
;;         (if next-point
;;           (conj (vec clear-path)
;;                 next-point)
;;           clear-path))
;;       clear-path)))

(defn map-2d [coll-fn element-fn coll]
  "Applies a collection function onto a nested-collection, passing element-fn to the coll-fn"
  (map (fn [nested-coll] (coll-fn element-fn nested-coll))
       coll))

(defn find-all-simple-moves [{:as state :keys [board]} point]
  (let [{:as piece
         :keys [moves]} (get-piece board point)
        moves->final-position (partial add-offset point)
        append-current-position (partial concat [point])]
    (->> moves
         (map-2d map moves->final-position)
         (map append-current-position))))

(defn remove-disallowed-moves [{:as state :keys [board]} simple-moves]
  (->> simple-moves
       (map-2d remove out-of-bounds?)
       (mapcat (partial remove-blocked-moves board))))

(defn add-diagonal-pawn-takes [board point allowed-moves]
  (let [pawn (get-piece board point)
        [row col]   (-> pawn :moves first first)]
    (->> [[row -1]
          [row  1]]
        (map (partial add-offset point))
        (filter (fn piece-to-take? [move]
                  (let [target-piece (get-piece board move)]
                    (and target-piece
                         (not (same-colour? pawn target-piece))))))
        (concat allowed-moves))))

(defn- add-pawn-first-move [board point allowed-moves]
  (let [pawn (get-piece board point)
        [pawn-row pawn-col] point
        [row col]   (-> pawn :moves first first)
        two-steps-forward [(* row 2)
                           col]
        in-starting-row? (or (and (= pawn-row 1)
                                  (= :white (:colour pawn)))
                             (and (= pawn-row 6)
                                  (= :black (:colour pawn))))]
    (if (and in-starting-row?
             (nil? (get-piece board two-steps-forward)))
      (concat allowed-moves
              [(add-offset point two-steps-forward)])
      allowed-moves)))

(defn- get-en-passant-position [{:as state :keys [board history]}]
  (let [[from to] (peek history)
        [_ to-col] to]
    (->> to
         (get-piece board)
         :moves
         first ;; get the opponent-pawn direction
         first
         (add-offset from)))) ;; move one step in opponent direction

(defn- last-move-pawn-double-step? [{:as state :keys [board history]}]
  (let [[from to] (peek history)
        piece (get-piece board to)
        [from-row _] from
        [to-row _] to]
    (and (= :pawn (:piece piece))
         (= 2 (Math/abs (- to-row from-row))))))

(defn add-en-passant [{:as state :keys [board]} point allowed-moves]
  (if (last-move-pawn-double-step? state)
    (conj allowed-moves (get-en-passant-position state))
    allowed-moves))

(defn add-pawn-moves [{:as state :keys [board]} point allowed-moves]
  (let [pawn (get-piece board point)
        [row col]   (-> pawn :moves first)]
    (->> allowed-moves
         (add-diagonal-pawn-takes board point)
         (add-pawn-first-move board point)
         (add-en-passant state point))))

(def starting-rows
  {:white 0
   :black 7})

(def starting-columns
  {:king 4
   :left-rook 0
   :right-rook 7})

(defn never-moved? [{:as state :keys [board history]} colour piece]
  (let [starting-row (starting-rows colour)
        starting-column (starting-columns piece)]
    (empty? (filter (fn [[from _]]
                      (= from [starting-row starting-column]))
                    history))))

(defn clear-path-for-castling? [board colour direction]
  (let [king-position [(starting-rows colour)
                       4]]
    (->> direction
         (extend-moves 8)
         (map (partial add-offset king-position))
         (remove out-of-bounds?)
         butlast ;; remove rook position
         (map (partial get-piece board))
         (remove nil?)
         empty?)))

(defn check-for-castling [{:as state :keys [board history]} colour direction-key allowed-moves]
  (let [direction (up-down-left-right direction-key)
        piece (get {:l :left-rook :r :right-rook} direction-key)]
    (if (and (never-moved? state colour piece)
             (clear-path-for-castling? board colour direction))
      (let [king-position [(starting-rows colour) 4]
            two-steps-from-king-to-rook (add-offset king-position
                                                    (multiply-direction direction 2))]
        (conj allowed-moves
              two-steps-from-king-to-rook))
      allowed-moves)))

(defn- add-castling [{:as state :keys [board history]} point allowed-moves]
  (let [{:keys [piece colour]} (get-piece board point)]
    (if (and (= :king piece)
             (never-moved? state colour :king))
      (->> allowed-moves
           (check-for-castling state colour :l)
           (check-for-castling state colour :r))
      allowed-moves)))

(defn add-king-moves [state point allowed-moves]
  (add-castling state point allowed-moves))

(defn handle-special-moves [{:as state :keys [board]} point allowed-moves]
  (case (->> point (get-piece board) :piece)
    :pawn (add-pawn-moves state point allowed-moves)
    :king (add-king-moves state point allowed-moves)
    allowed-moves))

(defn translate
  "given a board a point and an translation, translates the piece by a given offset"
  [board origin final-position]
  (let [;;final-position ;;(add-offset origin translation)
        piece (get-piece board origin)]
    (-> board
        (update-board origin nil)
        (update-board final-position piece))))

(s/def ::point (s/cat :row (s/int-in 0 8)
                           :col (s/int-in 0 8)))
(s/def ::move (s/cat :from ::point
                         :to ::point))
(s/def ::history (s/coll-of ::move))
(s/def ::state (s/keys :req-un [::board ::history]))

(defn get-moves [state point]
  (->> (find-all-simple-moves state point)
       (remove-disallowed-moves state)
       (handle-special-moves state point)
       ))

(defn allowed? [state move]
  (let [[from to] move]
    (contains? (set (get-moves state from))
               to)))

(defn make-move [state move]
  (let [[from to] move]
    (if (allowed? state move)
      (update state
              :board
              translate
              from
              to)
      :error)))
