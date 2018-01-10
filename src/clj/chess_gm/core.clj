(ns chess-gm.core)

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
  (let [black (fn [i] (str "| " (or i ".") ))
        white (fn [i] (str "| " (or i " ") ))
        linebr (apply str (take 33 (repeat "-")))
        print-rows (fn [rows]
                     (doseq [r rows]
                      (println linebr)
                      (apply println (concat r ["| "]))))]
    (->> board
         reverse
         (map-board
           (fn [col-idx row-idx item]
             (if (odd? (+ row-idx col-idx))
               (white item)
               (black item))))
       print-rows)
    (println linebr)))

(defn- update-board [board point value]
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

(defn extend-moves
  "produces a continuous lazy seq of moves for a given direction from 1 to n times"
  [n [row col]]
  (map (fn [idx]
         [(* idx row)
          (* idx col)])
       (range 1 (inc n))))

(defn directions->moves [directions n]
  "helper for generating a set of moves from a set of directions. Moves are grouped by direction [[moves...] ...]"
  (mapv (partial extend-moves n)
        (vals directions)))

(def white-king-piece {:piece :king
                       :colour :white
                       :moves (directions->moves all-directions 1)})

(def black-king-piece (update white-king-piece :colour :black))

(def white-queen-piece {:piece :queen
                        :colour :white
                        :moves (directions->moves all-directions 8)})

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

(defn out-of-bounds? [[row col]]
  (or (> row 8) (> 0 row)
      (> col 8) (> 0 col)))

(defn- find-first-piece [board line-of-points]
  "Returns index of either the first piece or the end of the sequence"
  (->> line-of-points
       (map (partial get-piece board))
       (take-while nil?)
       count))

(defn- same-colour? [{colour-a :colour} {colour-b :colour}]
  (= colour-a colour-b))

(defn blocked? [board [current-point & points-in-a-line]]
  (let [end-of-line (find-first-piece board points-in-a-line)
        unobstructed-line-and-maybe-piece (take end-of-line points-in-a-line)
        last-allowed-point (get points-in-a-line end-of-line)
        maybe-first-piece (find-piece board last-allowed-point)]
    (if (and maybe-first-piece
             (same-colour? (find-piece board current-point) maybe-first-piece))
      (drop-last unobstructed-line-and-maybe-piece)
      unobstructed-line-and-maybe-piece)))

(defn map-2d [coll-fn element-fn coll]
  "Applies a collection function onto a nested-collection, passing element-fn to the coll-fn"
  (map (fn [nested-coll] (coll-fn element-fn nested-coll))
       coll))

(defn remove-disallowed-moves [board point]
  (let [{:as piece
         :keys [moves]} (get-piece board point)
        moves->final-position (partial add-offset point)
        append-current-position (partial concat [point])]
    (->> moves
         (map-2d map moves->final-position)
         (map append-current-position)
         (map-2d remove out-of-bounds?)
         (mapcat (partial blocked? board)))))

(defn translate
  "given a board a point and an translation, translates the piece by a given offset"
  [board origin translation]
  (let [final-position (add-offset origin translation)
        piece (get-piece board origin)]
    (-> board
        (update-board origin nil)
        (update-board final-position piece))))

(defn get-moves [board point]
  (-> board
      (remove-disallowed-moves point)))
