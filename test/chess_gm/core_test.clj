(ns chess-gm.core-test
  (:require [clojure.test :refer :all]
            [chess-gm.core :as sut]))

(deftest find-piece
  (testing "For an 8x8 board 1 indexed in the first column:"
    (is (= :1 (sut/find-piece (sut/board-with-markers) [0 0]))
        "First column and first row should be :1")
    (is (= :2 (sut/find-piece (sut/board-with-markers) [1 0]))
        "Second row, first column and should be :2")
    (is (= :b (sut/find-piece (sut/board-with-markers) [0 1]))
        "First row, second column and should be :b")))

(deftest translate
  (testing "For an 2x2 board:"
    (let [board-2x2 [[:a nil]
                     [nil nil]]]
      (is (= (sut/translate board-2x2
                            [0 0]
                            [0 0])
             board-2x2)
          "A noop offset should return the board unchanged")
      (is (= (sut/translate board-2x2
                            [0 0]
                            [1 0])
             [[nil nil]
              [:a nil]])
          "Should move the piece down on a one row-offset")
      (is (= (sut/translate board-2x2
                            [0 0]
                            [0 1])
             [[nil :a]
              [nil nil]])
          "Should move the piece down on a one col-offset")
      (is (= (-> board-2x2
                 (sut/translate [0 0] [1 1])
                 (sut/translate [1 1] [-1 -1]))
             board-2x2)
          "Translate should handle negative translations"))))

(deftest remove-disallowed-moves
  (testing "For a 2x2 board:"
    (let [wk sut/white-king-piece
          bk sut/black-king-piece
          wq sut/white-queen-piece
          board-2x2 [[wk  nil],
                     [nil nil]]
          origin [0 0]]
      (testing "moves to out of bounds should be removed"
        (let [allowed-moves (sut/remove-disallowed-moves {:board board-2x2} origin)]
          (is (= #{[0 1] [1 1] [1 0]}
                 (set allowed-moves)))))
      (testing "moves blocked by piece of same colour:"
        (testing "horizontally blocked moves should be removed"
          (let [blocked-horizontally [[wk  wq]
                                      [nil nil]]
                allowed-moves (sut/remove-disallowed-moves {:board blocked-horizontally} origin)]
            (is (= #{[1 1] [1 0]}
                   (set allowed-moves)))))
        (testing "vertically blocked moves should be removed"
          (let [blocked-vertically [[wk nil]
                                    [wq nil]]
                allowed-moves (sut/remove-disallowed-moves {:board blocked-vertically} origin)]
            (is (= #{[0 1] [1 1]}
                   (set allowed-moves))))))
      (testing "moves blocked by piece of opposite colour:"
        (testing "horizontal blocked moves should be allowed"
          (let [blocked-horizontally [[wk  bk]
                                      [nil nil]]
                allowed-moves (sut/remove-disallowed-moves {:board blocked-horizontally} origin)]
            (is (= #{[1 1] [1 0]}
                   (set allowed-moves)))))
        (testing "vertically blocked moves should be allowed"
          (let [blocked-vertically [[wk nil]
                                    [bk nil]]
                allowed-moves (sut/remove-disallowed-moves {:board blocked-vertically} origin)]
            (is (= #{[0 1] [1 1]}
                   (set allowed-moves)))))))))

(deftest add-pawn-moves
  (testing "For a 8x8 board:"
    (let [wp sut/white-pawn-piece
          pawn-position [1 0]
          board (sut/update-board (sut/new-board) pawn-position wp)
          allowed-moves [[2 0]]]
      (testing "Should add two step move for pawn in starting row"
        (let [added-two-step (sut/add-pawn-moves {:board board :history []}
                                                 pawn-position
                                                 allowed-moves)]
          (is (= #{[2 0] [3 0]}
                 (set added-two-step)))))
      (testing "Should be able to take a black piece diagonal and in front of it"
        (let [bk sut/black-king-piece
              diagonal-to-pawn [2 1]
              board-with-queen (sut/update-board board diagonal-to-pawn bk)
              added-diagonal-take (sut/add-pawn-moves {:board board-with-queen
                                                       :history []}
                                                      pawn-position
                                                      allowed-moves)]
          (is (= #{[2 0] [3 0] [2 1]}
                 (set added-diagonal-take)))))
      (testing "Should be able to take a pawn that passes it with a double-step"
        (let [bp sut/black-pawn-piece
              black-starting-position [6 1]
              black-double-step [4 1]
              white-passed-pawn [4 0]
              board-with-passed-pawn (-> (sut/new-board)
                                         (sut/update-board white-passed-pawn wp)
                                         (sut/update-board black-double-step bp))
              new-allowed-moves [[5 0]]
              added-en-passant (sut/add-pawn-moves {:board board-with-passed-pawn
                                                    :history [[black-starting-position
                                                               black-double-step]]}
                                                   white-passed-pawn
                                                   new-allowed-moves)]
          (is (= #{[5 0] [5 1]}
                 (set added-en-passant))))))))

(deftest add-king-moves
  (testing "For a 8x8 board:"
    (let [wk sut/white-king-piece
          king-starting-position [0 4]
          wr sut/white-rook-piece
          rook-left-starting-position [0 0]
          rook-right-starting-position [0 7]
          board (-> (sut/new-board)
                    (sut/update-board king-starting-position wk)
                    (sut/update-board rook-left-starting-position wr)
                    (sut/update-board rook-right-starting-position wr))
          allowed-moves [[0 5]
                         [1 4]
                         [0 3]
                         [1 5]
                         [1 3]]]
      (testing "Adding Castling:"
        (testing "When king has not moved, rooks have not moved and line is unobstructed:"
          (let [added-castling (sut/add-king-moves {:board board :history []}
                                                   king-starting-position
                                                   allowed-moves)]
            (testing "should add left castling"
              (is (contains? (set added-castling) [0 2])))
            (testing "should add right castling"
              (is (contains? (set added-castling) [0 6])))))
        (testing "When king has moved away and back to starting position"
          (let [move-king-right-then-left [[[0 4] [0 5]]
                                           [[0 5] [0 4]]]
                added-castling (sut/add-king-moves {:board board
                                                    :history move-king-right-then-left}
                                                   king-starting-position
                                                   allowed-moves)]
            (testing "should add no additional moves"
              (is (= allowed-moves added-castling)))))
        (testing "When a rook has moved away and back to starting position:"
          (let [move-rook-left-then-right [[[0 7] [0 6]]
                                           [[0 6] [0 7]]]
                added-castling (sut/add-king-moves {:board board
                                                    :history move-rook-left-then-right}
                                                   king-starting-position
                                                   allowed-moves)]
            (testing "should be able to castle left"
              (is (contains? (set added-castling) [0 2])))
            (testing "should not be able to castle right"
              (is (not (contains? (set added-castling) [0 6]))))))))))
