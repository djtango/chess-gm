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
        (let [allowed-moves (sut/remove-disallowed-moves board-2x2 origin)]
          (is (= #{[0 1] [1 1] [1 0]}
                 (set allowed-moves)))))
      (testing "moves blocked by piece of same colour:"
        (testing "horizontally blocked moves should be removed"
          (let [blocked-horizontally [[wk  wq]
                                      [nil nil]]
                allowed-moves (sut/remove-disallowed-moves blocked-horizontally origin)]
            (is (= #{[1 1] [1 0]}
                   (set allowed-moves)))))
        (testing "vertically blocked moves should be removed"
          (let [blocked-vertically [[wk nil]
                                    [wq nil]]
                allowed-moves (sut/remove-disallowed-moves blocked-vertically origin)]
            (is (= #{[0 1] [1 1]}
                   (set allowed-moves))))))
      (testing "moves blocked by piece of opposite colour:"
        (testing "horizontal blocked moves should be allowed"
          (let [blocked-horizontally [[wk  bk]
                                      [nil nil]]
                allowed-moves (sut/remove-disallowed-moves blocked-horizontally origin)]
            (is (= #{[1 1] [1 0]}
                   (set allowed-moves)))))
        (testing "vertically blocked moves should be allowed"
          (let [blocked-vertically [[wk nil]
                                    [bk nil]]
                allowed-moves (sut/remove-disallowed-moves blocked-vertically origin)]
            (is (= #{[0 1] [1 1]}
                   (set allowed-moves)))))))))
