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
