(ns chess-gm.core-test
  (:require [clojure.test :refer :all]
            [chess-gm.core :as sut]))

(deftest find-piece
  (testing "for a board with algebraic chess columns, should be b"
    (is (= :b (sut/find-piece (sut/board-with-markers) [1 6])))
    ;; (is (= :b (sut/find-piece (sut/board-with-markers) [0 2])))
    ;; (is (= :b (sut/find-piece (sut/board-with-markers) [0 3])))
    ;; (is (= :b (sut/find-piece (sut/board-with-markers) [0 4])))
    ;; (is (= :b (sut/find-piece (sut/board-with-markers) [0 5])))
    ;; (is (= :b (sut/find-piece (sut/board-with-markers) [0 6])))
    ))
