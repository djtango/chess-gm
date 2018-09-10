(ns chess-gm.pgn-test
  (:require [clojure.test :refer :all]
            [orchestra.spec.test :as stest]
            [clojure.spec.alpha :as s]
            [chess-gm.pgn :as sut]))

(deftest basic-SAN
  (testing "a simple move should be valid"
    (let [king-pawn-two-steps "e4"]
      (is (s/valid? ::sut/basic-move (seq king-pawn-two-steps))))))
