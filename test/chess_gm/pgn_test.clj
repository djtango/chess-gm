(ns chess-gm.pgn-test
  (:require [clojure.test :refer :all]
            [orchestra.spec.test :as stest]
            [clojure.spec.alpha :as s]
            [chess-gm.pgn :as sut]))

(deftest basic-SAN
  (testing "a simple vertical move should be valid"
    (let [white-king-pawn-two-steps (seq "e4")]
      (is (s/valid? ::sut/basic-move white-king-pawn-two-steps))))
  (testing "piece names are supported"
    (let [white-kingside-knight-moves-left (seq "Nf3")]
      (is (s/valid? ::sut/basic-move white-kingside-knight-moves-left)))
    (let [white-kingside-bishop-moves-left (seq "Bb5")]
      (is (s/valid? ::sut/basic-move white-kingside-bishop-moves-left))))
  (testing "should support moves that move across column"
    (let [black-knight-protects-queen (seq "Nbd7")]
      (is (s/valid? ::sut/basic-move black-knight-protects-queen)))))

(deftest captures
  (testing "should recognize 'x' as a capture"
    (let [knight-captures-pawn (seq "Nxe4")]
      (is (s/valid? ::sut/capture knight-captures-pawn))))
  (testing "should also recognize pawn captures"
    (let [pawn-captures-another-pawn (seq "axb5")]
      (is (s/valid? ::sut/capture pawn-captures-another-pawn)))))
