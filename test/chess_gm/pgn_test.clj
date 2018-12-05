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
      (is (s/valid? ::sut/basic-move black-knight-protects-queen))))
  (testing "disambiguating moves is supported"
    (let [queen-moves-from-h8-to-g7 (seq "Qh8g7")]
      (is (s/valid? ::sut/basic-move queen-moves-from-h8-to-g7))))
  (testing "canned set of disambiguated moves should all be valid"
    (are [s] (s/valid? ::sut/basic-move s)
         (seq "Rbb7")
         (seq "Rab7")
         (seq "Rcb7")
         (seq "Nac3")
         (seq "Nbc3")
         (seq "Ndc3")))
  (testing "checking move with a '+' suffix is supported"
    (is (s/valid? ::sut/basic-move (seq "Ra6+"))))
  (testing "checkmating move with a '#' suffix is supported"
    (is (s/valid? ::sut/basic-move (seq "Qg5#"))))
  (testing "check and checkmate should not be simultaneously possible"
    (is (not (s/valid? ::sut/basic-move (seq "Qg5#+"))))
    (is (not (s/valid? ::sut/basic-move (seq "Qg5+#")))))
  (testing "canned set of disambiguated moves should all be valid"
    (are [s] (s/valid? ::sut/basic-move s)
         (seq "Rbb7#")
         (seq "Rab7+")
         (seq "Rab7+"))))

(deftest captures
  (testing "should recognize 'x' as a capture"
    (let [knight-captures-pawn (seq "Nxe4")]
      (is (s/valid? ::sut/capture knight-captures-pawn))))
  (testing "should also recognize pawn captures"
    (let [pawn-captures-another-pawn (seq "axb5")]
      (is (s/valid? ::sut/capture pawn-captures-another-pawn))))
  (testing "disambiguated captures should work"
    (are [s] (s/valid? ::sut/capture s)
         (seq "fxg5")
         (seq "hxg5")
         (seq "hxg6")))
  (testing "checking captures should work"
    (are [s] (s/valid? ::sut/capture s)
         (seq "Bxf7+")
         (seq "Rxe1+")))
  (testing "checkmating capture with a '#' suffix is supported"
    (is (s/valid? ::sut/capture (seq "Qxg5#"))))
  (testing "a disambiguated move that captures leading to a pawn promotion and check is supported"
    (is (s/valid? ::sut/capture (seq "d7xc8=N+")))))

(deftest castling
  (testing "kingside castling is supported"
    (is (s/valid? ::sut/castling (seq "O-O"))))
  (testing "queenside castling is supported"
    (is (s/valid? ::sut/castling (seq "O-O-O")))))

(deftest pawn-promotion
  (testing "an '=' denoting a pawn promotion should be supported"
    (are [s] (s/valid? ::sut/basic-move s)
         (seq "e8=Q")
         (seq "d8=N#")
         (seq "d8=N+")))
  (testing "captures leading to a pawn promotion should be supported"
    (are [s] (s/valid? ::sut/capture s)
         (seq "exd8=Q")
         (seq "dxc8=N+")
         (seq "dxc8=N#"))))
