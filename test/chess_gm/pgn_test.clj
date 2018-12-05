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
         (seq "Ndc3"))))

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
         (seq "hxg6"))))

(deftest castling
  (testing "kingside castling is supported"
    (is (s/valid? ::sut/castling (seq "O-O"))))
  (testing "queenside castling is supported"
    (is (s/valid? ::sut/castling (seq "O-O-O")))))

(deftest pawn-promotion
  (testing "an '=' denoting a pawn promotion should be supported"
    (are [s] (s/valid? ::sut/move s)
         (seq "e8=Q")
         (seq "d8=N#")
         (seq "d8=N+")))
  (testing "captures leading to a pawn promotion should be supported"
    (are [s] (s/valid? ::sut/move s)
         (seq "exd8=Q")
         (seq "dxc8=N+")
         (seq "dxc8=N#"))))

(deftest check-or-checkmate
  (testing "checking move with a '+' suffix is supported"
    (is (s/valid? ::sut/move (seq "Ra6+"))))
  (testing "checkmating move with a '#' suffix is supported"
    (is (s/valid? ::sut/move (seq "Qg5#"))))
  (testing "check and checkmate should not be simultaneously possible"
    (is (not (s/valid? ::sut/move (seq "Qg5#+"))))
    (is (not (s/valid? ::sut/move (seq "Qg5+#")))))
  (testing "canned set of disambiguated moves should all be valid"
    (are [s] (s/valid? ::sut/move s)
         (seq "Rbb7#")
         (seq "Rab7+")
         (seq "Rab7+")))
  (testing "checking captures should work"
    (are [s] (s/valid? ::sut/move s)
         (seq "Bxf7+")
         (seq "Rxe1+")))
  (testing "checkmating capture with a '#' suffix is supported"
    (is (s/valid? ::sut/move (seq "Qxg5#"))))
  (testing "a disambiguated move that captures leading to a pawn promotion and check is supported"
    (is (s/valid? ::sut/move (seq "d7xc8=N+")))))

(declare canned-game)
(deftest check-a-canned-game
  (testing "every move in a canned game should be valid"
    (is (every? #(s/valid? ::sut/move (seq %)) canned-game))))

(def canned-game
  ["e4"
   "e5"
   "Nf3"
   "Nc6"
   "Bb5"
   "a6"
   "Ba4"
   "Nf6"
   "O-O"
   "Be7"
   "Re1"
   "b5"
   "Bb3"
   "d6"
   "c3"
   "O-O"
   "h3"
   "Nb8"
   "d4"
   "Nbd7"
   "c4"
   "c6"
   "cxb5"
   "axb5"
   "Nc3"
   "Bb7"
   "Bg5"
   "b4"
   "Nb1"
   "h6"
   "Bh4"
   "c5"
   "dxe5"
   "Nxe4"
   "Bxe7"
   "Qxe7"
   "exd6"
   "Qf6"
   "Nbd2"
   "Nxd6"
   "Nc4"
   "Nxc4"
   "Bxc4"
   "Nb6"
   "Ne5"
   "Rae8"
   "Bxf7+"
   "Rxf7"
   "Nxf7"
   "Rxe1+"
   "Qxe1"
   "Kxf7"
   "Qe3"
   "Qg5"
   "Qxg5"
   "hxg5"
   "b3"
   "Ke6"
   "a3"
   "Kd6"
   "axb4"
   "cxb4"
   "Ra5"
   "Nd5"
   "f3"
   "Bc8"
   "Kf2"
   "Bf5"
   "Ra7"
   "g6"
   "Ra6+"
   "Kc5"
   "Ke1"
   "Nf4"
   "g3"
   "Nxh3"
   "Kd2"
   "Kb5"
   "Rd6"
   "Kc5"
   "Ra6"
   "Nf2"
   "g4"
   "Bd3"
   "Re6"])
