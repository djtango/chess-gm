(ns chess-gm.pgn
  (:require [clojure.spec.alpha :as s]))

(s/def ::column-index (set "abcdefgh"))
(s/def ::row-index    (set "12345678"))

(s/def ::piece-name (set "QKWBNRP"))

(s/def ::castling (s/or :kingside #{(seq "O-O")}
                        :queenside #{(seq "O-O-O")}))
(s/def ::check #{\+})
(s/def ::checkmate #{\#})
(s/def ::draw #{(seq "1/2-1/2")})
(s/def ::white-won #{(seq "1-0")})
(s/def ::black-won #{(seq "0-1")})
(s/def ::game-ongoing #{\*})

(s/def ::position (s/cat :file ::column-index
                         :rank ::row-index))

(s/def ::check-or-checkmate (s/alt :check (s/? ::check)
                                   :checkmate (s/? ::checkmate)))

(s/def ::basic-move (s/cat :piece (s/? ::piece-name)
                           :original-file (s/? ::column-index)
                           :original-rank (s/? ::row-index)
                           :position ::position))
(s/def ::pawn-promotion (s/cat :pawn-promotion #{\=}
                               :piece ::piece-name))

(s/def ::capture (s/cat :taker (s/or :pawn ::column-index
                                     :piece ::piece-name)
                        :original-file (s/? ::column-index)
                        :original-rank (s/? ::row-index)
                        :x #{\x}
                        :takee ::position))

(s/def ::move (s/or :move (s/cat :piece-move (s/alt :basic-move ::basic-move
                                                    :capture ::capture)
                                 :?pawn-promotion (s/? ::pawn-promotion)
                                 :?check-or-checkmate (s/? ::check-or-checkmate))
                    :castling ::castling
                    :end (s/or :white-won    ::white-won
                               :black-won    ::black-won
                               :draw         ::draw
                               :game-ongoing ::game-ongoing)))

