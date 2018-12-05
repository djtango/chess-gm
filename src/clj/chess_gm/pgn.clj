(ns chess-gm.pgn
  (:require [clojure.spec.alpha :as s]))

(s/def ::column-index (set "abcdefgh"))
(s/def ::row-index    (set "12345678"))

(s/def ::piece-name (set "QKWBNRP"))

(s/def ::check #{\+})
(s/def ::checkmate #{\#})

(s/def ::position (s/cat :file ::column-index
                         :rank ::row-index))

(s/def ::basic-move (s/cat :piece (s/? ::piece-name)
                           :original-file (s/? ::column-index)
                           :original-rank (s/? ::row-index)
                           :position ::position
                           :pawn-promotion? (s/? ::pawn-promotion)
                           :win? (s/alt :check (s/? ::check)
                                        :checkmate (s/? ::checkmate))))
(s/def ::pawn-promotion (s/cat :pawn-promotion #{\=}
                               :piece ::piece-name))

(s/def ::capture (s/cat :taker (s/or :pawn ::column-index
                                     :piece ::piece-name)
                        :original-file (s/? ::column-index)
                        :original-rank (s/? ::row-index)
                        :x #{\x}
                        :takee ::position
                        :pawn-promotion? (s/? ::pawn-promotion)
                        :check (s/? ::check)
                        :checkmate (s/? ::checkmate)))

(s/def ::castling (s/or :kingside #{(seq "O-O")}
                        :queenside #{(seq "O-O-O")}))
