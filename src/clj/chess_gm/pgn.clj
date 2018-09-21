(ns chess-gm.pgn
  (:require [clojure.spec.alpha :as s]))

(s/def ::column-index (set "abcdefgh"))
(s/def ::row-index    (set "12345678"))

(s/def ::piece-name (set "KWBNRP"))

(s/def ::position (s/cat :col ::column-index
                         :row ::row-index))

(s/def ::basic-move (s/cat :piece (s/? ::piece-name)
                           :starting-column (s/? ::column-index)
                           :position ::position))

(s/def ::capture (s/cat :taker (s/or :pawn ::column-index
                                     :piece ::piece-name)
                        :x #{\x}
                        :takee ::position))

;; YAGNI
;; (s/def ::pawn #{\P})
;; (s/def ::rook #{\R})
;; (s/def ::bishop #{\B})
;; (s/def ::queen #{\Q})
;; (s/def ::king #{\K})
;; (s/def ::knight #{\N})
