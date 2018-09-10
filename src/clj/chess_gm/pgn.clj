(ns chess-gm.pgn
  (:require [clojure.spec.alpha :as s]))

(s/def ::basic-move any?)
