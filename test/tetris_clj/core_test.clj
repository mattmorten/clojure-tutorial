(ns tetris-clj.core-test
  (:require [clojure.test :refer :all]
            [tetris-clj.core :refer :all]
            [clojure.spec.alpha :as s]))

(deftest generative-test
  (s/exercise-fn `do-action 100))

