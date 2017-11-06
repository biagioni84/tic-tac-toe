(ns tic-tac.core-test
  (:require [clojure.test :refer :all]
            [tic-tac.core :refer :all]))

(deftest is-square-test
  (is (is-square [[0 0]
                  [0 0]]))
  (is (is-square [[0 0 0]
                  [0 0 0]
                  [0 0 0]]))
  (is (not (is-square [[0 0]
                       [0 0]
                       [0 0]])))
  (is (not (is-square [[0]
                       [0 0]]))))

(deftest diagonals-test
  (is (= (set (diagonals [[1 2 3]
                          [4 5 6]
                          [7 8 9]]))
         #{[1 5 9] [3 5 7]})))

(deftest find-with-val-test
  (is (= (set (find-with-val [[\? \o \o]
                              [\x \? \?]
                              [\? \o \?]]
                             \o))
         #{[0 1] [0 2] [2 1]})))

(deftest apply-action-test
  (is (= (apply-action 8
                       [[0 0 0]
                        [0 0 0]
                        [0 0 0]]
                       [1 1])
         [[0 0 0]
          [0 8 0]
          [0 0 0]])))

(deftest future-boards-test
  (is (= (future-boards \o
                        [[\o \o \x]
                         [\x \x \?]
                         [\x \? \?]]
                        \?)
         '([[1 2] [[\o \o \x]
                   [\x \x \o]
                   [\x \? \?]]]
           [[2 1] [[\o \o \x]
                   [\x \x \?]
                   [\x \o \?]]]
           [[2 2] [[\o \o \x]
                   [\x \x \?]
                   [\x \? \o]]]))))

(deftest index-board
  (is (= (index-board [[\? \? \?]
                       [\? \? \?]
                       [\? \? \?]])
         ([[0 0] \?]
          [[0 1] \?]
          [[0 2] \?]
          [[1 0] \?]
          [[1 1] \?]
          [[1 2] \?]
          [[2 0] \?]
          [[2 1] \?]
          [[2 2] \?]))))

(deftest first-moves-test
  (first-moves [[\? \? \?]
                [\? \? \?]
                [\? \? \?]]
               \o
               \x))

(deftest winner-test

  (testing "we can find winners in 3x3"
    #_(is (= (winner [[\x \o \x]
                    [\x \x \?]
                    [\x \? \?]])
           '(\x)))
    (is (= (winner [[\o \o \x]
                    [\x \x \?]
                    [\x \? \?]])
           '(\x)))
    (is (= (winner [[\o \o \x]
                    [\x \o \?]
                    [\x \? \o]])
           '(\o)))
    (is (= (winner [[\o \o \x]
                    [\? \? \?]
                    [\x \? \o]])
           '())))
  
  (testing "we can find winners in 4x4"
    (is (= (winner [[\x \o \x \x]
                    [\x \x \? \x]
                    [\x \? \? \o]
                    [\x \? \? \o]])
           '(\x)))))


(comment
  
  (clojure.test/run-tests)
  )
