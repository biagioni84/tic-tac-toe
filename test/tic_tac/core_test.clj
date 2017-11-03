(ns tic-tac.core-test
  (:require [clojure.test :refer :all]
            [tic-tac.core :refer :all]))

(deftest a-test
  (testing "we can find winners in 3x3"
    (is (= (winner [[\x \o \x]
                    [\x \x \?]
                    [\x \? \?]])
           \x))
    (is (= (winner [[\o \o \x]
                    [\x \x \?]
                    [\x \? \?]])
           \x))
    (is (= (winner [[\o \o \x]
                    [\x \o \?]
                    [\x \? \o]])
           \o))
    (is (= (winner [[\o \o \x]
                    [\? \? \?]
                    [\x \? \o]])
           nil))
    )
  (testing "we can find winners in 4x4"
    (is (= (winner [[\x \o \x \x]
                    [\x \x \? \x]
                    [\x \? \? \o]
                    [\x \? \? \o]
                    ])
           \x))
    )
  ;(testing "not a square"
  ;  (is (= (winner [[\x \o \x \x]
  ;                  [\x \x \? \x]
  ;                  [\x \? \? \o]
  ;                  ])
  ;         \x))
  ;  )
  )

(clojure.test/run-tests)