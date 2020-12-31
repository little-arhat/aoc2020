(ns larhat.aoc2020-test
   (:require [clojure.test :refer :all]
             [larhat.aoc2020 :refer :all]))

(defmacro t [n & body]
  `(deftest ~n
     (testing ~n ~@body)))
                                        ; regression
(t day1
  (is (= 970816 (run-day-1-1)))
  (is (= 96047280 (run-day-1-2))))

(t day2
  (is (= 586 (run-day-2-1)))
  (is (= 352 (run-day-2-2))))

(t day3
  (is (= 145 (run-day-3-1)))
  (is (= 3424528800 (run-day-3-2))))

(t day4
  (is (= 230 (run-day-4-1)))
  (is (= 156 (run-day-4-2))))

(t day5
  (is (= 953 (run-day-5-1)))
  (is (= 615 (run-day-5-2))))

(t day6
  (is (= 6590 (run-day-6-1)))
  (is (= 3288 (run-day-6-2))))

(t day7
  (is (= 179 (run-day-7-1)))
  (is (= 18925 (run-day-7-2))))

(t day8
  (is (= 1217 (run-day-8-1)))
  (is (= 501 (run-day-8-2))))

(t day9
  (is (= 1930745883 (run-day-9-1)))
  (is (= 268878261 (run-day-9-2))))

(t day10
  (is (= 2112 (run-day-10-1)))
  (is (= [3022415986688
          3022415986688
          3022415986688] (run-day-10-2))))
