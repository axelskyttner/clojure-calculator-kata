(ns calculator.core-test
  (:require [clojure.test :refer :all]
            [calculator.core :refer :all]))


(deftest test-of-evaluate-0
  (testing "Test of evaluating non existing register should return 0"
    (let [
          transaction-list []
          ]
      (is (= (evaluate-register "a" transaction-list ) 0)))))

(deftest test-of-evaluate-1
  (testing "Test of evaluating add should return sum "
    (let [
          transaction-list ["a add 2","a add 3"]
          ]
      (is (= (evaluate-register "a" transaction-list ) 5)))))


(deftest test-of-evaluate-2
  (testing "Test of evaluating list should only evaluate specific register"
    (let [
          transaction-list ["a add 2", "b add 5", "a add 3"]
          ]
      (is (= (evaluate-register "a" transaction-list ) 5)))))


(deftest test-of-evaluate-3
  (testing "Test of evaluating list should have plus and minus"
    (let [
          transaction-list ["a add 2", "a subtract 1"] 
          ]
      (is (= (evaluate-register "a" transaction-list ) 1)))))


(deftest test-of-evaluate-4
  (testing "Test of evaluating list with register as inputs"
    (let [
          transaction-list ["a add b", "b add 1"] 
          ]
      (is (= (evaluate-register "a" transaction-list ) 1)))))

(deftest test-from-req-spec-0
  (testing "Testing if the example given in file is wokring"
    (let [
          operations [
                      "A add 2"
                      "A add 3"
                      "B add 5"
                      "B subtract 2"
                      "A add 1"
                      ]
          ]
      (do 
        (is (=  (evaluate-register "A" operations) 6))
        (is (=  (evaluate-register "B" operations) 3))))))

(deftest test-from-req-spec-1
  (testing "Testing if the example given in file is wokring"
    (let [
          operations [
                      "result add revenue"
                      "result subtract costs"
                      "revenue add 200"
                      "costs add salaries"
                      "salaries add 20"
                      "salaries multiply 5"
                      "costs add 10"
                      ]
          ]
      (is (=  (evaluate-register "result" operations) 90)))))

(deftest test-case-insentive-0
  (testing "test if operations can be evaluated case insensitive"
    ( let [
          operations [
                      "a ADD 2"
                      "QUIT"
                      ]
          ]
    (is (= (evaluate-register "a" operations) 2))
    (is (= (quit? operations) true)))))

(deftest test-error-0
  (testing "Testing if the program handles unexpected input"
    (let [
          operations [
                      "hejkonbacon"
                      "a add 2"
                      "a divide 2"
                      "a add asd"
                      "a asd sdadd asd"
                      "I'm a very wrong operation!;';["
                      "; [ ' "
                      ]

          ]
      (is (= (evaluate-register "a" operations) 2)))))

(deftest quit-has-been-pressed
  (testing "Test if ind wuit functino is working"
    (let [
          operations [
                      "quit"
                      ]
          ]

      (is (= (quit? operations) true)))))
