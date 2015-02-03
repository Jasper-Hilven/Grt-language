(ns test-grt.core-test
  (:require [clojure.test :refer :all]
            [grt.core :refer :all]))


(deftest basic-lexing
  (testing "lex nothing, expect nothing"
           (is (add-line "" 0) []))
  (testing "simple int"
           (is (add-line "0" 0) [{:type :int :text "0" :char-pos 0 :l-number 0}]))
  (testing "int"
           (is (add-line "12345" 0) [{:type :int :text "12345" :char-pos 0 :l-number 0}]))
  (testing "open parenthesis"
           (is (add-line "(" 0) [{:type :l-bracket, :text "(", :char-pos 0, :l-number 0}]))
  (testing "close parenthesis"
           (is (add-line ")" 0) [{:type :r-bracket, :text ")", :char-pos 0, :l-number 0}])))
 (deftest failing-lexing
   (testing "int stuck to id"
            (let [int-id (add-line "1a" 0)]
              ( is (:type (first int-id)) :error))))
            