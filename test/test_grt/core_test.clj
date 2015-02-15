(ns test-grt.core-test
  (:require [clojure.test :refer :all]
            [grt.core :as c]
           [grt.lexer :as l]
            [grt.parser.core :as p]
            ))


(deftest basic-lexing
 (testing "lex nothing, expect nothing"
         (is (l/add-line "" 0) []))
 (testing "simple int"
           (is (l/add-line "0" 0) [{:type :int :text "0" :char-pos 0 :l-number 0}]))
  (testing "int"
           (is (l/add-line "12345" 0) [{:type :int :text "12345" :char-pos 0 :l-number 0}]))
  (testing "open parenthesis"
         (is (l/add-line "(" 0) [{:type :l-bracket, :text "(", :char-pos 0, :l-number 0}]))
 (testing "close parenthesis"
           (is (l/add-line ")" 0) [{:type :r-bracket, :text ")", :char-pos 0, :l-number 0}])))

;;;;;;;;parsing
(def left-par {:type :l-parenthesis, :text "(", :char-pos 4, :l-number 0})
(def right-par {:type :r-parenthesis, :text ")", :char-pos 3, :l-number 0})
(def lbracket {:type :l-bracket, :text "[", :char-pos 12, :l-number 0})
(def rbracket {:type :r-bracket, :text "]", :char-pos 112, :l-number 0})
(def function-lbl {:type :id, :text "fn", :char-pos 3, :l-number 4})
(def function-name {:type :id, :text "callme", :char-pos 3, :l-number 4})
(def id1 {:type :id , :text "maybe", :char-pos 3, :l-number 45 })
(def id2 {:type :id , :text "crazy", :char-pos 3, :l-number 45 })
(def fretval {:type :int :text "0" :char-pos 1 :l-number 2})
(def letid {:type :id :text "let",:char-pos 324,:l-number 34})
(def argument-arr [rbracket fargument lbracket])
(def emptyparentheses [left-par right-par])
(def functiondef [left-par function-lbl function-name lbracket id1 rbracket fretval right-par])
(def emptyparenthesesbuilt (grt.parser.core/parse-till-id emptyparentheses))
(def zerofunctionbuilt (grt.parser.core/parse-till-id functiondef))
(def letconstruction [left-par letid lbracket id1 id2 rbracket fretval right-par])
(def letbuilt (grt.parser.core/parse-till-id letconstruction))

(deftest parser-hierarchy-building
  (testing "parsing emptyness is wrong..." (is ((grt.parser.core/parse-it []) :type) :parse-error)))
(deftest recognize-function
   (testing "recognize a function" (is ((grt.parser.core/parse-it zerofunctionbuilt) :type) :function)))
(deftest recognize-let
   (testing "recognize a let" (is ((grt.parser.core/parse-it letconstruction) :type) :let)))
(deftest recognize-function-call
   (testing "recognize a function call" (is ((grt.parser.core/parse-it zerofunctionbuilt) :type) :function)))
