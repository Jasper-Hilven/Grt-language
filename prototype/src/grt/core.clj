(ns grt.core
  (:require 
    [grt.lexer :as lexer]
    [grt.parser.core :as parser]
    [grt.symboltabler :as symboltabler]
    [grt.typechecker :as typechecker]))








(def lex (lexer/lex-it ["(3.15 3.16)"]))
;(def eparse (parser/parse-it lex))
