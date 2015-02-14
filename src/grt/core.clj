(ns grt.core
  (:require [grt.lexer :as lexer]
            [grt.parser :as parser]
            [grt.symboltabler :as symboltabler]
            [grt.typechecker :as typechecker]))





(defn build-identifier-navigation [ast] (:annotated (build-identifier-navigation-rec 1 ast 0)))




(def ided (build-identifier-navigation (parse-it (lex-it ["(3.15 3.16)"]))))
