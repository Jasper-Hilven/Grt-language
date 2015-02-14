(ns grt.parser.core
  :require [grt.parser.hierarchybuilder :as h]
           [grt.parser.elementmatcher :as em])


;;;;;;;;;;;;;;;;;;;GENERAL
(defn parse-it [lexed] 
  (let [parentheses-parsed    (h\parse-by-parentheses lexed 0)
        built-indentifier (h\build-identifier-navigation-rec parentheses-parsed)
        matched-elements  (em\match-elements built-indentifier)
        ] matched-elements))

