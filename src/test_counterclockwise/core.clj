(ns test-counterclockwise.core)

(def path-to-file "C:\Data\Clojure\counterclockwise")

(defn add-line [text l-number]
  (loop [cur-state :init char-count 0 previous "" rem-text text]
    (if (empty? rem-text)
      [{:type (if ( = cur-state :init) :newline cur-state):text previous :char-pos char-count :l-number l-number}]
      (case cur-state
       :init )      
  ))


(defn lex-it [text-file]
  (loop [lexies '() text text-file line-number 0]
    (if 
      (empty? text)
      lexies
      (recur (concat lexies (add-line  (first text) line-number )) (rest text) (+ line-number 1)))))
