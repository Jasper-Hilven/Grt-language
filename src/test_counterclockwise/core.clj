(ns test-counterclockwise.core
  )

(def path-to-file "C:\Data\Clojure\counterclockwise")

(defn add-line [text l-number]
  (let [cur-char (first rem-text)
            number-char "0123456789"
            dot-char "."
            text-char "qwertyuiopasdfghjklmzxcvbnAZERTYUIOPQSDFGHKLMWXCVBN"
            space-char " "
            quote-char "\""
            dot-char "."
            open-bracket-char "("
            close-bracket-char ")"
            end-others-char (str close-bracket-char open-bracket-char space-char)
            text-number-char (str text-char number-char)]
    (loop [cur-state :init char-count 0 startword-char-count 0 previous "" rem-text text]
      (if (empty? rem-text)
        [{:type (if ( = cur-state :init) :newline cur-state):text previous :char-pos startword-char-count :l-number l-number}]
      
        
          (case cur-state
            :init (cond
                    (contains? number-char cur-char)  (recur :int char-count startword-char-count "" rem-text)
                    (contains? text-char cur-char) (recur :id char-count startword-char-count "" rem-text)
                    (contains? quote-char cur-char (recur :string char-count startword-char-count "" rem-text)
                    (contains? space-char cur-char)(recur :space char-count startword-char-count "" rem-text)
                    (contains? open-bracket-char cur-char) (into [{:type :l-bracket :text "(" :char-pos char-count :l-number l-number}]
                                                                 (recur :init (inc char-count) (inc char-count) "" (cdr rem-text))
                  (contains? close-bracket-char cur-char) (into [{:type :r-bracket :text ")" :char-pos char-count :l-number l-number}]
                                                                  (recur :init (inc char-count) (inc char-count) "" (cdr rem-text))
                  (contains? dot-char cur-char) (recur :float-dot-direct (inc char-count) char-count cur-char rem-text)))))
            :int (cond
                   (contains? dot-char cur-char) (recur :float-dot (inc char-count) startword-char-count (str previous cur-char) (cdr rem-text))
                   (contains? number-char cur-char) (recur :int (inc char-count) startword-char-count (str previous cur-char) (cdr rem-text))
                   (contains? end-others-char cur-char) (into [{:type :int :text previous :char-pos startword-char-count :l-number l-number}]
                                                              (recur :init char-count char-count "" rem-text)))
            :float-dot-direct (cond
                       (contains? number-char cur-char) (recur :float (inc char-count) startword-char-count (str previous cur-char) (cdr rem-text))          
            :float-dot (cond
                         (contains? number-char cur-char) (recur :float (inc char-count) startword-char-count (str previous cur-char) (cdr rem-text))
                         (contains? end-others-char cur-char) (into [{:type :float :text previous :char-pos startword-char-count :l-number l-number}]
                                                            (recur :init char-count char-count "" rem-text)))
            :float (cond
                     (contains? number-char cur-char) (recur :float (inc char-count) startword-char-count (str previous cur-char) (cdr rem-text))
                     (contains? end-others-char cur-char) (into [{:type :float :text previous :char-pos startword-char-count :l-number l-number}]
                                                            (recur :init char-count char-count "" rem-text)))
            :id (cond
                  (contains? text-number-char cur-char) (recur :id (inc char-count) startword-char-count (str previous cur-char) (cdr rem-text))
                  (contains? end-others-char cur-char) (into [{:type :id :text previous :char-pos startword-char-count :l-number l-number}]
                                                            (recur :init char-count char-count "" rem-text)))
            :string (cond 
                      (contains
            
            
                  
          
            )))))

(defn lex-it [text-file]
  (loop [lexies '() text text-file line-number 0]
    (if 
      (empty? text)
      lexies
      (recur (concat lexies (add-line  (first text) line-number )) (rest text) (+ line-number 1)))))
