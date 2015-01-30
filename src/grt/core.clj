(ns Grt.core)

(defn add-line [text l-number]
  (let [number-char "0123456789" 
        text-char "<>+-?!_:qwertyuiopasdfghjklmzxcvbnAZERTYUIOPQSDFGHKLMWXCVBN"
        space-char " " quote-char "'" dot-char "."
        open-bracket-char "(" close-bracket-char ")"
        open-sqbracket-char "[" close-sqbracket-char "]"
        open-hbracket-char "{" close-hbracket-char "}"
        end-others-char (str close-bracket-char open-bracket-char space-char)
        text-number-char (str text-char number-char)]
    (loop [cur-state :init char-count 0 startword-char-count 0 previous "" rem-text text acc []]
      (let [cur-char (str (first rem-text))]
      (if (empty? rem-text)
        (if ( = cur-state :init)
          acc
          (into acc [(cond 
                       (= cur-state :float-dot-direct) {:type :error :text "bad float ending" :char-pos startword-char-count :l-number l-number}
                       :else {:type :error :text "bad ending of something" :char-pos startword-char-count :l-number l-number}
                       )]))
          (case cur-state
            :init (cond
                    (.contains number-char cur-char)  (recur :int char-count startword-char-count "" rem-text acc)
                    (.contains text-char cur-char) (recur :id char-count startword-char-count "" rem-text acc)
                    (.contains quote-char cur-char) (recur :char char-count startword-char-count "" rem-text acc)
                    (.contains space-char cur-char)(recur :space char-count startword-char-count "" rem-text acc)
                    (.contains open-bracket-char cur-char) (recur :init (inc char-count) (inc char-count) "" (rest rem-text) (into acc [{:type :l-parenthesis :text "(" :char-pos char-count :l-number l-number}]))
                    (.contains close-bracket-char cur-char) (recur :init (inc char-count) (inc char-count) "" (rest rem-text)(into acc [{:type :r-parenthesis :text ")" :char-pos char-count :l-number l-number}]))
                    (.contains open-hbracket-char cur-char) (recur :init (inc char-count) (inc char-count) "" (rest rem-text) (into acc [{:type :l-hbracket :text "{" :char-pos char-count :l-number l-number}]))
                    (.contains close-hbracket-char cur-char) (recur :init (inc char-count) (inc char-count) "" (rest rem-text)(into acc [{:type :r-hbracket :text "}" :char-pos char-count :l-number l-number}]))
                    (.contains open-sqbracket-char cur-char) (recur :init (inc char-count) (inc char-count) "" (rest rem-text) (into acc [{:type :l-bracket :text "[" :char-pos char-count :l-number l-number}]))
                    (.contains close-sqbracket-char cur-char) (recur :init (inc char-count) (inc char-count) "" (rest rem-text)(into acc [{:type :r-bracket :text "]" :char-pos char-count :l-number l-number}]))
                    (.contains dot-char cur-char) (recur :float-dot-direct (inc char-count) char-count cur-char (rest rem-text) acc))
            :int (cond
                   (.contains dot-char cur-char) (recur :float-dot (inc char-count) startword-char-count (str previous cur-char) (rest rem-text) acc)
                   (.contains number-char cur-char) (recur :int (inc char-count) startword-char-count (str previous cur-char) (rest rem-text) acc)
                   (.contains end-others-char cur-char) (recur :init char-count char-count "" rem-text (into acc [{:type :int :text previous :char-pos startword-char-count :l-number l-number}]) )
                   :else (recur :init char-count char-count "" rem-text (into acc [{:type :error :text (str "The integer: " previous " at (line: " l-number ", position: " startword-char-count ") is illegally stopped by the character '" cur-char "' at position: " char-count) :char-pos startword-char-count :l-number l-number}]) ))                
            :float-dot-direct (cond
                       (.contains number-char cur-char) (recur :float (inc char-count) startword-char-count (str previous cur-char) (rest rem-text) acc)          
                        :else ["floatdotdirecterror" cur-char])
            :float-dot (cond
                         (.contains number-char cur-char) (recur :float (inc char-count) startword-char-count (str previous cur-char) (rest rem-text) acc )
                         (.contains end-others-char cur-char) (recur :init char-count char-count "" rem-text (into acc [{:type :float :text previous :char-pos startword-char-count :l-number l-number}])))
            :float (cond
                     (.contains number-char cur-char) (recur :float (inc char-count) startword-char-count (str previous cur-char) (rest rem-text) acc)
                     (.contains end-others-char cur-char) (recur :init char-count char-count "" rem-text (into acc [{:type :float :text previous :char-pos startword-char-count :l-number l-number}] )))
            :id (cond
                  (.contains text-number-char cur-char) (recur :id (inc char-count) startword-char-count (str previous cur-char) (rest rem-text) acc)
                  (.contains end-others-char cur-char) (recur :init char-count char-count "" rem-text (into acc [{:type :id :text previous :char-pos startword-char-count :l-number l-number}])))
            :space (cond
                     (.contains space-char cur-char) (recur :space (inc char-count) startword-char-count (str previous " ") (rest rem-text) acc)
                     :else (recur :init char-count char-count "" rem-text (into acc [{:type :space :text  previous :char-pos startword-char-count :l-number l-number}])))                                
            :char (cond 
                    (.contains text-number-char cur-char) (recur :id (inc char-count) startword-char-count (str previous cur-char) (rest rem-text) acc))
            :end-char (cond
                  (.contains quote-char cur-char) (recur :init (inc char-count) (inc char-count) "" (rest rem-text) (into acc [{:type :char :text previous :char-pos startword-char-count :l-number l-number}]) ))))))))

(defn lex-it [full-text]
  (loop [lexies '() remaining-text full-text line-number 0]
    (if 
      (empty? remaining-text)
      lexies 
      (recur (concat lexies (add-line (first full-text) line-number )) (rest remaining-text) (+ line-number 1)))))


(comment (is (add-line "" 0) [{:type :newline, :text "", :char-pos 0, :l-number 0}])
         (is  (add-line "(" 0) [{:type :l-bracket, :text "(", :char-pos 0, :l-number 0}]))