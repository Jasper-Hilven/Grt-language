(ns grt.core)

(defn add-line [text l-number]
  (let [number-char "0123456789" 
        text-char "<>+-?!_:qwertyuiopasdfghjklmzxcvbnAZERTYUIOPQSDFGHKLMWXCVBN"
        space-char " " quote-char "'" dot-char "."
        open-bracket-char "(" close-bracket-char ")"
        open-sqbracket-char "[" close-sqbracket-char "]"
        open-hbracket-char "{" close-hbracket-char "}"
        end-others-char (str close-bracket-char open-bracket-char 
                             close-sqbracket-char open-sqbracket-char
                             open-hbracket-char close-hbracket-char
                             space-char)
        text-number-char (str text-char number-char)]
        
    (loop [cur-state :init char-count 0 startword-char-count 0 previous "" rem-text text acc []]
      (let [cur-char (str (first rem-text))]
      (if (empty? rem-text)
        (if ( = cur-state :init)
          acc
          (into acc [(cond 
                       (= cur-state :float-dot-direct) {:type :error :text "bad float ending" :char-pos startword-char-count :l-number l-number}
                       (= cur-state :end-char) {:type :error :text "bad character ending" :char-pos startword-char-count :l-number l-number}
                       (= cur-state :char) {:type :error :text "bad character ending" :char-pos startword-char-count :l-number l-number}
                       :else {:type cur-state :text previous :char-pos startword-char-count :l-number l-number}
                       )]))
          (case cur-state
            :init (cond
                    (.contains number-char cur-char)  (recur :int char-count startword-char-count "" rem-text acc)
                    (.contains text-char cur-char) (recur :id char-count startword-char-count "" rem-text acc)
                    (.contains quote-char cur-char) (recur :char (inc char-count) startword-char-count "'" (rest rem-text) acc)
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
                    true (recur :end-char (inc char-count) startword-char-count (str previous cur-char) (rest rem-text) acc)
                    :else ["unsupported char" cur-char])
            :end-char (cond
                  (.contains quote-char cur-char) (recur :init (inc char-count) (inc char-count) "" (rest rem-text) (into acc [{:type :char :text (str previous "'") :char-pos startword-char-count :l-number l-number}])))))))))

(defn lex-it [full-text]
  (loop [lexies '() remaining-text full-text line-number 0]
    (if 
      (empty? remaining-text)
      lexies 
      (recur (concat lexies (add-line (first full-text) line-number )) (rest remaining-text) (+ line-number 1)))))



(defn parse-by-parentheses [lexed access-index] 
  (if (<= (count lexed)  access-index)
    {:use 0 :size 0 :lex nil :type :parse-error :subtype :empty :text "unvalid parse call on empty lex data" :is-leaf true}
      (let [toHandle (nth lexed access-index)
            current-type (:type toHandle)]
        (case current-type 
          :int           {:use 1 :size 1 :lex toHandle :is-leaf true :type :int}
          :float         {:use 1 :size 1 :lex toHandle :is-leaf true :type :float}
          :id            {:use 1 :size 1 :lex toHandle :is-leaf true :type :id}
          :char          {:use 1 :size 1 :lex toHandle :is-leaf true :type :char}
          :space         {:use 1 :size 0 :lex toHandle :is-leaf true :type :space}
          :error         {:use 1 :size 1 :lex toHandle :is-leaf true :type :lex-error}
          :r-parenthesis {:use 1 :size 1 :lex toHandle :is-leaf true :type :r-parenthesis}
          :r-hbracket    {:use 1 :size 1 :lex toHandle :is-leaf true :type :r-hbracket}
          :r-bracket     {:use 1 :size 1 :lex toHandle :is-leaf true :type :r-bracket}
          :l-parenthesis (loop [current-lexed (inc access-index) acc [] own-use 1]
                           (let [part-of-the-cake (parse-by-parentheses lexed current-lexed)
                                 type (:type part-of-the-cake)
                                 usage (:use part-of-the-cake)]
                             (case type
                               :parse-error
                               (case (:subtype part-of-the-cake)
                                 :empty {:use own-use :size 1 :l-par toHandle :r-par nil :mid acc :type :parse-error :subtype :unclosed-par :is-leaf false}
                                 (recur (+ current-lexed usage) (conj acc part-of-the-cake) (+ own-use usage)))
                               :r-parenthesis {:use (inc own-use) :size 1 :l-par toHandle :r-par (:lex part-of-the-cake) :mid acc :type :parenthesis :is-leaf false}
                               :r-hbracket {:use (inc own-use) :size 1 :l-par toHandle :r-par (:lex part-of-the-cake) :mid acc :type :parse-error :subtype :wrongly-closed-par1 :is-leaf false}
                               :r-bracket  {:use (inc own-use) :size 1 :l-par toHandle :r-par (:lex part-of-the-cake) :mid acc :type :parse-error :subtype :wrongly-closed-par2 :is-leaf false}
                               (recur (+ current-lexed usage) (conj acc part-of-the-cake) (+ own-use usage)))))
          ;[part-of-the-cake type] [:undetected part-of-the-cake type]) ))
        :l-hbracket 
        (loop [current-lexed (inc access-index) acc [] own-use 1]
          (let [part-of-the-cake (parse-by-parentheses lexed current-lexed)
                type (:type part-of-the-cake)
                usage (:use part-of-the-cake)]
            (case type
              :parse-error
              (case (:subtype part-of-the-cake)
                :empty {:use own-use :size 1 :l-par toHandle :r-par nil :mid acc :type :parse-error :subtype :unclosed-par :is-leaf false}
                (recur (+ current-lexed usage) (conj acc part-of-the-cake) (+ own-use usage)))
              :r-hbracket {:use (inc own-use) :size 1 :l-par toHandle :r-par (:lex part-of-the-cake) :mid acc :type :hbracket :is-leaf false}
              :r-parenthesis {:use (inc own-use) :size 1 :l-par toHandle :r-par (:lex part-of-the-cake) :mid acc :type :parse-error :subtype :wrongly-closed-par3 :is-leaf false}
              :r-bracket  {:use (inc own-use) :size 1 :l-par toHandle :r-par (:lex part-of-the-cake) :mid acc :type :parse-error :subtype :wrongly-closed-par4 :is-leaf false}
                 (recur (+ current-lexed usage) (conj acc part-of-the-cake) (+ own-use usage)))))
        :l-bracket (loop [current-lexed (inc access-index) acc [] own-use 1]
                    (let [part-of-the-cake (parse-by-parentheses lexed current-lexed)
                          type (:type part-of-the-cake)
                          usage (:use part-of-the-cake)]
                      (case type
                        :parse-error
                        (case (:subtype part-of-the-cake)
                          :empty {:use own-use :size 1 :l-par toHandle :r-par nil :mid acc :type :parse-error :subtype :unclosed-par :is-leaf false}
                          (recur (+ current-lexed usage) (conj acc part-of-the-cake) (+ own-use usage))) 
                        :r-bracket {:use (inc own-use) :size 1 :l-par toHandle :r-par (:lex part-of-the-cake) :mid acc :type :bracket :is-leaf false}
                        :r-parenthesis {:use (inc own-use) :size 1 :curlex current-lexed :pcake part-of-the-cake :l-par toHandle :r-par (:lex part-of-the-cake) :mid acc :type :parse-error :subtype :wrongly-closed-par5 :is-leaf false}
                        :r-hbracket  {:use (inc own-use) :size 1 :l-par toHandle :r-par (:lex part-of-the-cake) :mid acc :type :parse-error :subtype :wrongly-closed-par6 :is-leaf false}
                         (recur (+ current-lexed usage) (conj acc part-of-the-cake) (+ own-use usage)))))))))


(defn build-identifier-navigation [current-count remainder]
  (if (:is-leaf remainder)
    {:next-count (inc current-count) :annotated (assoc remainder :id current-count)}
    (let [[new-count updated-mid]
          (loop [sub-current-count (inc current-count) remaining (:mid remainder) acc []]
            (if (empty? remaining)
              [sub-current-count acc]
              (let [sub-nav (build-identifier-navigation sub-current-count (first remaining))
                    updated-count (:next-count sub-nav)
                    updated-sub-nav (:annotated  sub-nav)]
                (recur updated-count (rest remaining) (into acc [updated-sub-nav])))))]
      {:next-count new-count :annotated (assoc (assoc remainder :mid updated-mid) :id current-count)})))



(comment 
         (is  (add-line "(" 0) [{:type :l-bracket, :text "(", :char-pos 0, :l-number 0}]))