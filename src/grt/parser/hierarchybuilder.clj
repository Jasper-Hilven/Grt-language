(ns grt.parser.hierarchybuilder)
;;;;;;;;;;;Hierarchy building;;;;;;;;;;;;;;;
(defn parse-by-parentheses-rec [lexed access-index] 
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
          :error         {:use 1 :size 1 :lex toHandle :is-leaf true :type :parse-error :subtype :lex-error}
          :r-parenthesis {:use 1 :size 1 :lex toHandle :is-leaf true :type :r-parenthesis}
          :r-hbracket    {:use 1 :size 1 :lex toHandle :is-leaf true :type :r-hbracket}
          :r-bracket     {:use 1 :size 1 :lex toHandle :is-leaf true :type :r-bracket}
          :l-parenthesis (loop [current-lexed (inc access-index) acc [] own-use 1]
                           (let [part-of-the-cake (parse-by-parentheses-rec lexed current-lexed)
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
          (let [part-of-the-cake (parse-by-parentheses-rec lexed current-lexed)
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
                    (let [part-of-the-cake (parse-by-parentheses-rec lexed current-lexed)
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
(defn parse-by-parentheses [lexed] (parse-by-parentheses-rec lexed 0)) 

(defn build-identifier-navigation-rec [current-count remainder parent-id]
  (if (:is-leaf remainder)
    {:next-count (inc current-count) :annotated (assoc remainder :id current-count :parent-id parent-id)}
    (let [[new-count updated-mid]
          (loop [sub-current-count (inc current-count) remaining (:mid remainder) acc []]
            (if (empty? remaining)
              [sub-current-count acc]
              (let [sub-nav (build-identifier-navigation-rec sub-current-count (first remaining) current-count)
                    updated-count (:next-count sub-nav)
                    updated-sub-nav (:annotated  sub-nav)]
                (recur updated-count (rest remaining) (into acc [updated-sub-nav])))))]
      {:next-count new-count :annotated (assoc (assoc remainder :mid updated-mid) :id current-count :parent-id parent-id)})))

(defn build-identifier-navigation [ast] (:annotated (build-identifier-navigation-rec 1 ast 0)))
