(ns grt.parser.elementmatcher)
;;;;;;;;;;;;;;;;;;;;;Element matching;;;;;;;;;;;;;;;
(defn first-containing [AST group-type] (and 
                                          (= (AST :type) group-type)
                                          (not-empty (AST :mid))
                                          (= :id ((first (AST :mid)) :subtype))
                                          (first (AST :mid))))

(defn remove-spaces [remainder] 
  (if (remainder :is-leaf) remainder 
    (assoc remainder :mid (filter #(not= (% :type) :space) (remainder :mid)))))

(defmacro please-use-me [ST parse-func] 
  (if (ST :is-leaf) 
    ST
    (assoc ST :mid (for [element (ST :mid)] (parse-func element)))))
;;;;;;;;;;;;FUNCTION;;;;;;;;;
(defn intend-function? [ST] 
  
     (let [ qualifier (first-containing ST :func-eval)] 
       (and qualifier 
            (= "fn" ((qualifier :lex):text))
            (= (count (ST :mid)) 4)
            (= ((nth (ST :mid) 2) :type) :array)
       )))

(defn annotate-intended-functions [ST] 
  (let [children-handled (if (ST :is-leaf) ST
    (assoc ST :mid  
                     (for [element (ST :mid)] 
                       (annotate-intended-functions element))))] 
    (if 
      (intend-function? children-handled)  
      (assoc children-handled 
               :type :function)
      children-handled)))
(defn valid-function? [ST] true)

(defn get-function-association [ST] 
  (if 
    (valid-function? ST)  
    (dissoc (assoc ST 
                   :def (first (ST :mid))
                   :name (second (ST :mid))
                   :parameters (nth (ST :mid) 2)
                   :value (nth (ST :mid) 3)) :mid)
    (throw "handle invalid function")))
      

;;;;;;;;;;;;LET;;;;;;;;;;;;;;
(defn intend-let? [ST] 

     (let [qualifier (first-containing ST :func-eval)] 
       (and qualifier 
            (= "let" ((qualifier :lex) :text))
            (= (count (ST :mid)) 3)
            (= ((nth (ST :mid) 1) :type) :array))))

(defn annotate-intended-lets [ST] 
  (let [children-handled (if (ST :is-leaf) ST
    (assoc ST :mid 
                     (for [element (ST :mid)] 
                       (annotate-intended-lets element))))] 
      (if (intend-let? children-handled)  
          (assoc children-handled
               :type :let)
        children-handled)))

(defn valid-let? [ST] true)

(defn get-let-association [ST] 
  (if 
    (valid-let? ST)
    (dissoc (assoc
                     ST
                     :let (first (ST :mid))
                     :defns (second (ST :mid))
                     :result (nth (ST :mid) 2)) :mid)
    (throw "not implemented build error let")))
;;;;;;;;;FUNCTION EVALUATION;;;;;;;;

(defn intend-function-evaluation? [ST] 
    (= (ST :type) :parenthesis))

(defn valid-function-evaluation? [ST] 
  (and 
    (not= (ST :is-leaf))
    (not-empty (ST :mid))))


(defn annotate-intended-function-evaluation [ST]
  (let [children-handled (if (ST :is-leaf) ST
    (assoc ST :mid  
                     (for [element (ST :mid)] 
                       (annotate-intended-function-evaluation element))))] 
      (if (intend-function-evaluation? children-handled)  
        (assoc children-handled 
               :type :func-eval)
        children-handled)))
(defn get-func-eval-association [ST] 
  (if (valid-function-evaluation? ST)
    (dissoc (assoc 
                           ST
                           :call-func (first (ST :mid))
                           :arguments (rest (ST :mid)))
                         :mid)
    (throw "invalidity")))

;;;;;;;;;;;ARRAYS;;;;;;;;

(defn get-array-association [ST]
  (dissoc (assoc 
      ST :elements (ST :mid)) :mid))
(defn is-an-array? [ST] 
  (= (:type ST) :bracket))

(defn annotate-intended-arrays [ST] 
    (let [children-handled (if (ST :is-leaf) ST
    (assoc ST :mid 
                     (for [element (ST :mid)] 
                       (annotate-intended-arrays element))))] 
  (if (is-an-array? children-handled)
      (assoc children-handled
             :type :array)
      children-handled)))
;;;;;;;;;;;;;;DICTIONARY;;;;;;;;;;;;

(defn intended-dictionary? [ST] (= (ST :type) :hbracket))
(defn annotate-intended-dictionary-likes [ST] 
    (let [children-handled (if (ST :is-leaf) ST
    (assoc ST :mid  
                     (for [element (ST :mid)] 
                       (annotate-intended-dictionary-likes element))))] 
      (if (intended-dictionary? children-handled)  
        (assoc children-handled 
               :type :dictionary)
      children-handled)))
(defn valid-dictionary-association? [ST] true)

(defn get-dictionary-association [ST]  
  (if (valid-dictionary-association? ST)
    (dissoc 
      (assoc ST :keyvalues (ST :mid)) 
      :mid)
    (throw "implement error")))


;;;;;;;lEAF VALUES;;;;;;;;;;;
(defn parse-leaf-values [ST] 
    (let [children-handled 
          (if (ST :is-leaf) ST
            (assoc ST :mid 
                             (map parse-leaf-values (ST :mid) )))] 
        (if (= (children-handled :type) :parse-error)
      children-handled
      (if (children-handled :is-leaf)
        (assoc children-handled 
               :type :leaf-val
               :subtype (children-handled :type))
        children-handled))))
;;;;;;;CORE;;;;;;;;;;;;
(defn annotate-values [ST]
  (if (ST :is-leaf)
    (parse-leaf-values ST)   ;;Leafs are already done by the parse-leaf-values
    (let [children-handled 
          (if (ST :is-leaf) ST
            (assoc ST :mid 
                   (map annotate-values (ST :mid))))] 
      (case (children-handled :type)
        :function (get-function-association children-handled)
        :dictionary  (get-dictionary-association children-handled)
        :array  (get-array-association children-handled)
        :func-eval (get-func-eval-association children-handled)
        :let (get-let-association children-handled)
        (throw "other")))))

(defn match-elements [ST]
  (-> ST 
    (parse-leaf-values)
    (annotate-intended-arrays)
    (annotate-intended-dictionary-likes)
    (annotate-intended-function-evaluation) 
    (annotate-intended-functions)
    (annotate-intended-lets)
    (annotate-values)
))
