(ns grt.parser.elementmatching)
;;;;;;;;;;;;;;;;;;;;;Element matching;;;;;;;;;;;;;;;
(defn first-containing [AST] (and (= (AST :type) :parenthesis)
                                        (not-empty (AST :mid))
                                        (= :id ((first (AST :mid)) :type))
                                        (first (AST :mid))))

(defn is-let? [ST s-let-id] 
  (= (ST :type) 
     (let [qualifier (first-containing ST)] 
       (and qualifier 
            (= s-let-id (qualifier :text))
            (= (count (ST :mid)) 3)
            (= (((ST :mid) 1) :type) :bracket)))))

(defn is-function? [ST function-id] 
  (= (ST :type) 
     (let [ qualifier (first-containing ST)] 
       (and qualifier 
            (= function-id (qualifier :text))
            (= (count (ST :mid)) 4)
            (= (((ST :mid) 2) :type) :bracket)))))

(defn remove-spaces [remainder] 
  (if (remainder :is-leaf) remainder 
    (assoc remainder :mid (filter #(not= (% :type) :space) (remainder :mid)))))


(defn handle-children [ST parse-func] 
  (if (ST :is-leaf)
    ST
    (handle-children :mid ST) ))
(defn parse-functions [ST] 
  (let [children-handled (handle-children ST parse-functions)] 
    (if (is-function? ST)  
        (assoc ST 
               :type :function 
      ST)))
(defn parse-lets [ST] 
    (let [children-handled (handle-children ST parse-lets)]))

(defn parse-function-evaluation [ST] ST
    (let [children-handled (handle-children ST parse-function-evaluation)]))
(defn parse-arrays [ST] 
  (let [children-handled (handle-children ST parse-arrays)]))
(defn parse-dictionary-likes [ST] ST)
(defn parse-r-values [ST] ST)
(defn annotate-values [ST]
  (if (ST :is-leaf)
    ST
    (
    (case (ST :type)
      :function (assoc ST 
                   :def (first (ST :mid))
                   :name (second (ST :mid))
                   :parameters (nth (ST :mid) 2)
                   :value (nth (ST :mid)3))))))
(defn match-elements [ST]
  (let[parentheses-lets      (parse-lets ST)
        parentheses-functions (parse-functions parentheses-lets)
        function-evaluation   (parse-function-evaluation parentheses-functions)
        arrays                (parse-arrays function-evaluation)
        dictionary-likes      (parse-dictionary-likes arrays)
        r-values              (parse-r-values dictionary-likes)
        annotated-values       (annotate-values r-values)] annotated-values ))
