;;;;;;;;;;;;;;Symbol table;;;;;;;;;;;;
(ns grt.symboltabler)

(defn build-let-symbol [AST previous-defined-table] nil )
(defn build-function-symbol [] nil)
(defn build-tree-symbol-acc [] nil)
(defn build-empty-symbol-table [] nil)

(defn build-let-symbol-table 
  [AST previous-defined-table previous-defined-id acc] 
  (let [updated-wth-let-function
        (cond 
          ;(is-let? AST :d) (assoc acc (AST :id) (build-let-symbol AST))
          ;(is-function? AST :p) (assoc acc (AST :id) (build-function-symbol))
          :else acc)]
    (if (AST :is-leaf)
      acc
      (build-tree-symbol-acc ))))

;(defn build-symbol-table 
;  [namespace-id define-type-id s-let-id AST current-symbol-table-id acc] 
;  (let [let-table (fn [AST] (throw (Exception. "notimplemented")))
;        function-table (fn [AST] (throw (Exception. "notimplemented")))
;        merge-children (fn [AST acc] (throw (Exception. "notimplemented")))] 
;  (if (AST :is-leaf) 
;    (assoc acc (AST :id) current-symbol-table-id)
;    (cond
;      (= (AST :type) :bracket) (merge-children AST acc)
;      (= (AST :type) :hbracket) (merge-children AST acc)
;      (is-let? AST)  (merge-children AST (assoc acc (AST :id) (let-table AST)))
;     (is-function? AST)  (merge-children AST (assoc acc (AST :id) (function-table AST)))
;      :else (throw (Exception. "Unexpected symbol"))))))