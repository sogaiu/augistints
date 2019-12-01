(ns augistints.utils
  (:refer-clojure :exclude
   [string? symbol?])
  (:require
   [clojure.string :as cs]
   [rewrite-cljc.node :as rn]
   [rewrite-cljc.zip :as rz]))

(alias 'ag 'augistints.utils)

;; XXX: hopefully something similar will show up in rewrite-cljc
;; XXX: put in another namespace so this can be named uneval? as well?
(defn uneval-node?
  "Returns true if node represents an uneval, else false."
  [node]
  (= (rn/tag node) :uneval))

(defn uneval?
  "Returns true if zipper represents an uneval, else false."
  [zloc]
  (some-> zloc rz/node uneval-node?))

;; following things will throw when sexpr is called:
;;
;;   comma,
;;   newline,
;;   whitespace
;;
;;   comment
;;
;;   uneval
;;
;; sometimes, this will throw:
;;
;;   some reader nodes
;;
;; however, currently all reader nodes don't throw by default
;;
;; XXX: hopefully something similar will show up in rewrite-cljc
;; XXX: put in another namespace so this can be named sexprable? as well?
(defn sexprable-node?
  "Returns true if `sexpr' can be called safely on node, else false."
  [node]
  (not (or (uneval-node? node)
           (rn/whitespace? node)
           (rn/comment? node))))

(defn sexprable?
  "Returns true if `sexpr' can be called safely on zipper, else false."
  [zloc]
  (some-> zloc rz/node sexprable-node?))

(defn string-value
  "Return the string value for a node."
  [node]
  (when-let [lines (:lines node)]
    (cs/join "\n" lines)))

;; XXX: put in another namespace so this can be named string? as well?
(defn string-node?
  "Returns true if node represents a string, else false."
  [node]
  (string-value node))

(defn string?
  "Returns true if zipper represents a string, else false."
  [zloc]
  (some-> zloc rz/node string-node?))

(defn symbol-value
  "Return the symbol value for a node."
  [node]
  (:value node))

;; XXX: put in another namespace so this can be named symbol? as well?
(defn symbol-node?
  "Returns true if node represents a symbol, else false."
  [node]
  (clojure.core/symbol? (symbol-value node)))

(defn symbol?
  "Returns true if zipper represents a symbol, else false."
  [zloc]
  (some-> zloc rz/node symbol-node?))

(defn first-in-list-node
  "Returns first node in list node, else nil."
  [l-node]
  (when (= :list (rn/tag l-node))
    (-> l-node
        :children
        first)))

(defn let-form?
  "Returns true if zipper is a non-trivial let form, else nil."
  [zloc]
  (when (and (rz/list? zloc)
             (> (rz/length zloc) 1))
    (let [f-node (first-in-list-node (rz/node zloc))]
      (when (and (symbol-node? f-node)
                 (= 'let (symbol-value f-node)))
        true))))

;; XXX: it's possible to have metadata preceding 'defn...
(defn defn?
  [zloc]
  (let [a-node (rz/node zloc)]
    (and (symbol-node? a-node)
         (some #{'defn 'defn-}
               [(symbol-value a-node)]))))

(defn let?
  [zloc]
  (let [a-node (rz/node zloc)]
    (and (symbol-node? a-node)
         (= 'let (symbol-value a-node)))))

;; make safe for use with sexpr by replacing values to bind to with
;; values that are safe for use with sexpr
;;
;; [x 1 y 2/3 z 8/11] ;; not safe for cljs because of rational values
;; =>
;; [x 0 y 0 z 0] ;; safe for consumption by cljs
;;
;; [[top bottom] [2 3/4] y 0 {:keys [bob alice]} {:bob 1 :alice 2}]
;; =>
;; [[top bottom] 0 y 0 {:keys [bob alice]} 0] ; not "right", but safe for calling sexpr
(defn sanitize-bindings
  [b-zloc]
  (let [binding-nodes (filter sexprable-node?
                              (:children (rz/node b-zloc)))
        binding-form-nodes (keep-indexed (fn [idx item]
                                           (when (even? idx)
                                             item))
                                         binding-nodes)]
    (->> (interleave binding-form-nodes
                     (repeat (rn/coerce 0))) ; 0 should be safe
         (interpose (rn/whitespace-node " "))
         rn/vector-node
         rz/edn)))

(defn find-node-type?
  "Return first node of type node-type from coll, if any."
  [coll node-type]
  (->> coll
       (filter #(= (rn/tag %) node-type))
       first))

;; XXX: needs testing?
;; XXX: tricky...may be rethink
(defn name-from-ns-node
  "Return name of ns, given node for ns form."
  [ns-form-node]
  (let [kids (:children ns-form-node)]
    (if-let [?meta-node (find-node-type? kids :meta)]
      (loop [node ?meta-node]
        (let [kids (:children node)]
          (if-let [?node
                   (find-node-type? kids :meta)]
            (recur ?node)
            (:value (find-node-type? (reverse kids) :token))))) ; last token
      (:value (find-node-type? (rest kids) :token))))) ; skip 'ns

(comment

  (require
   '[augistints.let :as al]
   '[augistints.samples :as as]
   '[rewrite-cljc.node :as rn]
   '[rewrite-cljc.zip :as rz]
   :reload-all)

  ^{:ael/want true}
  (uneval-node? (-> (rz/of-string "#_(+ 1 1)")
                    rz/node))

  ^{:ael/want true}
  (uneval? (rz/of-string "#_(+ 1 1)"))

  ^{:ael/want false}
  (sexprable-node? (-> (rz/of-string "#_(+ 1 1)")
                       rz/node))

  ^{:ael/want false}
  (sexprable? (rz/of-string "#_(+ 1 1)"))

  ;; (<comment: ";; hi there\n">
  ;;  <list: (+ 1 1)>
  ;;  <newline: "\n\n">
  ;;  <whitespace: "  ">
  ;;  <list: (- 1 1)>)
  (def test-form
    (str ";; hi there\n"
         "(+ 1 1)\n\n"
         "  (- 1 1)"))

  ^{:ael/want '(false true false false true)}
  (let [nodes (-> (rz/of-string test-form)
                  rz/up
                  rz/node
                  rn/children)]
    (map #(sexprable-node? %)
         nodes))

  ^{:ael/want [x 0 y 0 z 0]}
  (rz/string
   (sanitize-bindings
    (-> "(let [x 1 y 2/3 z 8/11] 0)"
        rz/of-string
        al/study-let
        :bindings)))

  ^{:ael/want [[top bottom] 0 y 0 {:keys [bob alice]} 0]}
  (rz/string
   (sanitize-bindings
    (-> "(let [[top bottom] [2 3/4] y 0 {:keys [bob amy]} {:bob 1 :amy 2}] 1)"
        rz/of-string
        al/study-let
        :bindings)))

  ^{:ael/want 'hello}
  (name-from-ns-node (-> (rz/of-string "(ns hello)")
                         rz/node))

  ^{:ael/want 'hello}
  (name-from-ns-node (-> (rz/of-string "(ns ^:no-doc hello)")
                         rz/node))

  ^{:ael/want 'hello}
  (name-from-ns-node (-> (rz/of-string "(ns ^:no-doc ^{:fun 1} hello)")
                         rz/node))

  (def clj-kondo-core-ns-form-str
    (str "(ns clj-kondo.core\n"
         "  (:refer-clojure :exclude [run!])\n"
         "  (:require\n"
         "    [clj-kondo.impl.cache :as cache]\n"
         "    [clj-kondo.impl.core :as core-impl]\n"
         "    [clj-kondo.impl.linters :as l]\n"
         "    [clj-kondo.impl.overrides :refer [overrides]]\n"
         "    [clojure.java.io :as io]\n"
         "    [clojure.string :as str]))"))
  
  ^{:ael/want 'clj-kondo.core}
  (name-from-ns-node (-> (rz/of-string clj-kondo-core-ns-form-str)
                         rz/node))

  ^{:ael/want 'clojure.core}
  (name-from-ns-node
   (-> (rz/of-string "(ns ^{:doc \"The core Clojure language.\"
                            :author \"Rich Hickey\"}
                        clojure.core)")
       rz/node))

  ^{:ael/want 'rewrite-cljc.node.protocols}
  (name-from-ns-node
   (-> (rz/of-string
        "(ns ^:no-doc ^{:added \"0.4.0\"} rewrite-cljc.node.protocols
           (:require [clojure.string :as string]
                     [rewrite-cljc.interop :as interop]
                     #?(:clj [rewrite-cljc.potemkin.clojure 
                              :refer [defprotocol+]]))
           #?(:cljs (:require-macros [rewrite-cljc.potemkin.cljs 
                                      :refer [defprotocol+]])))")
       rz/node))

  )
