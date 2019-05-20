(ns augistints.defn
  (:require
   [rewrite-clj.zip :as rz]))

(alias 'ad 'augistints.defn)

;; from defn docs

;; single arity
;; (defn name doc-string? attr-map? [params*] prepost-map? body)

;; multi-arity
;; (defn name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?)

;; successful study should result in:
;;
;; {:form-type <zloc>
;;  :fn-name <zloc>
;;  :docstring <zloc>
;;  :meta <zloc>
;;  :found-... ...
;;  :zloc ...}
(defn study-defn-to-tail
  [zloc]
  (let [{:keys [zloc] :as m}
        {:root-zloc zloc
         :zloc (rz/down zloc)}
        {:keys [zloc] :as m}
        (merge m
               (if (some #{'defn 'defn-}
                         [(rz/sexpr zloc)])
                 {:zloc (rz/right zloc)}
                 (throw (#?(:cljs js/Error. :default Exception.)
                         (str "Expected defn or defn-, but got: "
                              (rz/sexpr zloc)))))
               {:form-type zloc})
        {:keys [zloc] :as m}
        (merge m
               (if (symbol? (rz/sexpr zloc))
                 {:zloc (rz/right zloc)}
                 (throw (#?(:cljs js/Error. :default Exception.)
                         (str "Expected symbol, but got: "
                              (rz/sexpr zloc)))))
               {:fn-name zloc})
        {:keys [zloc] :as m}
        (merge m
               (if (string? (rz/sexpr zloc))
                 {:found-docstring true
                  :zloc (rz/right zloc)}
                 {:found-docstring false})
               {:docstring zloc})
        {:keys [zloc] :as m}
        (merge m
               (if (map? (rz/sexpr zloc))
                 {:found-meta true
                  :zloc (rz/right zloc)}
                 {:found-meta false})
               {:meta zloc})]
    m))

(defn fn-tail-arity
  [{:keys [zloc] :as studying}]
  (cond
    (rz/vector? zloc) :arity-1
    (rz/list? zloc) :arity-n
    :else nil))

;; successful study should result in:
;;
;; {:params ...
;;  :prepost ...
;;  :body ...
;;  :found-prepost ...
;;  :zloc ...}
(defn study-params+body
  [{:keys [zloc] :as studying}]
  (let [{:keys [zloc] :as m} {:params zloc
                              :zloc (rz/right zloc)}
        {:keys [zloc] :as m}
        (merge studying
               m
               (if (and (map? (rz/sexpr zloc))
                        (not (nil? (rz/right zloc))))
                 {:found-prepost true
                  :zloc (rz/right zloc)}
                 {:found-prepost false
                  :zloc zloc})
               {:prepost zloc})]
    (assoc m
           :body zloc)))

(defn study-multi-arity-defn-tail-attr-map
  [{:keys [zloc] :as studying}]
  (let [tail (rz/rightmost zloc)]
    (merge studying
           (if (rz/map? tail)
             {:found-attr-map-2 true}
             {:found-attr-map-2 false})
           {:attr-map-2 tail})))

(defn study-defn
  [d-zloc]
  (let [{:keys [zloc] :as studying} (study-defn-to-tail d-zloc)
        arity (fn-tail-arity studying)]
    (case arity
      :arity-1
      (merge studying
             (study-params+body studying)
             {:arity :arity-1})
      :arity-n
      (let [m (study-multi-arity-defn-tail-attr-map studying)
            [paramss preposts bodies]
            (loop [zloc zloc
                   [paramss preposts bodies] [[] [] []]]
              (if (or (rz/end? zloc)
                      (not (rz/list? zloc)))
                [paramss preposts bodies]
                (let [{:keys [params prepost body]}
                      (study-params+body {:zloc
                                          (rz/down zloc)})]
                  (recur (rz/right zloc)
                         [(conj paramss params)
                          (conj preposts prepost)
                          (conj bodies body)]))))]
        (merge studying
               m
               {:paramss paramss
                :preposts preposts
                :bodies bodies
                :arity :arity-n})))))

(comment

  (require
   '[augistints.defn :as ad]
   '[augistints.gen :as ag]
   '[augistints.names :as an]
   '[augistints.samples :as as]
   '[rewrite-clj.node :as rn]
   '[rewrite-clj.zip :as rz]
   :reload-all)

  ^{:ael/want '#{:arity :body :docstring :fn-name :form-type
                 :found-docstring :found-meta :found-prepost :meta
                 :params :prepost :root-zloc :zloc}}
  (set (keys as/studied))

  ^{:ael/want '#{:arity :attr-map-2 :bodies :docstring :fn-name
                 :form-type :found-attr-map-2 :found-docstring :found-meta
                 :meta :paramss :preposts :root-zloc :zloc}}
  (set (keys as/ma-studied))

  ^{:ael/want '(f x)}
  (-> (:params as/studied)
      rz/sexpr
      an/names-from-params)

  ;; insertion tests
  
  (def insertion-form
    (list 'tap> :wave))
  
  ;; ends up with metadata
  (-> (:body as/studied)
      (rz/insert-left '(tap> :smile))
      rz/root-string
      print)

  (def log-form
    (list 'tap> (ag/fn-log-map as/studied)))

  (def inserted-form
"(defn my-meta-pre-post-fn
  {:fun-meta \"smile!\"}
  [f x]
  {:pre [(pos? x)]
   :post [(= % (* 2 x))]}
  (tap> {:fn-name \"my-meta-pre-post-fn\", :form-type \"defn\", :params-map {:f f, :x x}}) (f x)
  true)
")

  ;; insert w/o metadata
  ^{:ael/name "Insert logging form at head of body"}
  (-> (:body as/studied)
      (rz/insert-left log-form)
      rz/root-string
      (= inserted-form))

  ^{:ael/want inserted-form}
  (-> (:body as/studied)
      (rz/insert-left log-form)
      rz/root-string)
  
  ;; XXX: test this
  ^{:ael/want
"(defn my-meta-pre-post-fn
  {:fun-meta \"smile!\"}
  [f x]
  {:pre [(pos? x)]
   :post [(= % (* 2 x))]}
  (tap> {:fn-name \"my-meta-pre-post-fn\", :form-type \"defn\", :params-map {:f f, :x x}}) (f x)
  true)
"}
  (-> (:body as/studied)
      (rz/insert-left log-form)
      rz/root-string)

  ;; edit + splice approach
  (-> (:body as/studied)
      (rz/edit (fn [sexpr]
                 [log-form sexpr]))
      rz/splice
      rz/root-string
      print)

  ;; XXX: custom function insert-left-with-nl in zip/insert.clj
  ;;
  ;; (def ^:private newline
  ;;   (node/newlines 1))
  ;;
  ;; ...
  ;;
  ;; (defn insert-left-with-nl
  ;;  "Insert item to the right of the left location. Will insert a space if necessary."
  ;;  [zloc item]
  ;;  (insert
  ;;   z/left
  ;;   z/insert-left
  ;;   [newline]
  ;;   zloc item))
  ;;
  ;; (-> (:body as/studied)
  ;;     (rz/insert-left-with-nl insertion-form)
  ;;     rz/root-string
  ;;     print)

  (-> (:body as/studied)
      (rz/insert-left (rn/spaces 1))
      rz/root-string
      print)

  (-> (:body as/studied)
      (rz/insert-left (rn/newlines 1))
      rz/root-string
      print)

  (-> (:body as/studied)
      rz/root-string
      print)

  (-> as/ma-d-form-str
      rz/of-string
      ad/study-defn
      :bodies
      (nth 0)
      (rz/insert-left "hi")
      (rz/insert-left (rn/newlines 1))
      rz/root-string
      rz/of-string
      ad/study-defn ; study again
      :bodies
      (nth 1)
      (rz/insert-left "bye")
      (rz/insert-left (rn/newlines 1))
      rz/root-string
      print)

  )
