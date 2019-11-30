(ns augistints.gen
  (:require
   [augistints.names :as an]
   [rewrite-cljc.zip :as rz]))

(alias 'ag 'augistints.gen)

(defn log-defn-entry-gen
  [studied]
  (let [fn-name (rz/string (:fn-name studied))]
    (case (:arity studied)
      :arity-1
      {:arity-1 `(println (str "entered: " ~fn-name))}
      :arity-n
      {:arity-n (map (fn [_]
                       `(println (str "entered: " ~fn-name)))
                     (:bodies studied))})))

;; single-arity only
(defn fn-log-map
  [{:keys [fn-name form-type params] :as studied}]
  (let [names (-> params
                  rz/sexpr
                  an/names-from-params)]
    ;; XXX: string or keyword or something else?
    {:fn-name (rz/string fn-name)
     :form-type (rz/string form-type)
     :params-map (an/names-map-from-names names)}))

;; for multi-arity functions
(defn fn-log-maps
  [{:keys [fn-name form-type paramss] :as studied}]
  ;; XXX: string or keyword or something else?
  (let [fn-name (rz/string fn-name)
        form-type (rz/string form-type)]
    (map (fn [params]
           (let [names (-> params
                           rz/sexpr
                           an/names-from-params)]
             {:fn-name fn-name
              :form-type form-type
              :params-map (an/names-map-from-names names)}))
         paramss)))

(defn log-defn-args-gen
  [studied]
  (case (:arity studied)
    :arity-1
    {:arity-1 (list 'tap> (fn-log-map studied))}
    :arity-n
    {:arity-n (map (fn [a-log-map]
                     (list 'tap> a-log-map))
                   (fn-log-maps studied))}))

;; XXX: be defensive by checking for existence of :fn-name
(defn log-map
  [{:keys [fn-name form-type bindings] :as studied}]
  (let [names (->> bindings
                   rz/sexpr
                   an/names-from-bindings)]
    {:fn-name fn-name
     :form-type (rz/string form-type)
     :params-map (an/names-map-from-names names)}))

(defn log-let-bindings-gen
  [studied]
  (list 'tap> (log-map studied)))

;; XXX: check
(defn interleave-bindings-gen
  [{:keys [fn-name form-type bindings] :as studied}]
  (reduce (fn [acc [destr-form init-expr]]
            (let [names (an/names-from-destr-form destr-form)
                  log-info {:fn-name fn-name
                            :form-type (rz/string form-type)
                            :binding-map (an/names-map-from-names names)}]
              (conj acc
                    destr-form init-expr
                    '_ (list 'tap> log-info))))
          []
          (partition 2 (rz/sexpr bindings))))

;; single-arity only
(defn fn-def-map
  [{:keys [params] :as studied}]
  (let [names (-> params
                  rz/sexpr
                  an/names-from-params)]
    (map (fn [name]
           (list 'def name name))
         names)))

;; for multi-arity functions
(defn fn-def-maps
  [{:keys [paramss] :as studied}]
  ;; XXX: string or keyword or something else?
  (map (fn [params]
         (let [names (-> params
                         rz/sexpr
                         an/names-from-params)]
           (map (fn [name]
                  (list 'def name name))
                names)))
       paramss))

(defn inline-def-gen
  [studied]
  (let [fn-name (rz/string (:fn-name studied))]
    (case (:arity studied)
      :arity-1
      {:arity-1 (cons 'do (fn-def-map studied))}
      :arity-n
      {:arity-n (map (fn [a-defs-list]
                       (cons 'do a-defs-list))
                     (fn-def-maps studied))})))

(defn make-inline-def-with-meta-gen
  [meta-map]
  (fn [studied]
    (let [fn-name (rz/string (:fn-name studied))]
      (case (:arity studied)
        :arity-1
        {:arity-1 (with-meta (cons 'do (fn-def-map studied))
                    meta-map)}
        :arity-n
        {:arity-n (map (fn [a-defs-list]
                         (with-meta (cons 'do a-defs-list)
                           meta-map))
                       (fn-def-maps studied))}))))

(comment

  (require
   '[augistints.gen :as ag]
   '[augistints.let :as al]
   '[augistints.samples :as as]
   '[rewrite-cljc.zip :as rz]
   :reload-all)

  ^{:ael/want :arity-1}
  (:arity as/studied)

  ^{:ael/want true}
  (:found-meta as/studied)

  ^{:ael/want false}
  (:found-docstring as/studied)

  ^{:ael/want '{:fn-name my-meta-pre-post-fn,
                :form-type defn,
                :params-map {:f f, :x x}}
    :ael/name "generate map for logging a single arity defn's arguments"}
  (ag/fn-log-map as/studied)

  ^{:ael/want '({:fn-name my-meta-pre-post-fn,
                 :form-type defn,
                 :params-map {:f f, :x x}}
                {:fn-name my-meta-pre-post-fn,
                 :form-type defn,
                 :params-map {}})
    :ael/name "generate map for logging a multiple arity defn's arguments"}
  (ag/fn-log-maps as/ma-studied)

  ^{:ael/want '[x 1
                _ (tap> {:fn-name "my-test-fn-name",
                         :form-type "let",
                         :binding-map {:x x}})
                y 2
                _ (tap> {:fn-name "my-test-fn-name",
                         :form-type "let",
                         :binding-map {:y y}})
                z (+ 1 2)
                _ (tap> {:fn-name "my-test-fn-name",
                         :form-type "let",
                         :binding-map {:z z}})]}
  (let [l-studied (-> as/l-form-str
                      rz/of-string
                      al/study-let)
        tweaked-studied (assoc l-studied
                               :fn-name "my-test-fn-name")]
    (-> tweaked-studied
        ag/interleave-bindings-gen))

  ^{:ael/want '((def f f)
                (def x x))}
  (ag/fn-def-map as/studied)

  ^{:ael/want '{:arity-1 (do
                           (def f f)
                           (def x x))}}
  (ag/inline-def-gen as/studied)

  ^{:ael/want '{:arity-n ((do
                            (def f f)
                            (def x x))
                          (do))}}
  (ag/inline-def-gen as/ma-studied)
  
  )
