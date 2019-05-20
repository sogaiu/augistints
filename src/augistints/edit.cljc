(ns augistints.edit
  (:require
   [augistints.defn :as ad]
   [augistints.let :as al]
   [rewrite-clj.node :as rn]
   [rewrite-clj.zip :as rz]))

(alias 'ae 'augistints.edit)

(defn- prepend-to-defn-body-1
  [studied ins-form]
  (-> (:body studied)
      (rz/insert-left ins-form)
      (rz/insert-left (rn/newlines 1))
      rz/root-string))

;; XXX: how to do this w/o repeatedly studying...
(defn- prepend-to-defn-bodies-1
  [defn-str ins-forms]
  (let [n-forms (count ins-forms)]
    (loop [i 0
           a-defn-str defn-str
           ins-forms ins-forms]
      (if (= i n-forms)
        a-defn-str
        (let [ins-form (first ins-forms)
              new-form-str (-> a-defn-str
                               rz/of-string
                               ad/study-defn
                               :bodies
                               (nth i)
                               (rz/insert-left ins-form)
                               (rz/insert-left (rn/newlines 1))
                               rz/root-string)]
          (recur (inc i)
                 new-form-str
                 (rest ins-forms)))))))

;; assumes that form-gen-fn doesn't generate any forms that contain
;; defn or defn-
(defn prepend-to-defn-body
  [defn-str form-gen-fn]
  (let [{:keys [arity] :as studied} (-> defn-str
                                        rz/of-string
                                        ad/study-defn)
        ins-forms (get (form-gen-fn studied) arity)]
    (case arity
      :arity-1
      (prepend-to-defn-body-1 studied ins-forms) ; actually single form
      :arity-n
      (prepend-to-defn-bodies-1 defn-str ins-forms))))

(defn prepend-to-defn-let-bodies
  [defn-str form-gen-fn]
  (let [d-studied (-> defn-str
                      rz/of-string
                      ad/study-defn)
        fn-name (rz/string (:fn-name d-studied))
        prepend-to-body
        (fn [zloc]
          (let [studied (al/study-let zloc)
                ins-form
                (form-gen-fn (assoc studied
                                    :fn-name fn-name))]
            (-> (:body studied)
                (rz/insert-left ins-form)
                (rz/insert-left (rn/newlines 1)))))]
    (-> (rz/of-string defn-str)
        (rz/prewalk #(and (rz/list? %)
                          (> (rz/length %) 1)
                          (= (first (rz/sexpr %))
                             'let))
                    prepend-to-body)
        rz/root-string)))

(defn replace-defn-let-bindings
  [defn-str form-gen-fn]
  (let [d-studied (-> defn-str
                      rz/of-string
                      ad/study-defn)
        fn-name (rz/string (:fn-name d-studied))
        replace-bindings
        (fn [zloc]
          (let [studied (al/study-let zloc)
                new-bindings-form
                (form-gen-fn (assoc studied
                                    :fn-name fn-name))]
            (-> (:bindings studied)
                (rz/replace new-bindings-form))))]
    (-> (rz/of-string defn-str)
        (rz/prewalk #(and (rz/list? %)
                          (> (rz/length %) 1)
                          (= (first (rz/sexpr %))
                             'let))
                    replace-bindings)
        rz/root-string)))

(comment

  (require
   '[augistints.defn :as ad]
   '[augistints.edit :as ae]
   '[augistints.format :as af]
   '[augistints.gen :as ag]
   '[augistints.samples :as as]
   '[rewrite-clj.zip :as rz]
   :reload-all)

  (-> as/ma-d-form-str
      (ae/prepend-to-defn-body ag/log-defn-args-gen)
      print)

  (-> as/ma-d-form-str
      (ae/prepend-to-defn-body ag/log-defn-entry-gen)
      print)

  (-> as/ma-d-form-str
      rz/of-string
      ad/study-defn
      ag/log-defn-entry-gen
      :arity-n
      (nth 1))

  (-> as/d-with-l-form-str
      (ae/prepend-to-defn-let-bodies ag/let-log-bindings-gen)
      '[x 1
        y 2
        z 3])
  
  (-> as/d-with-l-form-str
      (ae/prepend-to-defn-let-bodies ag/let-log-bindings-gen)
      print)

  (-> as/docstring-defn-str
      (ae/prepend-to-defn-body ag/log-defn-args-gen)
      print)

  ;; originally gave 'insert at top' error
  (-> as/just-map-returning-defn-str
      (ae/prepend-to-defn-body ag/log-defn-args-gen)
      print)

  ;; test replace
  (-> as/d-with-l-form-str
      rz/of-string
      rz/down
      (rz/replace 'defn-)
      rz/root-string)

  (-> as/d-with-l-form-str
      (ae/replace-defn-let-bindings ag/interleave-bindings-gen)
      af/cljfmt
      print)

  (-> as/ma-d-form-str
      (ae/prepend-to-defn-body ag/inline-def-gen)
      print)

 )
