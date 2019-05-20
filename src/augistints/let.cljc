(ns augistints.let
  (:require
   [rewrite-clj.zip :as rz]))

(alias 'al 'augistints.let)

(defn study-let
  [l-zloc]
  (let [{:keys [zloc] :as m}
        {:root-zloc l-zloc
         :zloc (rz/down l-zloc)}
        {:keys [zloc] :as m}
        (merge m
               (if (some #{'let}
                         [(rz/sexpr zloc)])
                 {:zloc (rz/right zloc)}
                 (throw (#?(:cljs js/Error. :default Exception.)
                         (str "Expected let, but got: "
                              (rz/sexpr zloc)))))
               {:form-type zloc})
        {:keys [zloc] :as m}
        (merge m
               (if (vector? (rz/sexpr zloc))
                 {:zloc (rz/right zloc)}
                 (throw (#?(:cljs js/Error. :default Exception.)
                         (str "Expected vector, but got: "
                              (rz/sexpr zloc)))))
               {:bindings zloc})]
    (assoc m
           :body zloc)))

(comment

  (require
   '[augistints.let :as al]
   '[augistints.samples :as as]
   '[rewrite-clj.zip :as rz]
   :reload-all)

  ^{:ael/want '[x 1 y 2 z (+ 1 2)]}
  (-> as/l-form-str
      rz/of-string
      al/study-let
      :bindings
      rz/sexpr)

  ^{:ael/want '(println (str "hi " x))}  
  (-> as/l-form-str
      rz/of-string
      al/study-let
      :body
      rz/sexpr)

  )
