(ns augistints.let
  (:require
   [augistints.utils :as au]
   [rewrite-cljc.zip :as rz]))

(alias 'al 'augistints.let)

(defn study-let
  "Return map based on studying a zipper for a let-form."
  [l-zloc]
  (let [{:keys [zloc] :as m}
        {:root-zloc l-zloc
         :zloc (rz/down l-zloc)}
        {:keys [zloc] :as m}
        (merge m
               (if (au/let? zloc)
                 {:zloc (rz/right zloc)}
                 (throw (#?(:cljs js/Error. :default Exception.)
                         (str "Expected let, but got: "
                              (rz/string zloc)))))
               {:form-type zloc})
        {:keys [zloc] :as m}
        (merge m
               (if (rz/vector? zloc)
                 {:zloc (rz/right zloc)}
                 (throw (#?(:cljs js/Error. :default Exception.)
                         (str "Expected vector, but got: "
                              (rz/string zloc)))))
               {:bindings zloc})]
    (assoc m
           :body zloc)))

(comment

  (require
   '[augistints.let :as al]
   '[augistints.samples :as as]
   '[rewrite-cljc.zip :as rz]
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
