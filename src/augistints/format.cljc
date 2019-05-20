(ns augistints.format
  (:require
   [cljfmt.core :as cc]
   [zprint.core :as zc]))

(alias 'af 'augistints.format)

;; adapted from cljfmt.main
(def cljfmt-default-options
  {:alias-map {}
   :ansi? true
   :file-pattern #"\.clj[csx]?$"
   :indentation? true
   :indents cc/default-indents
   :insert-missing-whitespace? true
   :project-root "."
   :remove-consecutive-blank-lines? true
   :remove-surrounding-whitespace? true
   :remove-trailing-whitespace? true})

(defn cljfmt
  ([target-str]
   (cljfmt target-str cljfmt-default-options))
  ([target-str options]
   ;; determined by examining cljfmt.main
   ((cc/wrap-normalize-newlines #(cc/reformat-string % options))
    target-str)))

(def zprint-default-options
  {:map {:comma? false
         :force-nl? true}
   :parse-string-all? true})

;; XXX: likely always want :parse-string-all? true
(defn zprint
  ([target-str]
   (zprint target-str zprint-default-options))
  ([target-str options]
   (zc/zprint-str target-str
                  (merge zprint-default-options options))))

(comment

  (require
   '[augistints.edit :as ae]
   '[augistints.format :as af]
   '[augistints.gen :as ag]
   '[augistints.samples :as as]
   '[cljfmt.core :as cc]
   '[zprint.core :as zc]
   :reload-all)

  ;; XXX: use with-out-str for testing?
  
  (let [options {}
        modified-defn-str (-> as/d-form-str
                              (ae/prepend-to-defn-body ag/log-defn-args-gen))]
    (->> modified-defn-str
         ((cc/wrap-normalize-newlines #(cc/reformat-string % options)))
         print))

  (-> as/ma-d-form-str
      af/cljfmt
      print)

  (-> as/ma-d-form-str
      print)

  (-> as/ma-d-form-str
      (ae/prepend-to-defn-body ag/log-defn-args-gen)
      print)

  (-> as/ma-d-form-str
      (ae/prepend-to-defn-body ag/log-defn-args-gen)
      af/cljfmt
      print)

  (-> as/ma-d-form-str
      (ae/prepend-to-defn-body ag/log-defn-args-gen)
      (af/zprint {:map {:force-nl? true}})
      print)

  )
