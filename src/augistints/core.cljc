(ns augistints.core
  (:require
   [augistints.base64 :as ab]
   [augistints.edit :as ae]
   [augistints.format :as af]
   [augistints.gen :as ag]))

(def prepend-to-defn-body
  ae/prepend-to-defn-body)

(def log-defn-entry-gen
  ag/log-defn-entry-gen)

(def log-defn-args-gen
  ag/log-defn-args-gen)

(def inline-def-gen
  ag/inline-def-gen)

(def make-inline-def-with-meta-gen
  ag/make-inline-def-with-meta-gen)

(def prepend-to-defn-let-bodies
  ae/prepend-to-defn-let-bodies)

(def log-let-bindings-gen
  ag/log-let-bindings-gen)

(def replace-defn-let-bindings
  ae/replace-defn-let-bindings)

(def interleave-bindings-gen
  ag/interleave-bindings-gen)

(def cljfmt
  af/cljfmt)

(def zprint
  af/zprint)

(def encode
  ab/b64-encode)

(def decode
  ab/b64-decode)

(comment

  (require
   '[augistints.defn :as ad]
   '[augistints.let :as al]
   '[augistints.names :as an]
   '[augistints.samples :as as]
   '[rewrite-cljc.node :as rn]
   '[rewrite-cljc.zip :as rz]
   :reload-all)

  (do
    (require '[cognitect.rebl :as cr])
    (cr/ui))

  (require '[clojure.datafy :as cd])

  (defn send-to-rebl
    [code]
    (when-let [d-val (cd/datafy code)]
      (cr/submit 'code d-val)))

  ;; XXX: cljs has no slurp?
  (def source-str
    (slurp "src/augistints/core.cljc"))

  (def memory
    (atom []))

  (defn remember
    [zloc]
    (swap! memory conj zloc)
    zloc)

  (loop [a-zloc (rz/of-string source-str)]
    (if (rz/end? a-zloc)
      nil
      (recur (-> a-zloc
                 remember
                 rz/right))))

  (count @memory)

  (-> (nth @memory 14)
      rz/string)
  
  (map (fn [a-zloc]
         (rz/sexpr a-zloc))
       @memory)

  (def collected-defn-zlocs
    (keep (fn [a-zloc]
            (when (= 'defn (first (rz/sexpr a-zloc)))
              a-zloc))
          @memory))

  (count collected-defn-zlocs)
  
  (-> (nth collected-defn-zlocs 12)
      rz/sexpr)
  
  (reset! memory [])

  (def let-instances
    (atom []))

  (def visited
    (atom []))

  {:a 1 :b 2
   :c 3 :d 4 :e 5}
  
  (-> source-str
      rz/of-string
      (rz/prewalk (fn [zloc]
                    (swap! visited
                           conj zloc)
                    true)
                  (fn [zloc]
                    (let [sexpr (rz/sexpr zloc)]
                      (when (and (list? sexpr)
                                 (= (first sexpr) 'let))
                        (swap! let-instances
                               conj zloc))))))

  (count @let-instances)

  (count @visited)

  (defn report
    [zloc]
    (let [sexpr (rz/sexpr zloc)]
      (if (seq sexpr)
        (println (str (first sexpr) ": " sexpr "\n"))
        (println "not in a container")))
    zloc)
  
  ;; finding smallest sensible container (e.g. defn, defmacro, etc.)
  (-> as/ma-d-form-str
      rz/of-string
      rz/down
      rz/right
      rz/right
      rz/right
      rz/down
      rz/right
      rz/right ; now at (f x)
      report
      rz/up ; now at ([f x] ...
      report
      rz/up ; now at (defn my-meta-pre-post-fn ...
      report
      rz/sexpr)

  (defn mute
    [zloc]
    nil)
  
  (-> as/repl-tooling-connect-socket2!-str
      rz/of-string
      rz/down
      rz/right
      rz/right
      rz/right
      rz/down
      rz/right
      rz/right
      rz/down
      rz/right
      rz/right
      rz/down
      rz/right
      rz/right ; at (.wrte socket string)
      rz/up
      report
      rz/up
      report
      rz/up
      report
      rz/up
      report
      rz/up
      report
      rz/up
      report
      mute)

  (def res
    (let [s-loc (-> as/repl-tooling-connect-socket2!-str
                    rz/of-string
                    rz/down
                    rz/right
                    rz/right
                    rz/right
                    rz/down
                    rz/right
                    rz/right
                    rz/down
                    rz/right
                    rz/right
                    rz/down
                    rz/right
                    rz/right)] ; at (.wrte socket string)
      (loop [zloc s-loc
             up-trail []]
        (let [sexpr (rz/sexpr zloc)]
          (if (not (seq sexpr))
            up-trail
            (recur (rz/up zloc)
                   (conj up-trail zloc)))))))

  (print
   (nth
    (map #(rz/string %)
         res)
    3))

  ;; learn about find
  
  (-> as/repl-tooling-connect-socket2!-str
      rz/of-string
      (rz/find-depth-first (fn [zloc]
                             (and (rz/list? zloc)
                                  (= (first (rz/sexpr zloc))
                                     'let))))
      rz/string
      print)
              
  (let [s-loc (rz/of-string as/repl-tooling-connect-socket2!-str)
        let-finder (fn [zloc]
                     (and (rz/list? zloc)
                          (= (first (rz/sexpr zloc))
                             'let)))]
    (-> s-loc
        (rz/find-depth-first let-finder)
        (rz/find-next-depth-first let-finder)
        rz/string
        print))

  ;; operate on whole file
  
  (def src-str
    (slurp "src/augistints/defn.cljc"))

  ;; print out initial sexpr of each top-level form
  
  (loop [a-zloc (rz/of-string src-str)]
    (if (rz/end? a-zloc)
      :end
      (recur (-> a-zloc
                 ((fn [zloc]
                    (-> zloc
                        rz/down
                        rz/sexpr
                        println)
                     zloc))
                 rz/right))))

  ;; trying out zip subedit

  (def comment-form-with-two-tests-str
    (str "(comment\n"
         "\n"
         "  ;;< Subtracting 1 from 1 should yield 0\n"
         "  (- 1 1)\n"
         "  ;;> 0\n"
         "\n"         
         "  ;;< Adding 1 and 1 should yield 2\n"
         "  (+ 1 1)\n"
         "  ;;> 2\n"
         "\n"
         ")\n"))

  (let [a-zloc (rz/of-string comment-form-with-two-tests-str)
        s-zloc (-> a-zloc
                   rz/down
                   rz/right)
        e-zloc (rz/edit-> s-zloc
                          rz/down
                          rz/right
                          rz/remove)
        e2-zloc (rz/edit-> e-zloc
                           rz/down
                           rz/right
                           (rz/replace 8))]
    (rz/string
     (rz/edit-> e2-zloc
                rz/down
                rz/right
                (rz/insert-right (rn/coerce 9)))))

  ;; edit + replace
  
  (let [a-zloc (rz/of-string comment-form-with-two-tests-str)
        s-zloc (-> a-zloc
                   rz/down
                   rz/right)
        e-zloc (rz/edit-> s-zloc
                          rz/down
                          rz/right
                          (rz/replace '(let [a 1] a)))]
    (print (rz/string e-zloc)))

  ;; try out meta data node

  (def a-form-with-meta-str
    "^{:a-key 1} (+ 1 1)")

  (let [meta-zloc (rz/of-string a-form-with-meta-str)
        map-zloc (-> meta-zloc
                     rz/down)
        form-zloc (-> meta-zloc
                      rz/down
                      rz/right)]
    (println (str "map: " (rz/sexpr map-zloc)))
    (println (str "keys: " (keys (rz/sexpr map-zloc))))
    (println (str "form: " (rz/sexpr form-zloc))))

  ;; what's a good way to add meta data?
  (def a-form-without-meta-str
    "(+ 1 1)")

  (let [root-zloc (rz/of-string a-form-without-meta-str)
        ;; suppose there is already a zloc
        ;; is there no better way than this for adding metadata?
        with-meta-zloc (rz/of-string (str "^{:test 1} "
                                          (rz/string root-zloc)))]
    (println (str "without metadata: " (rz/string root-zloc)))
    (println (str "with metadata: " (rz/string with-meta-zloc))))

  ;; rich comment block example
  (def comment-form-with-meta-idea-str
    (str "(comment\n"
         "\n"
         "  ^{:ael/expected 0 :ael/name \"simple subtraction\"}\n"
         "  (- 1 1)\n"
         ")\n"))

  (let [root-zloc (rz/of-string comment-form-with-meta-idea-str)
        meta-zloc (-> root-zloc
                      rz/down
                      rz/right)
        map-zloc (-> meta-zloc
                     rz/down)
        form-zloc (-> meta-zloc
                      rz/down
                      rz/right)]
    (println (str "map: " (rz/sexpr map-zloc)))
    (println (str "keys: " (keys (rz/sexpr map-zloc))))
    (println (str "form: " (rz/sexpr form-zloc))))

  ;; removal based on detecting particular metadata

  ;; this doesn't work because apparently, the 2nd function argument to prewalk
  ;; must modify the passed in zloc for prewalk to work.
  (-> (rz/of-string comment-form-with-meta-idea-str)
      (rz/prewalk (fn [zloc]
                    (when (= (rz/tag zloc) :meta)
                      (let [map-zloc (rz/down zloc)]
                        (contains? (rz/sexpr map-zloc)
                                   :ael/expected))))
                  (fn [zloc]
                    (-> zloc
                        rz/down
                        rz/right)))
      rz/string)

  ;; this works because the 2nd function argument to prewalk
  ;; modifies (using rz/edit) the zloc passed in to it
  (-> (rz/of-string comment-form-with-meta-idea-str)
      (rz/prewalk (fn [zloc]
                    (when (= (rz/tag zloc) :meta)
                      (let [map-zloc (rz/down zloc)]
                        (contains? (rz/sexpr map-zloc)
                                   :ael/expected))))
                  (fn [zloc]
                    (rz/edit zloc
                             (fn [expr]
                               (with-meta expr {})))))
      rz/string)

  ;; lread two examples that help understand prewalk -- note that in
  ;; the examples, prewalk returns the same zloc it is passed.  this
  ;; is because for prewalk to return something different, it's second
  ;; function argument must modify the zloc that is passed in to
  ;; it (as mentioned above).
  (def ex2 "[1 2 3 [4 5]]")

  ;; skips 3 -- so return value of 2nd fn to prewalk affects traversal
  (-> (rz/of-string ex2)
      (rz/prewalk (fn [zloc]
                    (println (rz/string zloc))
                    (= (rz/string zloc) "2"))
                  (fn [zloc]
                    (println "-->" (rz/node (rz/right zloc)))
                    (rz/right zloc)))
      (rz/string))

  ;; skips 3 (slightly edited)
  (-> (rz/of-string ex2)
      (rz/prewalk (fn [zloc]
                    (println "walking: " (rz/string zloc))
                    (= (rz/string zloc) "2"))
                  (fn [zloc]
                    (rz/right zloc)))
      (rz/string))

  ;; lread's alternative
  (-> (rz/of-string comment-form-with-meta-idea-str)
      (rz/prewalk (fn [zloc]
                    (when (= (rz/tag zloc) :meta)
                      (let [map-zloc (rz/down zloc)]
                        (contains? (rz/sexpr map-zloc)
                                   :ael/expected))))
                  (fn [zloc]
                    (rz/replace zloc
                                (-> zloc
                                    rz/down
                                    rz/right
                                    rz/node))))
      rz/string)

  ;; borkdude's alternative -- slightly edited for comparability
  ;; this approach bypasses the confusion by avoiding prewalk altogether
  (-> (loop [zloc (rz/of-string comment-form-with-meta-idea-str)]
        (if (rz/end? zloc) (rz/root zloc)
            (let [t (rz/tag zloc)]
              (if (= :meta t)
                (recur (-> zloc
                           rz/splice
                           rz/remove))
                (recur (rz/next zloc))))))
      (rz/edn rz/down)
      rz/string)

  ;; reader macro -- specifically reader conditional

  (def reader-conditional-str
    (str "#?(:clj \"jvm clojure\"\n"
         "   :cljs \"javascript clojure\"\n"
         "   :cljr \"clr clojure\")\n"))
  
  (let [rc-zloc (rz/of-string reader-conditional-str)
        macro-symbol-zloc (-> rc-zloc
                              rz/down)
        cndl-form-zloc (-> rc-zloc
                           rz/down
                           rz/right)]
    (println (str "reader macro symbol: " (rz/sexpr macro-symbol-zloc)))
    (println (str "form: " (rz/sexpr cndl-form-zloc)))
    (println (str "number of elts: " (count (rz/sexpr cndl-form-zloc)))))
  
  )
