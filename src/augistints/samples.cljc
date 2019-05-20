(ns augistints.samples
  (:require
   [augistints.defn :as ad]
   [rewrite-clj.zip :as rz]))

(alias 'as 'augistints.samples)

;; single arity
(def d-form-str
  (str "(defn my-meta-pre-post-fn\n"
       "  {:fun-meta \"smile!\"}\n"
       "  [f x]\n"
       "  {:pre [(pos? x)]\n"
       "   :post [(= % (* 2 x))]}\n"
       "  (f x)\n"
       "  true)\n"))

;; multi-arity
(def ma-d-form-str
  (str "(defn my-meta-pre-post-fn\n"
       "  {:fun-meta \"smile!\"}\n"
       "  ([f x]\n"
       "   {:pre [(pos? x)]\n"
       "    :post [(= % (* 2 x))]}\n"
       "   (f x)\n"
       "   true)\n"
       "  ([]\n"
       "   (+ 1 1)))\n"))

(def l-form-str
  (str "(let [x 1\n"
       "      y 2\n"
       "      z (+ 1 2)]\n"
       "  (println (str \"hi \" x)))\n"))

(def d-with-l-form-str
  (str "(defn fn-with-let\n"
       "  [a b]\n"
       "  (println \"hi\")\n"
       "  (let [x 1\n"
       "        {:keys [y] :as all} {:y 3}\n"
       "        z (+ 1 2)]\n"
       "    (+ z y (- 1 x))\n"
       "    (when true\n"
       "      (let [w 0\n"
       "            p 9]\n"
       "        (/ 2 3)))))\n"))

(def reader-cond-forms-str
  (str "#?(:clj\n"
       "   (defn clj-fn\n"
       "     [x]\n"
       "     (+ 1 x))\n"
       "   :cljr\n"
       "   (defn cljr-fn\n"
       "     ([x]\n"
       "      (cljr-fn x 0))\n"
       "     ([[x y :as point]]\n"
       "      (+ 2 x y)))\n"
       "   :cljs\n"
       "   (defn cljs-fn\n"
       "     [{:keys [x y z] :as all}]\n"
       "     (+ 3 x y z)))\n"))

(def repl-tooling-connect-socket2!-str
  (str "(defn connect-socket2! [host port]\n"
       "  (let [in (async/chan)\n"
       "        fragment (async/chan)\n"
       "        promises (atom (iterate #(async/promise-chan) (async/promise-chan)))\n"
       "        out (async/chan)\n"
       "        _ (go-loop [[chan & others] @promises]\n"
       "            (if-let [val (async/<! chan)]\n"
       "              (do\n"
       "                (async/>! out val)\n"
       "                (recur others))\n"
       "              (async/close! out)))\n"
       "        buffer (atom {:paused false :contents \"\"})\n"
       "        socket (doto (. net createConnection port host)\n"
       "                 (.on \"data\" #(treat-result buffer promises fragment %))\n"
       "                 (.on \"close\" #(async/close! (first @promises))))]\n"
       "    (go-loop []\n"
       "      (let [string (str (<! in))]\n"
       "        (.write socket string))\n"
       "        (recur))\n"
       "    [in out socket]))\n"))

(def wrap-dynamic-nav-str
  (str "(defn wrap-dynamic-nav [f]\n"
       "  (fn [& args]\n"
       "    (let [ret (apply f args)]\n"
       "      (cond (and (sequential? ret) (static-path? ret))\n"
       "            (i/comp-paths* ret)\n"
       "            (and (sequential? ret) (= 1 (count ret)))\n"
       "            (first ret)\n"
       "            :else\n"
       "            ret))))\n"))

(def destroy-str
    "(defn destroy
      \"Removes a GameObject, component or asset. When called with `t`, the removal
  happens after `t` seconds. Wraps `Object/Destroy`.\"
      ([^UnityEngine.Object obj]
       (UnityEngine.Object/Destroy obj))
      ([^UnityEngine.Object obj ^double t]
       (UnityEngine.Object/Destroy obj t)))")

(def docstring-defn-str
  (str "(defn my-docstr-fn\n"
       "  \"My nice docstring\"\n"
       "  []\n"
       "  (+ 1 1))\n"))
  
(def just-map-returning-defn-str
  (str "(defn empty-env\n"
       "  []\n"
       "  {:a 1\n"
       "   :b 2})\n"))

(def ampersand-in-params-defn-str
  (str "(defn analyze-const\n"
       "  [form env & [type]]\n"
       "  (+ 1 1 2))\n"))

(def d-zloc
  (rz/of-string as/d-form-str))

(def studied
  (ad/study-defn d-zloc))

(def ma-d-zloc
  (rz/of-string as/ma-d-form-str))

(def ma-studied
  (ad/study-defn ma-d-zloc))
