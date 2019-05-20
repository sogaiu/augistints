(ns augistints.names)

(alias 'an 'augistints.names)

;; XXX: perhaps names-* should be returning sets not lists

(defn names-from-destr-form
  "Given a destructuring form, `d-form`, return a lazy seqence of names.

   Examples:

     (names-from-destr-form '[fst & rst :as all])

       => (fst rst all)

     (names-from-destr-form '{:keys [name address email] :as all})

       => (all name address email)

   Relies on `clojure.core/destructure`."
  [d-form]
  (->> (vector d-form :whatever)
       destructure
       (keep-indexed (fn [idx item]
                       (when (even? idx)
                         item)))
       (filter (fn [symbol]
                 (not (re-find #"__\d+$"
                               (name symbol)))))))

(defn names-from-params
  [params]
  (->> params
       (filter #(not= '& %))
       (map names-from-destr-form)
       (apply concat)))

(defn names-from-bindings
  [bindings]
  (->> bindings
       (map-indexed (fn [idx item]
                      [idx item]))
       (keep (fn [[idx item]]
               (when (even? idx)
                 item)))
       names-from-params))

(defn names-map-from-names
  [names]
  (zipmap (map #(keyword %)
               names)
          names))

(comment

  (require
   '[augistints.names :as an]
   :reload-all)

  ^{:ael/want '(all name address email)}
  (an/names-from-destr-form
   '{:keys [name address email] :as all})

  ^{:ael/want '(fst rst all)}
  (an/names-from-destr-form
   '[fst & rst :as all])

  ^{:ael/want '(x y z)}
  (an/names-from-params
   '[x y z])

  ^{:ael/want '(all name address email)}
  (an/names-from-params
   '[{:keys [name address email] :as all}])

  ^{:ael/want '(fst rst all)}
  (an/names-from-params
   '[[fst & rst :as all]])

  ^{:ael/want '(fst rst)}
  (an/names-from-params
   '[fst & rst])

  ^{:ael/want '(x y)}
  (an/names-from-bindings
   '[x 1
     y 2])

  ^{:ael/want '(x y all a b)}
  (an/names-from-bindings
   '[x 1
     y 2
     {:keys [a b] :as all} {:a 2 :b 3 :c 2}])

  ^{:ael/want '{:a a, :b b, :c c}}
  (an/names-map-from-names
   '(a b c))

  ;; (defn names-from-args-vec
  ;;   [args-vec]
  ;;   (->> args-vec
  ;;        (map an/names-from-destr-form)
  ;;        (apply concat)))

  ;; (names-from-args-vec '[{:keys [x y z] :as all}])
  ;; ;; => (all x y z)

  )
