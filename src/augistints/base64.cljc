(ns augistints.base64
  (:require
   #?(:clj [base64-clj.core :as bc]
      :cljs [goog.crypt.base64 :as gcb])))

(alias 'ab 'augistints.base64)

(defn b64-encode
  [a-str]
  (#?(:clj bc/encode :cljs gcb/encodeString)
   a-str))

(defn b64-decode
  [b64-str]
  (#?(:clj bc/decode :cljs gcb/decodeString)
   b64-str))

(comment

  ;; base64 encoding and decoding

  #?(:clj
     (do
       (require '[base64-clj.core :as bc])

       ^{:ael/want "SGVsbG8sIFdvcmxkIQ=="
         :ael/name "Simple base 64 encoding test"}
       (bc/encode "Hello, World!")

       ^{:ael/want "Hello, World!"
         :ael/name "Simple base 64 decoding test"}
       (bc/decode "SGVsbG8sIFdvcmxkIQ==")
       
       ))
       
  #?(:cljs
     (do
       (require '[goog.crypt.base64 :as gcb])

       ^{:ael/want "aGk="
         :ael/name "Simple base 64 encoding test"}
       (gcb/encodeString "hi")

       ^{:ael/want "hi"
         :ael/name "Simple base 64 decoding test"}
       (gcb/decodeString "aGk=")

       ))

  ;; wrappers

  ^{:ael/want aG8gaG8gaG8=
    :ael/name "Base 64 encoding wrapper test"}
  (ab/b64-encode "ho ho ho")

  ^{:ael/want "ho ho ho"
    :ael/name "Base 64 decoding wrapper test"}
  (ab/b64-decode "aG8gaG8gaG8=")
  
  )
