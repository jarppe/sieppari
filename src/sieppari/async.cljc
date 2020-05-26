(ns sieppari.async
  #?(:clj (:refer-clojure :exclude [await]))
  (:require [sieppari.util :refer [exception?]])
  #?(:clj (:import java.util.concurrent.CompletionStage
                   java.util.concurrent.CompletionException
                   java.util.function.Function
                   java.util.function.BiFunction)))

(defprotocol AsyncContext
  (async? [t])
  (continue [t ctx f])
  #?(:clj (await [t])))

#?(:clj
   (extend-protocol AsyncContext
     Object
     (async? [_] false)
     (continue [t ctx f] (f t))
     (await [t] t)))

#?(:cljs
   (extend-protocol AsyncContext
     default
     (async? [_] false)
     (continue [t ctx f] (f t))))

#?(:clj
   (extend-protocol AsyncContext
     clojure.lang.IDeref
     (async? [_] true)
     (continue [c ctx f]
       (future (let [c @c]
                 (if (exception? c)
                   (f (assoc ctx :error c))
                   (f c)))))
     (await [c] @c)))

#?(:clj
   (deftype BiFunctionContinue [f ctx]
     BiFunction
     (apply [_ v e]
       (if (some? e)
         (if (instance? CompletionException e)
           (f (assoc ctx :error (.getCause ^Exception e)))
           (f (assoc ctx :error e)))
         (f v)))))

#?(:clj
   (extend-protocol AsyncContext
     CompletionStage
     (async? [_] true)
     (continue [this ctx f]
       (.handle ^CompletionStage this (->BiFunctionContinue f ctx)))
     (await [this]
       (deref this))))

#?(:cljs
   (extend-protocol AsyncContext
     js/Promise
     (async? [_] true)
     (continue [t ctx f]
       (-> t
           (.then f)
           (.catch  (fn [e]
                      (f (assoc ctx :error e))))))))
