(ns sieppari.async.manifold
  (:require [sieppari.async :as sa]
            [manifold.deferred :as d]))

(extend-protocol sa/AsyncContext
  manifold.deferred.Deferred
  (async? [_] true)
  (continue [d ctx f]
    (-> (d/chain'- nil d f)
        (d/catch' (fn [e] (f (assoc ctx :error e))))))
  (await [d] (deref d))

  manifold.deferred.ErrorDeferred
  (async? [_] true)
  (continue [d ctx f]
    (-> (d/chain'- nil d f)
        (d/catch' (fn [e] (f (assoc ctx :error e))))))
  (await [d] (deref d)))
