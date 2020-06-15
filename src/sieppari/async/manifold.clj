(ns sieppari.async.manifold
  (:require [sieppari.async :as sa]
            [manifold.deferred :as d]))

(extend-protocol sa/AsyncContext
  manifold.deferred.Deferred
  (async? [_] true)
  (continue [d f] (d/chain'- nil d f))
  (catch [d f] (d/catch' d f))
  (await [d] (deref d))

  manifold.deferred.ErrorDeferred
  (async? [_] true)
  (continue [d f] (d/chain'- nil d f))
  (catch [d f] (d/catch' d f))
  (await [d] (deref d))

  manifold.deferred.SuccessDeferred
  (async? [_] true)
  (continue [d f] (d/chain'- nil d f))
  (catch [d f] (d/catch' d f))
  (await [d] (deref d))

  manifold.deferred.LeakAwareDeferred
  (async? [_] true)
  (continue [d f] (d/chain'- nil d f))
  (catch [d f] (d/catch' d f))
  (await [d] (deref d))
  )
