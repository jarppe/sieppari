(ns sieppari.async.core-async
  (:require [sieppari.async :as sa]
            [sieppari.util :refer [exception?]]
            [clojure.core.async :as cca
             #?@(:clj  [:refer [go <! <!!]]
                 :cljs [:refer-macros [go]])]))


(extend-protocol sa/AsyncContext
  #?(:clj  clojure.core.async.impl.protocols.Channel
     :cljs cljs.core.async.impl.channels/ManyToManyChannel)
  (async? [_] true)
  (continue [c ctx f] (go (let [c (cca/<! c)]
                                (if (exception? c)
                                  (f (assoc ctx :error c))
                                  (f c)))))
  #?(:clj (await [c] (<!! c))))
