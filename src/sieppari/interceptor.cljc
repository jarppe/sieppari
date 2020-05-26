(ns sieppari.interceptor
  (:require [sieppari.async :as a]
            [sieppari.util :refer [exception?]]
            [clojure.core :as c]))

(defrecord Interceptor [name enter leave error])

(defprotocol IntoInterceptor
  (into-interceptor [t] "Given a value, produce an Interceptor Record."))

(defn- set-result [ctx response]
  (if (and (some? response) (a/async? response))
    (a/continue response ctx (fn [resp] (if (and (map? resp) (:error resp))
                                         resp
                                         (assoc ctx :response resp))))
    (assoc ctx
      (if (exception? response) :error :response)
      response)))

(extend-protocol IntoInterceptor
  ;; Map -> Interceptor:
  #?(:clj  clojure.lang.IPersistentMap
     :cljs cljs.core.PersistentHashMap)
  (into-interceptor [interceptor-map]
    (map->Interceptor interceptor-map))

  #?(:cljs cljs.core.PersistentArrayMap)
  (into-interceptor [interceptor-map]
    (map->Interceptor interceptor-map))

  ;; Function -> Handler interceptor:
  #?(:clj  clojure.lang.Fn
     :cljs function)
  (into-interceptor [handler]
    (into-interceptor {:enter (fn [ctx]
                                (set-result ctx (handler (:request ctx))))}))

  ;; Vector -> Interceptor, first element is a function to create
  ;; the interceptor, rest are arguments for it:
  #?(:clj  clojure.lang.IPersistentVector
     :cljs cljs.core.PersistentVector)
  (into-interceptor [t]
    (into-interceptor (apply (first t) (rest t))))

  ;; Interceptor -> Interceptor, nop:
  Interceptor
  (into-interceptor [t]
    t)

  ;; nil -> nil, nop:
  nil
  (into-interceptor [_]
    nil))
