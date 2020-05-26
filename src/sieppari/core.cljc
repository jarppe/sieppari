(ns sieppari.core
  #?(:cljs (:refer-clojure :exclude [iter]))
  (:require [sieppari.queue :as q]
            [sieppari.async :as a]
            [sieppari.context :as c]
            #?(:cljs [goog.iter :as iter]))
  #?(:clj (:import (java.util Iterator))))

(defrecord Context [error queue stack]
  c/Context
  (context? [_] true))

(defrecord RequestResponseContext [request response error queue stack]
  c/Context
  (context? [_] true))

(defn- -try [ctx f]
  (if f
    (try
      (f ctx)
      (catch #?(:clj Exception :cljs :default) e
        (assoc ctx :error e)))
    ctx))

(defn- -iter [v]
  #?(:clj  (clojure.lang.RT/iter v)
     :cljs (cljs.core/iter v)))

(defn- -invalid-context-type! [ctx stage]
  (throw
    (ex-info
      (str "Unsupported Context on " stage" - " ctx)
      {:ctx ctx})))

(defn- leave
  ([ctx] (leave nil ctx))
  ([old-ctx ctx]
   (cond
     (a/async? ctx) (a/continue ctx old-ctx leave)
     (c/context? ctx) (let [^Iterator it (:stack ctx)]
                        (if (.hasNext it)
                          (let [stage (if (:error ctx) :error :leave)
                                f     (-> it .next stage)]
                            (recur ctx (-try ctx f)))
                          ctx))
     :else (-invalid-context-type! ctx :leave))))

(defn- enter
  ([ctx] (enter nil ctx))
  ([old-ctx ctx]
   (cond
     (a/async? ctx) (a/continue ctx old-ctx enter)
     (c/context? ctx) (let [queue       (:queue ctx)
                            stack       (:stack ctx)
                            interceptor (peek queue)]
                        (if (or (not interceptor) (:error ctx))
                          (assoc ctx :stack (-iter stack))
                          (let [next-ctx (-> ctx
                                             (assoc :queue (pop queue))
                                             (assoc :stack #?(:clj  (conj stack interceptor)
                                                              :cljs (doto (or stack (array))
                                                                      (.unshift interceptor)))))]
                            (recur next-ctx
                                   (-try next-ctx (:enter interceptor))))))
     :else (-invalid-context-type! ctx :enter))))

#?(:clj
   (defn- await-result [ctx get-result]
     (if (a/async? ctx)
       (recur (a/await ctx) get-result)
       (if-let [error (:error ctx)]
         (throw error)
         (get-result ctx)))))

(defn- deliver-result [ctx get-result on-complete on-error]
  (if (a/async? ctx)
    (a/continue ctx nil #(deliver-result % get-result on-complete on-error))
    (let [error (:error ctx)
          result (or error (get-result ctx))
          callback (if error on-error on-complete)]
      (callback result))))

(defn- remove-context-keys [ctx]
  (dissoc ctx :error :queue :stack))

;;
;; Public API:
;;

(defn execute-context
  {:arglists
   '([interceptors ctx]
     [interceptors ctx on-complete on-error])}
  ([interceptors ctx on-complete on-error]
   (execute-context interceptors ctx on-complete on-error remove-context-keys))
  ([interceptors ctx on-complete on-error get-result]
   (if-let [queue (q/into-queue interceptors)]
     (try
       (-> (assoc ctx :queue queue)
           (map->Context)
           (enter)
           (leave)
           (deliver-result get-result on-complete on-error))
       (catch #?(:clj Exception :cljs js/Error) e (on-error e)))
     (on-complete nil))
   nil)
  #?(:clj
     ([interceptors ctx]
      (execute-context interceptors ctx remove-context-keys)))
  #?(:clj
     ([interceptors ctx get-result]
      (when-let [queue (q/into-queue interceptors)]
        (-> (assoc ctx :queue queue)
            (map->Context)
            (enter)
            (leave)
            (await-result get-result))))))

(defn execute
  {:arglists
   '([interceptors request]
     [interceptors request on-complete on-error])}
  ([interceptors request on-complete on-error]
   (if-let [queue (q/into-queue interceptors)]
     (try
       (-> (new RequestResponseContext request nil nil queue nil)
           (enter)
           (leave)
           (deliver-result :response on-complete on-error))
       (catch #?(:clj Exception :cljs js/Error) e (on-error e)))
     (on-complete nil))
   nil)
  #?(:clj
     ([interceptors request]
      (when-let [queue (q/into-queue interceptors)]
        (-> (new RequestResponseContext request nil nil queue nil)
            (enter)
            (leave)
            (await-result :response))))))
