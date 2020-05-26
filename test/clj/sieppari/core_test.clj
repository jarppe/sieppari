(ns sieppari.core-test
  (:require [clojure.test :refer :all]
            [testit.core :refer :all]
            [sieppari.core :as s]
            [sieppari.async.core-async]
            [clojure.core.async :as a]))

(def try-f #'s/-try)

(deftest try-f-test
  (fact
    (try-f {} nil)
    => {})
  (fact
    (try-f {} (fn [ctx] (assoc ctx :foo "bar")))
    => {:foo "bar"})
  (fact
    (try-f {} (fn [_] (throw (ex-info "oh no" {}))))
    =in=> {:error (ex-info? "oh no" {})})
  (fact
    @(try-f {} (fn [_] (future (ex-info "oh no" {}))))
    =eventually-in=> (ex-info? "oh no" {})))

(def await-result #'s/await-result)

(def error (RuntimeException. "kosh"))

(deftest wait-result-core-async-test
  (facts "response"
    (await-result {:response :ctx} :response) => :ctx
    (await-result (a/go {:response :ctx}) :response) => :ctx
    (await-result (a/go (a/go {:response :ctx})) :response) => :ctx)
  (facts "error"
    (await-result {:error error} :response) =throws=> error
    (await-result (a/go {:error error}) :response) =throws=> error
    (await-result (a/go (a/go {:error error})) :response) =throws=> error))

(deftest wait-result-deref-test
  (facts "response"
    (await-result {:response :ctx} :response) => :ctx
    (await-result (future {:response :ctx}) :response) => :ctx
    (await-result (future (future {:response :ctx})) :response) => :ctx)
  (facts "exception"
    (await-result {:error error} :response) =throws=> error
    (await-result (future {:error error}) :response) =throws=> error
    (await-result (future (future {:error error})) :response) =throws=> error))

(def deliver-result #'s/deliver-result)

(defn fail! [_] (throw (ex-info "should never get here" {})))

(let [p (promise)]
  (deliver-result {:response :r} :response p fail!)
  (fact
    @p =eventually=> :r))

(deftest deliver-result-test
  (let [p (promise)]
    (deliver-result {:response :r} :response p fail!)
    (fact
      @p =eventually=> :r))(let [p (promise)]
                                 (deliver-result {:response :r} :response p fail!)
                                 (fact @p =eventually=> :r))

  (let [p (promise)]
    (deliver-result {:response :r} :response p fail!)
    (fact @p =eventually=> :r))

  (let [p (promise)]
    (deliver-result {:error (ex-info "oh no" {})} :response fail! p)
    (fact @p =eventually=> (ex-info? "oh no" {})))

  (let [p (promise)]
    (deliver-result (a/go {:response :r}) :response p fail!)
    (fact @p =eventually=> :r))

  (let [p (promise)]
    (deliver-result (a/go {:error (ex-info "oh no" {})}) :response fail! p)
    (fact @p =eventually=> (ex-info? "oh no" {})))

  (let [p (promise)]
    (deliver-result (future (a/go {:response :r})) :response p fail!)
    (fact @p =eventually=> :r)))
