(ns sieppari.core-async-test
  (:require [clojure.test :refer :all]
            [testit.core :refer :all]
            [sieppari.core :as sc]
            [sieppari.async.core-async]
            [clojure.core.async :refer [go <! <!!]]))

(defn make-logging-interceptor [log name]
  {:name name
   :enter (fn [ctx] (swap! log conj [:enter name]) ctx)
   :leave (fn [ctx] (swap! log conj [:leave name]) ctx)
   :error (fn [ctx] (swap! log conj [:error name]) ctx)})

(defn make-async-logging-interceptor [log name]
  {:name name
   :enter (fn [ctx] (swap! log conj [:enter name]) (go ctx))
   :leave (fn [ctx] (swap! log conj [:leave name]) (go ctx))
   :error (fn [ctx] (swap! log conj [:error name]) (go ctx))})

(defn make-logging-handler [log]
  (fn [request]
    (swap! log conj [:handler])
    request))

(defn make-async-logging-handler [log]
  (fn [request]
    (swap! log conj [:handler])
    (go request)))

(def request {:foo "bar"})
(def error (ex-info "oh no" {}))

(defn fail! [& _]
  (throw (ex-info "Should never be called" {})))

(deftest setup-sync-test-test
  (let [log (atom [])
        response (-> [(make-logging-interceptor log :a)
                      (make-logging-interceptor log :b)
                      (make-logging-interceptor log :c)
                      (make-logging-handler log)]
                     (sc/execute request))]
    (fact
      @log => [[:enter :a]
               [:enter :b]
               [:enter :c]
               [:handler]
               [:leave :c]
               [:leave :b]
               [:leave :a]])
    (fact
      response => request)))

(deftest setup-async-test-test
  (let [log (atom [])
        response (-> [(make-async-logging-interceptor log :a)
                      (make-async-logging-interceptor log :b)
                      (make-async-logging-interceptor log :c)
                      (make-async-logging-handler log)]
                     (sc/execute request))]
    (fact
      @log => [[:enter :a]
               [:enter :b]
               [:enter :c]
               [:handler]
               [:leave :c]
               [:leave :b]
               [:leave :a]])
    (fact
      response => request)))

(deftest execute-context-setup-async-test-test
  (let [log (atom [])
        response (-> [(make-async-logging-interceptor log :a)
                      (make-async-logging-interceptor log :b)
                      (make-async-logging-interceptor log :c)
                      (make-async-logging-handler log)]
                     (sc/execute-context {:request request}))]
    (fact
      @log => [[:enter :a]
               [:enter :b]
               [:enter :c]
               [:handler]
               [:leave :c]
               [:leave :b]
               [:leave :a]])
    (fact
      (:response response) => request)))

(deftest async-b-sync-execute-test
  (let [log (atom [])
        response (-> [(make-logging-interceptor log :a)
                      (make-async-logging-interceptor log :b)
                      (make-logging-interceptor log :c)
                      (make-logging-handler log)]
                     (sc/execute request))]
    (fact
      @log => [[:enter :a]
               [:enter :b]
               [:enter :c]
               [:handler]
               [:leave :c]
               [:leave :b]
               [:leave :a]])
    (fact
      response => request)))

(deftest async-handler-test
  (let [log (atom [])
        response (-> [(make-logging-interceptor log :a)
                      (make-logging-interceptor log :b)
                      (make-logging-interceptor log :c)
                      (make-async-logging-handler log)]
                     (sc/execute request))]
    (fact
      @log => [[:enter :a]
               [:enter :b]
               [:enter :c]
               [:handler]
               [:leave :c]
               [:leave :b]
               [:leave :a]])
    (fact
      response => request)))

(deftest async-stack-sync-execute-test
  (let [log (atom [])
        response (-> [(make-logging-interceptor log :a)
                      (make-logging-interceptor log :b)
                      (make-logging-interceptor log :c)
                      (make-async-logging-handler log)]
                     (sc/execute request))]
    (fact
      response => request)
    (fact
      @log =eventually=> [[:enter :a]
                          [:enter :b]
                          [:enter :c]
                          [:handler]
                          [:leave :c]
                          [:leave :b]
                          [:leave :a]])))

(deftest async-stack-async-execute-test
  (let [log (atom [])
        response-p (promise)]
    (-> [(make-logging-interceptor log :a)
         (make-logging-interceptor log :b)
         (make-logging-interceptor log :c)
         (make-async-logging-handler log)]
        (sc/execute request (partial deliver response-p) fail!))
    (fact
      @response-p =eventually=> request)
    (fact
      @log =eventually=> [[:enter :a]
                          [:enter :b]
                          [:enter :c]
                          [:handler]
                          [:leave :c]
                          [:leave :b]
                          [:leave :a]])))

(deftest async-execute-with-error-test
  (let [log (atom [])
        response-p (promise)]
    (-> [(make-logging-interceptor log :a)
         (make-logging-interceptor log :b)
         (make-logging-interceptor log :c)
         (fn [_]
           (swap! log conj [:handler])
           (throw error))]
        (sc/execute request fail! (partial deliver response-p)))
    (fact
      @response-p =eventually=> error)
    (fact
      @log =eventually=> [[:enter :a]
                          [:enter :b]
                          [:enter :c]
                          [:handler]
                          [:error :c]
                          [:error :b]
                          [:error :a]])))

(deftest async-execute-rejection-test
  (let [log (atom [])
        response-p (promise)]
    (-> [(make-logging-interceptor log :a)
         (make-logging-interceptor log :b)
         (make-logging-interceptor log :c)
         (fn [_]
           (swap! log conj [:handler])
           (go error))]
        (sc/execute request fail! (partial deliver response-p)))
    (fact
      @response-p =eventually=> error)
    (fact
      @log =eventually=> [[:enter :a]
                          [:enter :b]
                          [:enter :c]
                          [:handler]
                          [:error :c]
                          [:error :b]
                          [:error :a]])))

(deftest async-failing-handler-test
  (let [log (atom [])]
    (fact
      (-> [(make-logging-interceptor log :a)
           (make-logging-interceptor log :b)
           (make-logging-interceptor log :c)
           (fn [_]
             (swap! log conj [:handler])
             (go error))]
          (sc/execute request))
      =throws=> error)
    (fact
      @log => [[:enter :a]
               [:enter :b]
               [:enter :c]
               [:handler]
               [:error :c]
               [:error :b]
               [:error :a]])))

(deftest async-failing-handler-b-fixes-test
  (let [log (atom [])]
    (fact
      (-> [(make-logging-interceptor log :a)
           (assoc (make-async-logging-interceptor log :b)
             :error (fn [ctx]
                      (swap! log conj [:error :b])
                      (go
                        (-> ctx
                            (assoc :error nil)
                            (assoc :response :fixed-by-b)))))
           (make-logging-interceptor log :c)
           (fn [_]
             (swap! log conj [:handler])
             (go error))]
          (sc/execute request))
      => :fixed-by-b)
    (fact
      @log => [[:enter :a]
               [:enter :b]
               [:enter :c]
               [:handler]
               [:error :c]
               [:error :b]
               [:leave :a]])))
