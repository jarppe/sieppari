(ns sieppari.async.core-async-test
  (:require [clojure.test :refer [deftest is #?(:cljs async)]]
            [sieppari.async :as as]
            [sieppari.async.core-async]
            [clojure.core.async :as a]))

(deftest async?-test
  (is (as/async? (a/go "foo"))))

#?(:clj
   (deftest core-async-continue-clj-promise-test
     (let [respond (promise)]
       (as/continue (a/go "foo") {} (partial deliver respond))
       (is (= "foo" @respond))))
   :cljs
   (deftest core-async-continue-cljs-callback-test
     (async done
       (is (as/continue (a/go "foo")
                        {}
                        (fn [response]
                          (is (= "foo" response))
                          (done)))))))

#?(:clj
   (deftest core-async-catch-clj-promise-test
     (let [respond (promise)]
       (as/continue (a/go (Exception. "fubar")) nil (fn [_] (deliver respond "foo")))
       (is (= "foo" @respond))))
   :cljs
   (deftest core-async-catch-cljs-callback-test
     (async done
       (let [err (js/Error. "fubar")]
         (is (as/continue (a/go err)
                          {}
                          (fn [response]
                            (is (= {:error err} response))
                            (done))))))))

#?(:clj
   (deftest await-test
     (is (= "foo" (as/await (a/go "foo"))))))
