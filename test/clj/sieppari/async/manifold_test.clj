(ns sieppari.async.manifold-test
  (:require [clojure.test :refer :all]
            [testit.core :refer :all]
            [sieppari.async :as as]
            [sieppari.async.manifold]
            [manifold.deferred :as d]))

(deftest async?-test
  (fact
    (as/async? (d/deferred)) => true))

(deftest continue-test
  (let [p (promise)
        d (d/deferred)]
    (future
      (d/success! d "foo"))
    (as/continue d nil (partial deliver p))
    (fact
      @p =eventually=> "foo")))

(deftest catch-test
  (let [p (promise)
        d (d/deferred)]
    (future
      (d/success! d "foo"))
    (as/continue (as/continue d nil (partial deliver p))
                 nil
              (fn [_] (deliver p "barf")))
    (fact
      @p =eventually=> "foo"))

  (let [p (promise)
        d (d/deferred)]
    (future
      (d/error! d (Exception. "fubar")))
    (as/continue d nil (fn [_] (deliver p "foo")))
    (fact
      @p =eventually=> "foo")))

(deftest await-test
  (fact
    (as/await (let [d (d/deferred)]
                (future
                  (d/success! d "foo"))
                d))
    => "foo"))
