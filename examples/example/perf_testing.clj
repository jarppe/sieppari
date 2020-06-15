(ns example.perf-testing
  (:require [criterium.core :as criterium]
            [sieppari.core :as s]
            [sieppari.queue :as sq]
            [sieppari.async.core-async]
            [sieppari.async.manifold]
            [io.pedestal.interceptor :as pi]
            [io.pedestal.interceptor.chain :as pc]
            [manifold.deferred :as d]
            [promesa.core :as p]
            [clojure.core.async :as a]))

(set! *warn-on-reflection* true)

(defn raw-title [color s]
  (println (str color (apply str (repeat (count s) "#")) "\u001B[0m"))
  (println (str color s "\u001B[0m"))
  (println (str color (apply str (repeat (count s) "#")) "\u001B[0m")))

(def title (partial raw-title "\u001B[35m"))
(def suite (partial raw-title "\u001B[32m"))

(defmacro bench! [name & body]
  `(do
     (title ~name)
     (assert (= ~@body {}))
     (let [{[lower#] :lower-q :as res#} (criterium/quick-benchmark (do ~@body) nil)]
       (println "\u001B[32m\n" (format "%.2fµs" (* 1000000 lower#)) "\u001B[0m")
       (println)
       (criterium/report-result res#))
     (println)))

(defn make-capture-result-interceptor [p]
  (pi/interceptor
    {:leave (fn [ctx]
              (deliver p (:response ctx))
              ctx)}))

(comment

  ; 2.7µs
  ; 0.5µs (chain')
  (bench!
    "manifold: identity"
    @(d/chain'
       {}
       identity
       identity
       identity
       identity
       identity
       identity
       identity
       identity
       identity
       identity))

  ; 73µs
  ; 82µs (chain')
  (bench!
    "manifold: future"
    @(d/chain'
       {}
       d/future
       d/future
       d/future
       d/future
       d/future
       d/future
       d/future
       d/future
       d/future
       d/future))

  ; 3.5µs
  ; 1.5µs (chain')
  (bench!
    "manifold: success-deferred"
    @(d/chain'
       {}
       d/success-deferred
       d/success-deferred
       d/success-deferred
       d/success-deferred
       d/success-deferred
       d/success-deferred
       d/success-deferred
       d/success-deferred
       d/success-deferred
       success-deferred)))

(def sync-interceptor {:enter identity})
(def async-interceptor {:enter #(a/go %)})
(def deferred-interceptor {:enter d/success-deferred})
(def promesa-interceptor {:enter p/promise})
(def future-interceptor {:enter #(future %)})
(def delay-interceptor {:enter #(delay %)})

(defn create-s-chain [n i]
  (sq/into-queue (concat (repeat n i) [identity])))

(defn create-s-mixed-chain [n i]
  (sq/into-queue (concat (repeat (dec n) sync-interceptor) [i identity])))

(defn run-simple-perf-test [n]
  (let [sync-interceptors (concat (repeat n sync-interceptor) [identity])
        async-interceptors (concat (repeat n async-interceptor) [identity])

        p-context {:request {}}
        p-sync-chain (mapv pi/interceptor sync-interceptors)
        p-async-chain (map pi/interceptor async-interceptors)

        s-sync-chain (create-s-chain n sync-interceptor)
        s-async-chain (create-s-chain n async-interceptor)

        s-deferred-chain (create-s-chain n deferred-interceptor)
        s-future-chain (create-s-chain n future-interceptor)
        s-delay-chain (create-s-chain n delay-interceptor)
        s-promesa-chain (create-s-chain n promesa-interceptor)]

    (suite (str "queue of " n))

    ;; 8.2µs
    ;; 8.2µs (context-api)
    (bench!
      "pedestal: sync"
      (->> p-sync-chain
           (pc/enqueue p-context)
           (pc/execute)
           :response))

    ;; 99µs
    ;; 92µs (context-api)
    (bench!
      "pedestal: core.async"
      (let [p (promise)]
        (->> (cons (make-capture-result-interceptor p) p-async-chain)
             (pc/enqueue p-context)
             (pc/execute))
        @p))

    ;; 1.3µs
    ;; 2.1µs
    ;; 1.2µs (context-api)
    (bench!
      "sieppari: sync (sync)"
      (s/execute s-sync-chain {}))

    ;; 1.3µs
    ;; 2,5µs
    ;; 1.2µs (context-api)
    (bench!
      "sieppari: sync (async)"
      (let [p (promise)]
        (s/execute s-sync-chain {} p identity)
        @p))

    ;; 61µs
    ;; 69µs
    ;; 66µs (context-api)
    (bench!
      "sieppari: core.async (sync)"
      (s/execute s-async-chain {}))

    ;; 60µs
    ;; 65µs
    ;; 70µs (context-api)
    (bench!
      "sieppari: core.async (async)"
      (let [p (promise)]
        (s/execute s-async-chain {} p identity)
        @p))

    ;; 140µs
    ;; 140µs (context-api)
    (bench!
      "sieppari: future (async)"
      (let [p (promise)]
        (s/execute s-future-chain {} p identity)
        @p))

    ;; 84µs
    ;; 100µs
    ;; 110µs (context-api)
    (bench!
      "sieppari: delay (async)"
      (let [p (promise)]
        (s/execute s-delay-chain {} p identity)
        @p))

    ;; 84µs
    ;; 62µs (chain'-)
    ;; 89µs
    ;; 90µs (context-api)
    (bench!
      "sieppari: deferred (sync)"
      (s/execute s-deferred-chain {}))

    ;; 84µs
    ;; 84µs (chain'-)
    ;; 150µs
    ;; 110µs (context-api)
    (bench!
      "sieppari: deferred (async)"
      (let [p (promise)]
        (s/execute s-deferred-chain {} p identity)
        @p))

    ;; 36µs
    ;; 3.8µs
    ;; 5.4µs
    ;; 3.8µs (context-api)
    (bench!
      "sieppari: promesa (sync)"
      (s/execute s-promesa-chain {}))

    ;; 38µs
    ;; 4.0µs
    ;; 5.3µs
    ;; 4.0µs (context-api)
    (bench!
      "sieppari: promesa (async)"
      (let [p (promise)]
        (s/execute s-promesa-chain {} p identity)
        @p))))

(defn one-async-in-sync-pipeline-test [n]

  (doseq [[name chain] [[(str "homogeneous queue of " n) create-s-chain]
                        [(str "queue of " (dec n) " sync + 1 async step") create-s-mixed-chain]]
          :let [_ (suite name)]
          [name interceptor] [["identity" sync-interceptor]
                              #_["deferred" deferred-interceptor]
                              #_["core.async" async-interceptor]
                              ["promesa" promesa-interceptor]]]

    (let [interceptors (chain n interceptor)]
      (bench!
        name
        (let [p (promise)]
          (s/execute interceptors {} p identity)
          @p)))

    ;; 1.8µs => 4.6µs => 1.8µs => 1.3µs
    ;; 1.7µs => 4.6µs => 1.8µs => 1.4µs
    "identity"

    ;; 93µs => 100µs
    ;; 20µs => 18µs
    "deferred"

    ;; 54µs => 73µs
    ;; 20µs => 17µs
    "core.async"

    ;; 40µs => 4.0µs => 4.4µs => 3.9µs => 3.3µs
    ;; 19µs => 2.5µs => 5.0µs => 2.0µs => 1.8µs
    "promesa"))

(defn middleware-comp [n]
  (let [chain (apply comp (repeat n identity))]

    ;; 105ns
    (bench!
      "comp"
      (chain {}))))

(defn pedestal-one-async [n]
  (let [ctx {:request {}}
        chain (conj (conj (vec (repeat (dec n) (pi/interceptor {:enter identity}))) (pi/interceptor {:enter #(a/go %)})) (pi/interceptor identity))]

    ;; 50µs
    (bench!
      "pedestal 9 sync + 1 async"
      (let [p (promise)]
        (->> (cons (make-capture-result-interceptor p) chain)
             (pc/enqueue ctx)
             (pc/execute))
        @p))))

(defn -main [& _]
  (run-simple-perf-test 10)
  (one-async-in-sync-pipeline-test 10)
  (middleware-comp 10)
  (pedestal-one-async 10))

(comment
  (run-simple-perf-test 10)
  (one-async-in-sync-pipeline-test 10)
  (middleware-comp 10)
  (pedestal-one-async 10)

  (do
    (require '[clj-async-profiler.core :as prof])
    (prof/serve-files 8080))

  (time
    (prof/profile
      (let [interceptors (create-s-chain 10 identity)]
        (dotimes [_ 3000000]
          (let [p (promise)]
            (s/execute interceptors {} p identity)
            @p))))))
