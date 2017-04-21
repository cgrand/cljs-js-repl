(ns net.cgrand.cljs.js.repl.main
  (:require
    [net.cgrand.cljs.js.repl.async-reader :as async]
    [net.cgrand.cljs.js.repl.dynvars :as dyn])
  (:refer-clojure :exclude [with-bindings]))

(defmacro with-bindings
  "Executes body in the context of thread-local bindings for several vars
  that often need to be set!: *ns* *warn-on-reflection* *math-context*
  *print-meta* *print-length* *print-level* *compile-path*
  *command-line-args* *1 *2 *3 *e"
  [& body]
  `(dyn/binding [*ns* *ns*
                 #_#_*warn-on-reflection* *warn-on-reflection*
                 #_#_*math-context* *math-context*
                 *print-meta* *print-meta*
                 *print-length* *print-length*
                 *print-level* *print-level*
                 *print-namespace-maps* true
                 async/*data-readers* async/*data-readers*
                 async/*default-data-reader-fn* async/*default-data-reader-fn*
                 #_#_*compile-path* (System/getProperty "clojure.compile.path" "classes")
                 #_#_*command-line-args* *command-line-args*
                 #_#_*unchecked-math* *unchecked-math*
                 *assert* *assert*
                 #_#_clojure.spec/*explain-out* clojure.spec/*explain-out*
                 *1 nil
                 *2 nil
                 *3 nil
                 *e nil]
     ~@body))