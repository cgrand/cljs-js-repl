(ns net.cgrand.cljs.js.repl.async-reader
  (:require [net.cgrand.cljs.js.repl.dynvars :as dyn]))

(defmacro bound-read [bindings [f rdr & args]]
  `(let [rdr# ~rdr] (or (dyn/binding ~bindings (doto (~f rdr# ~@args) (prn '~bindings))) (on-ready rdr# (fn [rdr#] (dyn/pop-frame!) (check rdr#))))))
