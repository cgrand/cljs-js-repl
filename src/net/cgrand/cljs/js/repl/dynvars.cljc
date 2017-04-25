(ns net.cgrand.cljs.js.repl.dynvars
  (:require [cljs.analyzer :as ana])
  (:refer-clojure :exclude [binding]))

(defmacro dynvar [name]
  (let [name (ana/resolve-symbol name)]
    `(or (dynvars '~name) 
       (doto (net.cgrand.cljs.js.repl.dynvars/DynVar. '~name
               (fn 
                 ([] ~name)
                 ([v#] (set! ~name v#))))
         (->> (assoc dynvars '~name) (set! dynvars))))))

(defmacro binding
  "Drop-in replacement for cljs.core/binding except that bindings set using this macro can be conveyed.
   See bound-fn, get-bindings, with-bindings."
  [bindings & body]
  (let [names (set (map ana/resolve-symbol (take-nth 2 bindings)))]
    `(let [dynvars# [~@(map #(list `dynvar %) names)]]
       (push-frame! dynvars#)
       ~@(map #(list* `set! %) (partition 2 bindings))
       (try
         ~@body
         (finally
           (pop-frame!))))))

(defmacro with-bindings [snapshot & body]
  `(let [switch-back!# (switch-bindings! ~snapshot)]
     (try
       ~@body
       (finally
         (switch-back!#)))))

(defmacro bound-fn [& fntail]
  `(let [bindings# (get-binding-env)
         f# (fn ~@fntail)]
     (fn [& args#]
       (switch-bindings! bindings#)
       (apply f# args#))))
