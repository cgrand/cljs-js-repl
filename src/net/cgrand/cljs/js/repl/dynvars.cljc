(ns net.cgrand.cljs.js.repl.dynvars
  (:require [cljs.analyzer :as ana])
  (:refer-clojure :exclude [binding]))

(defmacro dynvar [name idx]
  (let [name (ana/resolve-symbol name)]
    `(net.cgrand.cljs.js.repl.dynvars/DynVar. '~name ~idx
       (fn 
         ([] ~name)
         ([v#] (set! ~name v#))))))

(defmacro binding
  "Drop-in replacement for cljs.core/binding except that bindings set using this macro can be conveyed.
   See bound-fn, get-bindings, with-bindings."
  [bindings & body]
  (let [names (set (map ana/resolve-symbol (take-nth 2 bindings)))
        bdv (gensym 'bdv)
        vn (gensym 'vn)]
    `(cljs.core/binding 
       [*bound-dynvars* 
        (let [~bdv *bound-dynvars* 
              ~vn (volatile! (dec (count ~bdv)))]
          (cond-> ~bdv
            ~@(mapcat
                (fn [name] `[(not (contains? ~bdv '~name)) (assoc '~name (dynvar ~name (vswap! ~vn inc)))])
                names)))
        ~@bindings]
       ~@body)))

(defmacro with-bindings [snapshot & body]
  `(let [pop-bindings!# (push-bindings! ~snapshot)]
     (try
       ~@body
       (finally
         (pop-bindings!#)))))

(defmacro bound-fn [& fntail]
  `(let [bindings# (get-bindings)
         f# (fn ~@fntail)]
     (fn [& args#]
       (with-bindings bindings# (apply f# args#)))))
