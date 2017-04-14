(ns net.cgrand.cljs.js.repl.dynvars
  (:require-macros [net.cgrand.cljs.js.repl.dynvars :refer [bound-fn binding dynvar with-bindings]])
  (:refer-clojure :exclude [-lookup]))

(defprotocol IDynVar
  (-lookup [_ a])
  (-save [_ a])
  (-restore [_ a]))

(deftype DynVar [sym i f]
  Object
  (toString [_]
    (str "#'" sym))
  IDeref
  (-deref [_] (f))
  IDynVar
  (-lookup [_ a] (aget a i))
  (-save [_ a] (aset a i (f)))
  (-restore [_ a] (f (aget a i)))
  IEquiv
  (-equiv [this other]
    (if (instance? DynVar other)
      (= (.-sym this) (.-sym other))
      false))
  IHash
  (-hash [_]
    (hash-symbol sym)))

(def ^:dynamic *bound-dynvars* {`*bound-dynvars* (dynvar *bound-dynvars* 0)})

(defn- snapshot-bindings [bound-dynvars]
  (let [a (object-array bound-dynvars)]
    (doseq [^not-native dv (vals bound-dynvars)]
      (-save dv a))
    a))

(defn- restore-snapshot! [a bound-dynvars]
  (doseq [^not-native dv (vals bound-dynvars)]
    (-restore dv a)))

(defn get-bindings
  "Returns current dynamic bindings.
   The returned value can be passed to either bindings-map, with-bindings or push-bindings!."
  []
  (snapshot-bindings *bound-dynvars*))

(defn bindings-map
  "Returns a map representation (symbols to values) of the bindings."
  [bindings]
  (persistent!
    (reduce-kv (fn [m sym ^not-native dv] (assoc! m sym (-lookup dv bindings)))
      (transient {}) (aget bindings 0))))

(defn push-bindings!
  "WARNING: This is a low-level function. Prefer high-level macros like
   with-bindings where ever possible.

   \"Push\" the specified bindings and returns a 0-arg fn to call to \"pop\" them and restore original values."
  [bindings]
  (if bindings
    (let [dynvars-to-be-overwritten (aget bindings 0)
          backup (snapshot-bindings dynvars-to-be-overwritten)]
      (restore-snapshot! bindings dynvars-to-be-overwritten)
      #(restore-snapshot! backup dynvars-to-be-overwritten))
    #()))
