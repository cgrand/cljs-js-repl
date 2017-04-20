(ns net.cgrand.cljs.js.repl.dynvars
  (:require-macros [net.cgrand.cljs.js.repl.dynvars :refer [bound-fn binding dynvar with-bindings]])
  (:refer-clojure :exclude [-lookup]))

(defprotocol IDynVar
  (-restore [_ v]))

(deftype DynVar [sym f]
  Object
  (toString [_]
    (str "#<dynvar " sym " >"))
  IDeref
  (-deref [_] (f))
  IDynVar
  (-restore [_ v] (f v)))

(defn- snapshot-bindings [bound-dynvars]
  (let [bound-dynvars (into-array bound-dynvars)
        a #js []]
    (transduce (map-indexed (fn [i ^not-native dv] (aset a i @dv)))
      (constantly nil) bound-dynvars)
    (.push a bound-dynvars)))

(defn- restore-snapshot! [a]
  (let [bound-dynvars (aget a (dec (.-length a)))] ; no pop because destructive
    (transduce (map-indexed (fn [i ^not-native dv] (-restore dv (aget a i))))
      (constantly nil) bound-dynvars)))

(def dynvars "Interned dynvars" {})

(def ^:dynamic *pop!* nil)
(def ^:dynamic *bound-dynvars* #{(dynvar *pop!*) (dynvar *bound-dynvars*)})

(def ^:private specials [(dynvar *pop!*) (dynvar *bound-dynvars*)])

(defn push-frame! [dynvars]
  (let [bound-dynvars (concat specials dynvars)
        snapshot (snapshot-bindings bound-dynvars)]
    (set! *pop!* #(restore-snapshot! snapshot))
    (set! *bound-dynvars* (into *bound-dynvars* dynvars))))

(defn pop-frame! []
  (when-not *pop!*
    (throw (js/Error. "Pop without matching push.")))
  (*pop!*))

(defn get-binding-env
  "WARNING: This is a low-level function.

   Captures the entirety of the dynamic binding environment, 
   usually to restore it in a callback."
  []
  (snapshot-bindings *bound-dynvars*))

(defn unwind! []
  (when *pop!* (*pop!*) (recur)))

(defn bindings-map
  "(DEBUG) Returns a map representation (symbols to values) of the bindings."
  [a]
  (let [n (.-length a)
        bound-dynvars (aget a (dec n))]
    (into {} (map-indexed (fn [i dv] [(.-sym dv) (aget a i)])) bound-dynvars)))

(defn switch-bindings!
  "WARNING: This is a low-level function. Prefer high-level macros like
   with-bindings or bound-fn where ever possible.

   \"Push\" the specified bindings and returns a 0-arg fn to call to \"pop\" them and restore original values."
  [a]
  (if a
    (let [n (.-length a)
          bound-dynvars (aget a (dec n))
          backup (snapshot-bindings bound-dynvars)]
      (restore-snapshot! a)
      #(restore-snapshot! backup))
    (fn [])))
