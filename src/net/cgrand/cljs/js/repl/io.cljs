(ns net.cgrand.cljs.js.repl.io
  (:require [net.cgrand.cljs.js.repl.dynvars :as dyn])
  (:require-macros [net.cgrand.cljs.js.repl.io]))

(defprotocol AsyncPushbackReader
  "Protocol for asynchronous character streams."
  (read-char [rdr]
    "Returns either a character, nil (no more data at the moment), eof (false)")
  (unread [rdr]
    "Unreads the last read character, can only be called once and after a successful read-char (no nil no eof)")
  (-on-ready [rdr toprdr f]
    "When data is available, call f with argument toprdr.")
  (-info [rdr]
    "Return a map about the state of the reader."))

(defn on-ready
  "f is a function of two arguments: an async reader and an array where to push values.
   f will be called as soon as input is available.
   f must return true when no additional input is required."
  [rdr f]
  (-on-ready rdr rdr f))

(def eof? false?)

(defn- skip-lf [^not-native rdr]
  (let [ch (read-char rdr)]
    (cond
      (identical? "\n" ch) true
      (nil? ch) (on-ready rdr skip-lf)
      :else (do (unread rdr) true))))

(deftype LineColNumberingReader [^not-native prdr ^:mutable col ^:mutable pcol ^:mutable line]
  ; In addition to keeping tack of line/column, it collapses CR, LF, and CRLF into a single \newline.
  AsyncPushbackReader
  (read-char [rdr]
    (let [ch (read-char prdr)]
      (cond
        (identical? ch "\r") 
        (do
          (set! line (inc line))
          (set! pcol col)
          (set! col 0)
          (let [ch (read-char prdr)]
            (cond
              (identical? "\n" ch) "\n"
              (nil? ch) (on-ready rdr skip-lf)
              :else (do (unread prdr) "\n")))
          (set! skip-lf true)
          "\n")
        (identical? ch "\n") (do
                               (set! line (inc line))
                               (set! pcol col)
                               (set! col 0)
                               ch)
        :else (do
                (set! col (inc col))
                ch))))
  (unread [rdr]
    (unread prdr)
    (let [ch (read-char prdr)]
      (unread prdr)
      ; assuming that never more than one character is unread
      (if (or (identical? "\n" ch) (identical? "\r" ch))
        (do
          (set! line (dec line))
          (set! col pcol))
        (set! col (dec col)))))
  (-on-ready [_ rdr f]
    (-on-ready prdr rdr f))
  (-info [rdr] (assoc (-info prdr) :line line :column col)))

(defn line-col-reader
  ([rdr]
    (line-col-reader rdr 1 0))
  ([rdr line]
    (line-col-reader rdr line 0))
  ([rdr line col]
    (LineColNumberingReader. rdr col 0 line)))

(deftype FailingReader [e]
  AsyncPushbackReader
  (read-char [rdr] (throw e))
  (unread [rdr] (throw e))
  (-on-ready [rdr rdr f] (throw e))
  (-info [rdr] {}))

(defn check [rdr]
  (when (instance? FailingReader rdr)
    (throw (.-e rdr)))
  true)

(deftype Pipe [^:mutable running ^:mutable s ^:mutable idx ^:mutable cb+rdrs ^:mutable sentinel
               ^:mutable bindings waiting-cbs+readers+envs]
  AsyncPushbackReader
  (read-char [_]
    (if-some [ch (aget s idx)]
      (do (set! idx (inc idx)) ch)
      sentinel))
  (unread [_]
    (set! idx (dec idx))
    nil)
  (-info [rdr] {})
  (-on-ready [_ rdr cb]
    (if running
      (when cb (.push cb+rdrs cb) (.push cb+rdrs rdr))
      ; not running
      (do
        ; any callback registred while "not running" is a new root callback and
        ; should be enqueued
        (when cb
          (.push waiting-cbs+readers+envs cb rdr (dyn/get-binding-env)))
        ; if nor more active "stack", create one from the queue (if any)
        (when (and (zero? (.-length cb+rdrs)) (pos? (.-length waiting-cbs+readers+envs)))
          (.push cb+rdrs (.shift waiting-cbs+readers+envs)) ; cb
          (.push cb+rdrs (.shift waiting-cbs+readers+envs)) ; rdr
          (set! bindings (.shift waiting-cbs+readers+envs)))
        (when (and (pos? (.-length cb+rdrs))
                (or (< idx (.-length s)) (eof? sentinel)))
          ; cbs and input, let's go!
          (dyn/with-bindings bindings
            (set! running true)
            (let [conts cb+rdrs]
              (set! cb+rdrs #js [0 0])
              (loop [erdr nil]
                (when-some [cont (.shift conts)]
                  (let [rdr (.shift conts)
                        r (try (boolean (cont (or erdr rdr)))
                            (catch :default e
                              (set! (.-length cb+rdrs) 2)
                              (FailingReader. e)))]
                    (cond
                      (true? r) (recur nil)
                      r (recur r))))) ; else (falsey) exit loop
              (.apply js/Array.prototype.splice conts cb+rdrs)
              (set! cb+rdrs conts)
              (set! bindings (when (pos? (.-length cb+rdrs))
                               (dyn/get-binding-env)))
              (set! running false)))
          (cond
            (pos? (.-length cb+rdrs)) false ; returns false when pending cbs
            (pos? (.-length waiting-cbs+readers+envs)) (recur nil nil nil)
            :else true))))) ; returns true when all clear
  IFn
  (-invoke [rdr] ; EOF
    (set! sentinel false))
  (-invoke [rdr s'] ; append input
    (when (eof? sentinel)
      (throw (js/Error. "Can't print on a closed reader.")))
    (set! s (str (subs s idx) s'))
    (set! idx 0)))

(defn pipe
  "Creates a pipe. Returns a map containing two keys: :in and :print-fn.
  The :in value is an async reader suitable to use as *in* or to pass to read.
  The :print-fn is a fn suitable as *print-fn*. It supports a 0-arity to close the pipe."
  []
  (let [pipe (Pipe. false "" 0 #js [] nil nil #js [])]
    {:print-fn (fn
                 ([] (pipe) (on-ready pipe nil))
                 ([s] (pipe s) (on-ready pipe nil)))
     :in pipe}))

(defn string-reader [s]
  (let [pipe (Pipe. false "" 0 #js [] nil nil #js [])]
    (pipe s)
    (pipe)
    pipe))

(defn skip [^not-native rdr pred]
  (let [ch (read-char rdr)]
    (cond
      (eof? ch) true
      (nil? ch) (on-ready rdr #(skip % pred))
      (pred ch) (recur rdr pred)
      :else (do (unread rdr) true))))

(def ^:dynamic *in* "A reader implementing AsyncPushbackReader."
  (FailingReader. (js/Error. "No *in* reader set.")))
