(ns net.cgrand.lumo.socket-repl
  (:require [net.cgrand.cljs.js.repl.io :as io]
    [net.cgrand.cljs.js.repl.dynvars :as dyn]
    [net.cgrand.cljs.js.repl.main :as m]
    [cljs.js :as cljs]))

(defn socket-reader [socket]
  (let [{:keys [in print-fn]} (io/pipe)]
   (.on socket "data" print-fn)
   (.on socket "close" (fn [& _] (print-fn)))
   in))

(defn socket-printer [socket]
  #(.write socket %))

(defn eval [form cb]
  (try
    (cljs/eval 
      lumo.repl/st ; private
      form
      (into (lumo.repl/make-eval-opts)
        {:context :expr
         :def-emits-var true})
      (fn [{:keys [value ex-info]}]
        (cb value ex-info)))
    (catch :default e
      (cb nil e))))

(defn accept [socket]
  (.setEncoding socket "utf8")
  (let [in (io/line-col-reader (socket-reader socket))
        print-fn (socket-printer socket)]
    (dyn/binding [io/*in* in
                  *print-fn* print-fn
                  *print-err-fn* print-fn]
      (m/repl :eval eval :exit #(.destroy socket)))))

(defn start-server [port]
  (dyn/binding
    [*ns* *ns*
     cljs.js/*eval-fn* cljs.js/*eval-fn*]
    (doto (net.createServer (dyn/bound-fn [socket] (accept socket)))
     (.listen port))))