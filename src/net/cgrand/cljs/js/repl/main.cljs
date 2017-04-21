(ns net.cgrand.cljs.js.repl.main
  (:require [net.cgrand.cljs.js.repl.io :as io]
    [net.cgrand.cljs.js.repl.dynvars :as dyn]
    [net.cgrand.cljs.js.repl.async-reader :as async])
  (:require-macros [net.cgrand.cljs.js.repl.main :refer [with-bindings]])
  (:refer-clojure :exclude [with-bindings]))

(defn repl-prompt
  "Default :prompt hook for repl"
  []
  (print (str (ns-name *ns*) "=>")))

(defn skip-if-eol
  "If the next character on stream s is a newline, skips it, otherwise
  leaves the stream untouched. Returns :line-start, :stream-end, or :body
  to indicate the relative location of the next character on s. The stream
  must be an instance of io/LineColNumberingReader."
  [r cb]
  (let [c (io/read-char r)]
    (cond
      (nil? c) (io/on-ready r #(skip-if-eol % cb))
      (io/eof? c) (cb :stream-end)
      (identical? \newline c) (cb :stream-start)
      :else (do (io/unread r) (cb :body)))))

(defn- line-whitespace? [ch]
  (.test #"^[ \t,]$" ch))

(defn- not-newline? [ch]
  (not (identical? \newline ch)))

(defn- interpret-position [r cb]
  (let [ch (io/read-char r)]
    (cond
      (io/eof? ch) (cb :stream-end)
      (identical? \newline ch) (cb :line-start)
      (identical? \; ch)      
      (if (io/skip r not-newline?)
        (interpret-position r cb)
        (io/on-ready r #(interpret-position % cb)))
      :else (do (io/unread r) (cb :body))))
  true)

(defn skip-whitespace
  "Skips whitespace characters on stream s. Returns :line-start, :stream-end,
  or :body to indicate the relative location of the next character on s.
  Interprets comma as whitespace and semicolon as comment to end of line.
  Does not interpret #! as comment to end of line because only one
  character of lookahead is available. The stream must either be an
  instance of LineNumberingPushbackReader or duplicate its behavior of both
  supporting .unread and collapsing all of CR, LF, and CRLF to a single
  \\newline."
  [r cb]
  (if (io/skip r line-whitespace?)
    (interpret-position r cb)
    (io/on-ready r #(interpret-position % cb))))

(defn repl-read
  "Default :read hook for repl. Reads from io/*in* which must be an
  instance of io/LineColNumberingReader. repl-read:
    - skips whitespace, then
      - passes request-prompt to cb on start of line, or
      - passes request-exit to cb on end of stream, or
      - reads an object from the input stream, then
        - skips the next input character if it's end of line, then
        - passes the object to cb."
  [request-prompt request-exit cb]
  (skip-whitespace io/*in*
    (fn [pos]
      (if-some [status ({:line-start request-prompt :stream-end request-exit} pos)]
        (cb status nil)
        (async/read {:read-cond :allow} io/*in*
          (fn [input ex]
            (if ex
              (cb nil ex)
              (skip-if-eol io/*in* (fn [_] (cb input nil))))))))))

(defn repl-caught
  "Default :caught hook for repl"
  [e]
  (binding [*print-fn* *print-err-fn*]
    (prn e)))

(defn repl
  "Generic, reusable, read-eval-print loop. By default, reads from io/*in*,
  writes to *print-fn*, and prints exception summaries to *print-err-fn*. If you use the
  default :read hook, *in* must be an instance of
  io/LineColNumberingReader.
  Options are sequential keyword-value pairs. Available options and their defaults:

     - :init, function of no arguments, initialization hook called with
       bindings for set!-able vars in place.
       default: #()

     - :exit, function of no arguments, exit hook called with
       default: #()

     - :need-prompt, function of no arguments, called before each
       read-eval-print except the first, the user will be prompted if it
       returns true.
       default: (if (instance? LineNumberingPushbackReader *in*)
                  #(.atLineStart *in*)
                  #(identity true))

     - :prompt, function of no arguments, prompts for more input.
       default: repl-prompt

     - :flush, function of no arguments, flushes output
       default: flush

     - :read, function of two arguments, reads from *in*:
         - returns its first argument to request a fresh prompt
           - depending on need-prompt, this may cause the repl to prompt
             before reading again
         - returns its second argument to request an exit from the repl
         - else returns the next object read from the input stream
       default: repl-read

     - :eval, function of two arguments [form cb] calls cb (a 2-arg fn)
        with either returns the evaluation of form as 1st argument or an
        exception as second argument.

     - :print, function of one argument, prints its argument to the output
       default: prn

     - :caught, function of one argument, a throwable, called when
       read, eval, or print throws an exception or error
       default: repl-caught"
  [& options]
  (let [{:keys [init exit need-prompt prompt flush read eval print caught]
         :or {init        #()
              need-prompt (if (instance? io/LineColNumberingReader io/*in*)
                            #(zero? (.-col io/*in*))
                            #(identity true))
              prompt      repl-prompt
              flush       flush
              read        repl-read
              #_#_eval        eval
              print       prn
              caught      repl-caught
              exit     #{}}}
        (apply hash-map options)
        request-prompt #js {}
        request-exit #js {}]
    (letfn [(caught-ex [e]
              (caught e)
              (set! *e e)
              (prompt-read-eval-print))
            (prompt-read-eval-print []
              (when (need-prompt)
                (prompt)
                (flush))
              (read-eval-print))
            (read-eval-print []
              (read request-prompt request-exit
                (fn [input e]
                  (if e
                    (caught-ex e)
                    (cond
                      (identical? request-prompt input) (prompt-read-eval-print)
                      (identical? request-exit input) (exit)
                      :else
                      (eval input
                        (fn [value e]
                          (if e 
                            (caught-ex e) 
                            (do
                              (print value)
                              (prompt-read-eval-print))))))))))]
      (with-bindings
        (try
          (init)
          (catch :default e
            (caught e)
            (set! *e e)))
      (prompt)
      (flush)
      (read-eval-print)))))
