(ns net.cgrand.cljs.js.repl.async-reader
  (:require [goog.string :as gstring]))
  
(defprotocol AsyncPushbackReader
  (read-char [rdr])
  (unread [rdr])
  (on-ready [rdr f]
    "f is a function of two arguments: an async reader and an array where to push values.
     f will be called as soon as input is available.
     f returns true when no additional input is required."))
  
(def eof? false?)
  
(defn failing-input [e]
  (reify AsyncPushbackReader
    (read-char [rdr] (throw e))
    (unread [rdr] (throw e))
    (on-ready [rdr f] (throw e))))

(defn create-pipe
  "Creates a pipe. Returns a map containing two keys: :in and :print-fn.
  The :in value is an async reader suitable to use as *in* or to pass to read.
  The :print-fn is a fn suitable as *print-fn*. It supports a 0-arity to close the pipe."
  []
  (let [running (volatile! false)
        s (volatile! "")
        idx (volatile! 0)
        cbs (volatile! #js [])
        sentinel (volatile! nil)
        run-callbacks!
        (fn [rdr]
          (when-not @running
            (vreset! running true)
            (try
              (let [conts @cbs]
                (vreset! cbs #js [0 0])
                (loop [rdr rdr]
                  (when rdr
                    (recur
                      (try
                       (when (when-some [cont (.shift conts)] (cont rdr))
                         rdr)
                       (catch :default e
                         (set! (.-length @cbs) 2)
                         (failing-input e))))))
              (do
                (.apply js/Array.prototype.splice conts @cbs)
                (vreset! cbs conts)))
              (finally
                (vreset! running false)))))
        rdr
        (reify AsyncPushbackReader
          (read-char [_]
            (let [i @idx]
              (if-some [ch (aget @s i)]
                (do (vreset! idx (inc i)) ch)
                @sentinel)))
          (unread [_]
            (vswap! idx dec)
            nil)
          (on-ready [rdr cb]
            (.push @cbs cb)
            (run-callbacks! rdr)
            nil))]
    {:print-fn
     (fn 
       ([] ; EOF
         (vreset! sentinel false)
         (run-callbacks! rdr)
         true)
       ([s']
         (when (eof? @sentinel)
           (throw (js/Error. "Can't print on a closed reader.")))
         (vreset! s (str (subs @s @idx) s'))
         (vreset! idx 0) ; set EOF
         (run-callbacks! rdr)
       true))
     :in rdr}))

(def ^:dynamic *in* "A reader implementing AsyncPushbackReader."
  (failing-input (js/Error. "No *in* reader set.")))

;; readers
(declare read-some macros terminating-macros)

(defn skip [rdr pred]
  (let [ch (read-char rdr)]
    (cond
      (eof? ch) true
      (nil? ch) (on-ready rdr #(skip % pred))
      (pred ch) (recur rdr pred)
      :else (do (unread rdr) true))))

(defn whitespace? [ch]
  (gstring/contains " \t\r\n," ch))

(defn read-space [rdr _ _]
  (skip rdr whitespace?))

(defn not-newline? [ch]
  (not (gstring/contains "\r\n" ch)))

(defn read-comment [rdr _ _]
  (skip rdr not-newline?))
  
(defn read-delimited [rdr pa end f a]
  (let [ch (read-char rdr)]
    (cond
      (eof? ch) (throw (js/Error. "EOF while reading")) 
      (= end ch) (do (.push pa (f a)) true)
      :else (if (read-some (doto rdr unread) a) ; nil is implicitely handled
              (recur rdr pa end f a)
              (on-ready rdr #(read-delimited % pa end f a))))))
  
(defn read-list [rdr a _]
  (read-delimited rdr a ")" list* #js []))
  
(defn read-vector [rdr a _]
  (read-delimited rdr a "]" vec #js []))
  
(defn read-set [rdr a _]
  (read-delimited rdr a "}" set #js []))
  
(defn- map* [kvs]
  (let [n (alength kvs)]
    (when (odd? n)
      (throw (js/Error. "Map literal must contain an even number of forms.")))
    (if (<= n (* 2 (.-HASHMAP-THRESHOLD PersistentArrayMap)))
      (.createWithCheck PersistentArrayMap kvs)
      (.createWithCheck PersistentHashMap kvs))))
  
(defn read-map [rdr a _]
  (read-delimited rdr a "}" map* #js []))
  
(defn read-token [rdr a sb]
  (let [ch (read-char rdr)]
    (cond
      (nil? ch) (on-ready rdr #(read-token % a sb))
      (eof? ch) (do (.push a (.toString sb)) true)
      (goog.object/containsKey terminating-macros ch)
      (do (unread rdr) (.push a (.toString sb)) true)
      :else (recur rdr a (.append sb ch)))))
  
  ;;;; begin copy from cljs.reader
(def int-pattern (re-pattern "^([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+))(N)?$"))
(def ratio-pattern (re-pattern "^([-+]?[0-9]+)/([0-9]+)$"))
(def float-pattern (re-pattern "^([-+]?[0-9]+(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?$"))
(def symbol-pattern (re-pattern "^[:]?([^0-9/].*/)?([^0-9/][^/]*)$"))
  
(defn- re-matches* [re s]
  (let [matches (.exec re s)]
    (when (and (not (nil? matches))
               (identical? (aget matches 0) s))
      (if (== (alength matches) 1)
        (aget matches 0)
        matches))))
  
(defn- match-int [s]
  (let [groups (re-matches* int-pattern s)
        ie8-fix  (aget groups 2)
        zero     (if (= ie8-fix "") nil ie8-fix)]
    (if-not (nil? zero)
      0
      (let [a (cond
               (aget groups 3) (array (aget groups 3) 10)
               (aget groups 4) (array (aget groups 4) 16)
               (aget groups 5) (array (aget groups 5) 8)
               (aget groups 6) (array (aget groups 7)
                                      (js/parseInt (aget groups 6) 10))
               :else (array nil nil))
            n (aget a 0)
            radix (aget a 1)]
        (when-not (nil? n)
          (let [parsed (js/parseInt n radix)]
            (if (identical? "-" (aget groups 1))
              (- parsed)
              parsed)))))))
  
(defn- match-ratio [s]
  (let [groups (re-matches* ratio-pattern s)
        numinator (aget groups 1)
        denominator (aget groups 2)]
    (/ (js/parseInt numinator 10) (js/parseInt denominator 10))))
  
(defn- match-float [s]
  (js/parseFloat s))
 
(defn special-symbols [t not-found]
  (cond
    (identical? t "nil") nil
    (identical? t "true") true
    (identical? t "false") false
    (identical? t "/") '/
    :else not-found))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unicode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-2-chars [reader]
  (.toString
    (goog.string/StringBuffer.
      (read-char reader)
      (read-char reader))))

(defn read-4-chars [reader]
  (.toString
    (goog.string/StringBuffer.
      (read-char reader)
      (read-char reader)
      (read-char reader)
      (read-char reader))))

;;;; end copy from cljs.reader

(defn read-tokenized [rdr a ch f]
  (if (read-token rdr a (goog.string/StringBuffer. ch))
    (do (->> (.pop a) f (.push a)) true)
    (on-ready rdr (fn [_] (->> (.pop a) f (.push a)) true))))

(defn as-number [s]
  (if-some [n (cond
                (re-matches* int-pattern s) (match-int s)
                (re-matches* ratio-pattern s) (match-ratio s)
                (re-matches* float-pattern s) (match-float s))]
    n
    (throw (js/Error. (str "Invalid number: " s)))))

(defn read-number [rdr a ch]
  (read-tokenized rdr a ch as-number))

(defn as-symbol [token]
  (if (and (gstring/contains token "/")
        (not (== (.-length token) 1)))
    (symbol (subs token 0 (.indexOf token "/"))
      (subs token (inc (.indexOf token "/"))
        (.-length token)))
    (special-symbols token (symbol token))))

(defn read-symbol [rdr a ch]
  (read-tokenized rdr a ch as-symbol))

(defn read-sym-or-num [rdr a ch]
  (let [ch' (read-char rdr)]
    (cond
      (eof? ch') (read-symbol rdr a ch)
      (nil? ch') (on-ready rdr #(read-sym-or-num % a ch))
      (gstring/contains "0123456789" ch') (do (unread rdr) (read-number rdr a ch))
      :else (do (unread rdr) (read-symbol rdr a ch)))))

(defn make-unicode-char [code-str]
  (when-not (.test #"^[\da-fA-F]{4}$" code-str)
    (throw (js/Error. (str "Invalid unicode character: \\u" code-str))))
  (let [code (js/parseInt code-str 16)]
    (when (<= 0xD800 code 0xDFFF)
      (throw (js/Error. (str "Invalid character constant: \\u" code-str))))
    (js/String.fromCharCode code)))

(defn make-octal-char [code-str]
  (when-not (.test #"^[0-7]{1,3}$" code-str)
    (throw (js/Error. (str "Invalid octal escape sequence: \\o" code-str))))
  (let [code (js/parseInt code-str 8)]
    (when-not (<= 0 code 255)
      (throw (js/Error. "Octal escape sequence must be in range [0, 377].")))
    (js/String.fromCharCode code)))

(defn as-char [chars]
  (cond
    (identical? (.-length chars) 1) chars
    (identical? chars "tab")       "\t"
    (identical? chars "return")    "\r"
    (identical? chars "newline")   "\n"
    (identical? chars "space")     " "
    (identical? chars "backspace") "\b"
    (identical? chars "formfeed")  "\f"
    (identical? (.charAt chars 0) "u") (make-unicode-char (subs chars 1))
    (identical? (.charAt chars 0) "o") (make-octal-char (subs chars 1))
    :else (throw (js/Error. (str "Unknown character literal: \\" chars)))))

(defn read-charlit [rdr a _]
  (read-tokenized rdr a "" as-char))

(defn read-unbalanced [rdr a ch]
  (throw (js/Error. (str "Unmatched delimiter " ch))))

(defn as-keyword [token]
  (let [a (re-matches* symbol-pattern token)
        token (aget a 0)
        ns (aget a 1)
        name (aget a 2)]
    (if (or (and (not (undefined? ns))
                 (identical? (. ns (substring (- (.-length ns) 2) (.-length ns))) ":/"))
            (identical? (aget name (dec (.-length name))) ":")
            (not (== (.indexOf token "::" 1) -1)))
      (throw (js/Error. (str "Invalid token: " token)))
      (if (and (not (nil? ns)) (> (.-length ns) 0))
        (keyword (.substring ns 0 (.indexOf ns "/")) name)
        (keyword token)))))

(defn read-keyword [rdr a _]
  (read-tokenized rdr a "" as-keyword))

(defn read-num-escape [rdr len base n sb]
  (if (pos? len)
    (let [ch (read-char rdr)]
      (cond
        (eof? ch) (throw (js/Error. "EOF while reading"))
        (nil? ch) (on-ready rdr #(read-num-escape % len base n sb))
        :else (let [d (js/parseInt ch base)]
                (if (js/isNaN d)
                  ; not a digit
                  (if (= base 8) ; not exact length
                    (recur (doto rdr unread) 0 8 n sb)
                    (js/Error.
                      (str "Invalid unicode character escape length: " n ", should be 4.") ))
                  (recur rdr (dec len) base (+ (* n base) d) sb)))))
    (do (.append sb (js/String.fromCharCode n)) true)))

(defn parse-string [rdr a esc sb]
  (let [ch (read-char rdr)]
    (cond
      (eof? ch) (throw (js/Error. "EOF while reading"))
      (nil? ch) (on-ready rdr #(parse-string % a esc sb))
      esc (case ch
            \t (recur rdr a false (.append sb "\t"))
            \r (recur rdr a false (.append sb "\r"))
            \n (recur rdr a false (.append sb "\n"))
            (\\ \") (recur rdr a false (.append sb ch))
            \b (recur rdr a false (.append sb "\b"))
            \f (recur rdr a false (.append sb "\f"))
            \u (if (read-num-escape rdr 4 16 0 sb)
                 (recur rdr a false sb)
                 (on-ready rdr #(parse-string rdr a false sb)))
            (\0 \1 \2 \3 \4 \5 \6 \7) (if (read-num-escape rdr 2 8 (js/parseInt ch 8) sb)
                                        (recur rdr a false sb)
                                        (on-ready rdr #(parse-string % a false sb)))
            (throw (js/Error. (str "Unsupported escape character: \\" ch))))
      (identical? "\\" ch) 
      (recur rdr a true sb)
      (identical? \" ch) (do (.push a (.toString sb)) true)
      :else (recur rdr a esc (doto sb (.append ch))))))

(defn read-stringlit [rdr a _]
  (parse-string rdr a false (gstring/StringBuffer.)))

(defn- skip-form [rdr a n]
  (if (> (.-length a) n)
    (do (.pop a) true)
    (if (read-some rdr a)
      (recur rdr a n)
      (on-ready rdr #(skip-form % a n)))))

(defn read-null [rdr a _]
  (skip-form rdr a (.-length a)))

(def dispatch-macros
  (clj->js
    {"{" read-set
     "_" read-null
     "!" read-comment}))

(defn read-dispatch [rdr a _]
  (let [ch (read-char rdr)]
    (cond
      (eof? ch) (throw (js/Error. "EOF while reading"))
      (nil? ch) (on-ready rdr #(read-dispatch % a nil))
      :else
      (if-some [f (goog.object/get dispatch-macros ch nil)]
        (f rdr a ch)
        (throw (js/Error. (str "No dispatch macro for #" ch)))))))

(def all-macros
  (->
    {"\"" read-stringlit 
     ":" read-keyword
     ";" read-comment
     "(" read-list
     "[" read-vector
     "{" read-map
     "}" read-unbalanced
     "]" read-unbalanced
     ")" read-unbalanced
     "\\" read-charlit
     "#" read-dispatch
     "+" read-sym-or-num
     "-" read-sym-or-num}
    (into (map vector "0123456789" (repeat read-number)))
    (into (map vector "\t\n\r ," (repeat read-space)))
    clj->js))
  
(defn macros [ch]
  (goog.object/get all-macros ch read-symbol))
  
(def terminating-macros
  (reduce (fn [o ch] (doto o (goog.object/remove ch))) 
    (goog.object/clone all-macros) ":+-0123456789#'%"))
  
(defn read-some
  "Read at most one form and pushes it to a."
  [rdr a]
  (let [ch (read-char rdr)]
    (cond
      (eof? ch) (throw (js/Error. "EOF while reading"))
      (nil? ch) (on-ready rdr #(read-some % a))
      :else (if-some [r (macros ch)]
              (r rdr a ch)
              (throw (js/Error. (str "Unexpected character: " (pr-str ch))))))))

(defn safe-read-some [eof-value]
  (fn self [rdr a]
    (let [ch (read-char rdr)]
      (cond
        (eof? ch) (do (.push a eof-value) true)
        (nil? ch) (on-ready rdr #(self % a))
        :else (if-some [r (macros ch)]
                (r rdr a ch)
                (throw (js/Error. (str "Unexpected character: " (pr-str ch)))))))))
  
;; root readers
(defn- read1 [rdr a root-cb read-some]
  (let [ex (volatile! nil)
        r (or (pos? (alength a)) (try (read-some rdr a) (catch :default e (vreset! ex e))))]
    (cond
      @ex (do (root-cb nil @ex) true)
      (pos? (alength a)) (do (root-cb (aget a 0) nil) true)
      :else (on-ready rdr #(read1 % a root-cb read-some)))))
  
(defn read
  "Like usual read but takes an additional last argument: a callback.
   The callback takes two arguments value and error. It will be called when a value is read or
   an error thrown."
  ([cb] (read *in* cb))
  ([rdr cb]
    (read rdr cb true nil))
  ([opts rdr cb] :TODO)
  ([rdr cb eof-error? eof-value]
    (read1 rdr #js [] cb (if eof-error? read-some (safe-read-some eof-value)))))

(defn read-string
  ([s]
    (let [{:keys [in print-fn]} (create-pipe)
          ret #js [nil nil]]
      (print-fn s)
      (print-fn)
      (read in (fn [v ex] (doto ret (aset 0 v) (aset 1 ex))))
      (if-some [ex (aget ret 1)]
        (throw ex)
        (aget ret 0))))
  ([opts s]))

(comment
  (let [{:keys [in] write :print-fn} (create-pipe)] 
    (read in (partial prn '>))
    (write "(12(:fo")
    (write "o))]32")
    (read in (partial prn '>>))
    (read in (partial prn '>>>)))
  
  (let [{:keys [in] write :print-fn} (create-pipe)]
    (write "; hello ")
    (read in (partial prn '>))
    (read in (partial prn '>>) false :eof)
    (write "4\n32")
    (write)))


(comment

  (def repl
    (letfn [(rep []
              (println (str (ns-name *ns*)) "=>")
              (read (fn [form ex]
                      (if ex
                        (caught ex)
                        (eval form
                          (fn [val ex]
                            (if ex
                              (caught ex)
                              (do
                                (prn val)
                                (rep)))))))))
            (caught [ex]
              (binding [*print-fn* *print-err-fn*]
                (prn ex)))]
      rep))
  
   
              (defn repl-template [prompt read eval print caught]
                (prompt)
                (read
                  (fn self [form e]
                    (try
                      (if e (throw e) (print (eval form)))
                      (catch :default e
                        (caught e)))
                    (prompt)
                    (read self))))
  
              (def repl (repl-template #(println (str (ns-name *ns*) "=>"))
                          read eval prn #(binding [*print-fn* *print-err-fn*] (prn %))))
  
)