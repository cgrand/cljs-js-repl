(ns net.cgrand.cljs.js.repl.async-reader
  (:require [goog.string :as gstring]
    [net.cgrand.cljs.js.repl.dynvars :as dyn]))
  
(defprotocol AsyncPushbackReader
  (read-char [rdr])
  (unread [rdr])
  (-on-ready [rdr toprdr f])
  (-info [rdr]))

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
    (throw (.-e rdr))))

(deftype Pipe [^:mutable running ^:mutable s ^:mutable idx ^:mutable cb+rdrs ^:mutable sentinel
               ^:mutable bindings]
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
      (do ; not running
        (when cb
          (when (pos? (.-length cb+rdrs))
            (throw (js/Error. "This reader has already one consumer.")))
          (.push cb+rdrs cb)
          (.push cb+rdrs rdr))
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
                    r (recur r)))))
            (.apply js/Array.prototype.splice conts cb+rdrs)
            (set! cb+rdrs conts)
            (set! bindings (when (pos? (.-length cb+rdrs))
                             (dyn/get-bindings)))
            (set! running false)))))
    nil)
  IFn
  (-invoke [rdr] ; EOF
    (set! sentinel false))
  (-invoke [rdr s'] ; append input
    (when (eof? sentinel)
      (throw (js/Error. "Can't print on a closed reader.")))
    (set! s (str (subs s idx) s'))
    (set! idx 0)))

(defn create-pipe
  "Creates a pipe. Returns a map containing two keys: :in and :print-fn.
  The :in value is an async reader suitable to use as *in* or to pass to read.
  The :print-fn is a fn suitable as *print-fn*. It supports a 0-arity to close the pipe."
  []
  (let [pipe (Pipe. false "" 0 #js [] nil nil)]
    {:print-fn (fn
                 ([] (pipe) (on-ready pipe nil))
                 ([s] (pipe s) (on-ready pipe nil)))
     :in pipe}))

(def ^:dynamic *in* "A reader implementing AsyncPushbackReader."
  (FailingReader. (js/Error. "No *in* reader set.")))

(def ^:dynamic *error-data*)

;; readers
(defn reader-error! [& msg]
  (throw 
    (ex-info (apply str msg) (*error-data*))))

(defn eof-error! []
  (reader-error! "EOF while reading."))

(declare read-some macros ^boolean terminating-macros)

(defn skip [^not-native rdr pred]
  (let [ch (read-char rdr)]
    (cond
      (pred ch) (recur rdr pred)
      (eof? ch) true
      (nil? ch) (on-ready rdr #(skip % pred))
      :else (do (unread rdr) true))))

(defn- ^boolean whitespace?
  "Checks whether a given character is whitespace"
  [ch]
  (.test #"^[ \t\n\r,]$" ch))

(defn read-space [rdr _ _]
  (skip rdr whitespace?))

(defn- ^boolean not-newline? [ch]
  (.test #"^[^\r\n]$" ch))

(defn read-comment [rdr _ _]
  (skip rdr not-newline?))
  
(defn read-delimited [^not-native rdr pa end f a]
  (let [ch (read-char rdr)]
    (cond
      (nil? ch) (on-ready rdr #(read-delimited % pa end f a))
      (eof? ch) (eof-error!) 
      (identical? end ch) (do (.push pa (f a)) true)
      :else (if (read-some (doto rdr unread) a)
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
      (reader-error! "Map literal must contain an even number of forms."))
    (if (<= n (* 2 (.-HASHMAP-THRESHOLD PersistentArrayMap)))
      (.createWithCheck PersistentArrayMap kvs)
      (.createWithCheck PersistentHashMap kvs))))
  
(defn read-map [rdr a _]
  (read-delimited rdr a "}" map* #js []))
  
(defn read-token [^not-native rdr a sb]
  (let [ch (read-char rdr)]
    (cond
      (nil? ch) (on-ready rdr #(read-token % a sb))
      (eof? ch) (do (.push a (.toString sb)) true)
      (terminating-macros ch)
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

(defn read-tokenized [^not-native rdr a ch f]
  (if (read-token rdr a (goog.string/StringBuffer. ch))
    (do (->> (.pop a) f (.push a)) true)
    (on-ready rdr (fn [_] (->> (.pop a) f (.push a)) true))))

(defn as-number [s]
  (if-some [n (cond
                (re-matches* int-pattern s) (match-int s)
                (re-matches* ratio-pattern s) (match-ratio s)
                (re-matches* float-pattern s) (match-float s))]
    n
    (reader-error! "Invalid number: " s)))

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

(defn read-sym-or-num [^not-native rdr a ch]
  (let [ch' (read-char rdr)]
    (cond
      (eof? ch') (read-symbol rdr a ch)
      (nil? ch') (on-ready rdr #(read-sym-or-num % a ch))
      (.test #"^\d$" ch') (do (unread rdr) (read-number rdr a ch))
      :else (do (unread rdr) (read-symbol rdr a ch)))))

(defn make-unicode-char [code-str]
  (when-not (.test #"^[\da-fA-F]{4}$" code-str)
    (reader-error! "Invalid unicode character: \\u" code-str))
  (let [code (js/parseInt code-str 16)]
    (when (<= 0xD800 code 0xDFFF)
      (reader-error! "Invalid character constant: \\u" code-str))
    (js/String.fromCharCode code)))

(defn make-octal-char [code-str]
  (when-not (.test #"^[0-7]{1,3}$" code-str)
    (reader-error! "Invalid octal escape sequence: \\o" code-str))
  (let [code (js/parseInt code-str 8)]
    (when-not (<= 0 code 255)
      (reader-error! "Octal escape sequence must be in range [0, 377]."))
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
    :else (reader-error! "Unknown character literal: \\" chars)))

(defn read-charlit [rdr a _]
  (read-tokenized rdr a "" as-char))

(defn read-unbalanced [rdr a ch]
  (reader-error! "Unmatched delimiter " ch))

(defn as-keyword [token]
  (let [a (re-matches* symbol-pattern token)
        token (aget a 0)
        ns (aget a 1)
        name (aget a 2)]
    (if (or (and (not (undefined? ns))
                 (identical? (. ns (substring (- (.-length ns) 2) (.-length ns))) ":/"))
            (identical? (aget name (dec (.-length name))) ":")
            (not (== (.indexOf token "::" 1) -1)))
      (reader-error! "Invalid token: " token)
      (if (and (not (nil? ns)) (> (.-length ns) 0))
        (keyword (.substring ns 0 (.indexOf ns "/")) name)
        (keyword token)))))

(defn read-keyword [rdr a _]
  (read-tokenized rdr a "" as-keyword))

(defn read-num-escape [^not-native rdr len base n sb]
  (if (pos? len)
    (let [ch (read-char rdr)]
      (cond
        (eof? ch) (eof-error!)
        (nil? ch) (on-ready rdr #(read-num-escape % len base n sb))
        :else (let [d (js/parseInt ch base)]
                (if (js/isNaN d)
                  ; not a digit
                  (if (= base 8) ; not exact length
                    (recur (doto rdr unread) 0 8 n sb)
                    (reader-error! "Invalid unicode character escape length: " n ", should be 4."))
                  (recur rdr (dec len) base (+ (* n base) d) sb)))))
    (do (.append sb (js/String.fromCharCode n)) true)))

(defn parse-string [^not-native rdr a esc sb]
  (let [ch (read-char rdr)]
    (cond
      (eof? ch) (eof-error!)
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
            (reader-error! "Unsupported escape character: \\" ch))
      (identical? "\\" ch) 
      (recur rdr a true sb)
      (identical? \" ch) (do (.push a (.toString sb)) true)
      :else (recur rdr a esc (doto sb (.append ch))))))

(defn read-stringlit [rdr a _]
  (parse-string rdr a false (gstring/StringBuffer.)))

(defn- readN [^not-native rdr a upto]
  (cond
    (< (.-length a) upto)
    (if (read-some rdr a)
      (recur rdr a upto)
      (on-ready rdr #(readN % a upto)))
    (> (.-length a) upto) (reader-error! "Reader bug: read too many forms.")
    :else true))

(defn read-quote [rdr a _]
  (if (readN rdr a (inc (.-length a)))
    (do (.push a (list 'quote (.pop a))) true)
    (on-ready rdr #(do (check %) (.push a (list 'quote (.pop a))) true))))

(defn read-null [rdr a _]
  (if (readN rdr a (inc (.-length a)))
    (do (.pop a) true)
    (on-ready rdr #(do (check %) (.pop a) true))))

(defn- fold-meta [a]
  (let [v (.pop a)
        m (.pop a)
        m (cond
            (map? m) m
            (keyword? m) {m true}
            (or (symbol? m) (string? m)) {:tag m}
            :else (reader-error! "Metadata must be Symbol,Keyword,String or Map"))]
    (.push a (vary-meta v merge m))
    true))

(defn read-meta [rdr a _]
  (if (readN rdr a (+ 2 (.-length a)))
    (fold-meta a)
    (on-ready rdr #(do (check %) (fold-meta a)))))


(defn- tag-line+col [rdr a line col]
  (let [end-line (.-line rdr)
        end-col (.-col rdr)
        v (.pop a)]
    (when-not (or (boolean? v) (nil? v)) ; as read-symbol may push them
      (.push a (vary-meta v merge {:line line :column col :end-line end-line :end-column end-col})))
    true))

(defn with-line+col
  ([r] (with-line+col r 1))
  ([r offset]
    (fn [rdr a ch]
      (if (instance? LineColNumberingReader rdr)
        (let [line (.-line rdr)
              col (- (.-col rdr) offset)]
          (if (r rdr a ch)
            (tag-line+col rdr a line col)
            (on-ready rdr #(do (check %) (tag-line+col % a line col)))))
        (r rdr a ch)))))

(def ^:dynamic *dispatch-macros* #js [])
(def ^:dynamic *default-dispatch-macro* #(str "No dispatch macro for #" %3))

(defn read-dispatch [^not-native rdr a _]
  (let [ch (read-char rdr)]
    (cond
      (eof? ch) (eof-error!)
      (nil? ch) (on-ready rdr #(read-dispatch % a nil))
      :else
      (let [f (or (aget *dispatch-macros* (.charCodeAt ch 0)) *default-dispatch-macro*)]
        (f rdr a ch)))))

(def ^:dynamic *macros* #js [])

(def ^:dynamic *default-macro* #(str "No macro for #" %3))

(defn macros [ch]
  (or (aget *macros* (.charCodeAt ch 0)) *default-macro*))

(def ^:dynamic *terminating-macros* #js [])

(defn ^boolean terminating-macros [ch]
  (some? (aget *terminating-macros* (.charCodeAt ch 0))))

(defn read-some
  "Read at most one form and pushes it to a."
  [^not-native rdr a]
  (let [ch (read-char rdr)]
    (cond
      (eof? ch) (eof-error!)
      (nil? ch) (on-ready rdr #(read-some % a))
      :else (if-some [r (macros ch)]
              (r rdr a ch)
              (reader-error! "Unexpected character: " (pr-str ch))))))

(defn safe-read-some [eof-value]
  (fn self [^not-native rdr a]
    (let [ch (read-char rdr)]
      (cond
        (eof? ch) (do (.push a eof-value) true)
        (nil? ch) (on-ready rdr #(self % a))
        :else (if-some [r (macros ch)]
                (r rdr a ch)
                (reader-error! "Unexpected character: " (pr-str ch)))))))
  
;; root readers
(defn- read1 [a root-cb read-some]
  (let [ex (volatile! nil)]
    (fn self [^not-native rdr]
      (let [r (or (pos? (alength a)) (try (read-some rdr a) (catch :default e (vreset! ex e))))]
        (cond
          @ex (do (root-cb nil @ex) true)
          (pos? (alength a)) (do (root-cb (aget a 0) nil) true)
          :else (on-ready rdr self))))))

(defn macros-table [m]
  (let [a (object-array 128)]
    (doseq [[ch f] m]
      (aset a (.charCodeAt ch 0) f))
    a))

(def cljs-dispatch-macros
  {"{" (with-line+col read-set 2)
   "_" read-null
   "^" (with-line+col read-meta 2)
   "!" read-comment})

(def cljs-macros
  (->
    {"\"" read-stringlit 
     ":" read-keyword
     ";" read-comment
     "(" (with-line+col read-list)
     "[" (with-line+col read-vector)
     "{" (with-line+col read-map)
     "}" read-unbalanced
     "]" read-unbalanced
     ")" read-unbalanced
     "\\" read-charlit
     "#" read-dispatch
     "^" (with-line+col read-meta 1)
     "+" read-sym-or-num
     "-" read-sym-or-num
     "'" (with-line+col read-quote)}
    (into (map vector "0123456789" (repeat read-number)))
    (into (map vector "\t\n\r ," (repeat read-space)))))

(def cljs-terminating-macros
  (reduce dissoc cljs-macros ":+-0123456789#'%"))

(def ^:private default-read-opts
  {:eof :eofthrow
   :macros (macros-table cljs-macros)
   :default-macro (with-line+col read-symbol)
   :terminating-macros (macros-table cljs-terminating-macros)
   :dispatch-macros (macros-table cljs-dispatch-macros)
   :default-dispatch-macro #(str "No dispatch macro for #" %3)})

(defn read
  "Like usual read but takes an additional last argument: a callback.
   The callback takes two arguments value and error. It will be called when a value is read or
   an error thrown."
  ([cb] (read *in* cb))
  ([rdr cb]
    (read rdr cb true nil))
  ([opts rdr cb]
    (let [{:keys [eof macros default-macro terminating-macros dispatch-macros default-dispatch-macro]} (into default-read-opts opts)]
      (dyn/binding [*macros* macros
                    *default-macro* default-macro
                    *terminating-macros* terminating-macros
                    *dispatch-macros* dispatch-macros
                    *default-dispatch-macro* default-dispatch-macro
                    *error-data* #(-info rdr)]
        (on-ready rdr (read1 #js [] cb (if (= :eofthrow eof) read-some (safe-read-some eof)))))))
  ([rdr cb eof-error? eof-value]
    (read {:eof (if eof-error? :eof-throw eof-value)} rdr cb)))

(defn read-string
  ([s]
    (let [{:keys [in print-fn]} (create-pipe)
          in (line-col-reader in)
          ret #js [nil nil]]
      (print-fn s)
      (print-fn)
      (read in (fn [v ex] (doto ret (aset 0 v) (aset 1 ex))))
      (if-some [ex (aget ret 1)]
        (throw ex)
        (aget ret 0))))
  ([opts s]))

(defn with-string [s f]
  (let [{:keys [in print-fn]} (create-pipe)
        ret #js [nil nil]]
    (print-fn s)
    (print-fn)
    (on-ready in f)))

(comment
  (let [{:keys [in] write :print-fn} (r/create-pipe)] 
    (write "(12(:fo")
    (r/read in (partial prn '>))
    
    (write "o))]32")))


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