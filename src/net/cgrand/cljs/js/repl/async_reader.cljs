(ns net.cgrand.cljs.js.repl.async-reader
  (:require [goog.string :as gstring]
    [net.cgrand.cljs.js.repl.io :as io :refer [read-char unread on-ready eof? check skip]]
    [net.cgrand.cljs.js.repl.dynvars :as dyn]
    [cljs.analyzer :as ana]
    [cljs.env :as env]))  

(def ^:dynamic *error-data*)

(def ^:dynamic *dispatch-macros* #js [])
(def ^:dynamic *default-dispatch-macro* #(str "No dispatch macro for #" %3))

(def ^:dynamic *suppress-read* false)
(def ^:dynamic *read-eval* false)
(def ^:dynamic *data-readers* {})
(def ^:dynamic  *default-data-reader-fn* nil)

(def ^:dynamic *reading-cond* "true while reading a reader conditional" false)

(def ^:dynamic *read-cond-mode* ":allow or :preserve or falsey")

(def ^:dynamic *read-cond-features* #{})

(def ^:dynamic *autogensym*)
(def ^:dynamic *resolve* "A two function args (first arg bieing either :ns or :symbol) to perform resolution")

;; readers
(defn reader-error! [& msg]
  (throw 
    (ex-info (apply str msg) (*error-data*))))

(defn eof-error! []
  (reader-error! "EOF while reading."))

(declare read-some macros ^boolean terminating-macros)

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

(defn- list' [a]
  (loop [l () i (dec (.-length a))]
    (if (neg? i)
      l
      (recur (conj l (aget a i)) (dec i)))))

(defn read-list [rdr a _]
  (read-delimited rdr a ")" list' #js []))
  
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
  
(def symbol-pattern (re-pattern "^(::|:)?(?:([^0-9/:][^/]*)/)?([^0-9].*)$"))

;;;; begin copy from cljs.reader
(def int-pattern (re-pattern "^([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+))(N)?$"))
(def ratio-pattern (re-pattern "^([-+]?[0-9]+)/([0-9]+)$"))
(def float-pattern (re-pattern "^([-+]?[0-9]+(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?$"))
  
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
    (on-ready rdr #(do (check %) (->> (.pop a) f (.push a)) true))))

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
  (let [a (re-matches* symbol-pattern token)
        token (aget a 0)
        ns (aget a 2)
        name (aget a 3)]
    (cond
      ns (symbol ns name)
      (identical? "true" name) true
      (identical? "false" name) false
      (identical? "nil" name) nil
      :else (symbol nil name))))

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

(defn resolve-ns [x] (*resolve* :ns x))

(defn resolve-symbol [x] (*resolve* :symbol x))

(defn as-keyword [token]
  (let [a (re-matches* symbol-pattern token)
        token (aget a 0)
        auto (identical? "::" (aget a 1))
        ns (aget a 2)
        kwname (aget a 3)
        ns (if auto
             (name
               (if ns
                 (or (resolve-ns ns) (reader-error! "Unknown namespace alias: " ns))
                 (ns-name *ns*)))
             ns)]
    (keyword ns kwname)))

(defn read-keyword [rdr a _]
  (read-tokenized rdr a ":" as-keyword))

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
      :else (recur rdr a esc (.append sb ch)))))

(defn read-stringlit [rdr a _]
  (parse-string rdr a false (gstring/StringBuffer.)))

(defn parse-regex [^not-native rdr a esc sb]
  (let [ch (read-char rdr)]
    (cond
      (eof? ch) (eof-error!)
      (nil? ch) (on-ready rdr #(parse-regex % a esc sb))
      esc (recur rdr a false
            (if (or (identical? \" ch) (identical? \\ ch))
              (.append sb ch)
              (-> (.append sb \\) (.append sb ch))))
      (identical? "\\" ch) (recur rdr a true sb)
      (identical? \" ch) (do (.push a (re-pattern (.toString sb))) true)
      :else (recur rdr a esc (.append sb ch)))))

(defn read-regex [rdr a _]
  (parse-regex rdr a false (gstring/StringBuffer.)))

(defn- readN [^not-native rdr a upto]
  (cond
    (< (.-length a) upto)
    (if (read-some rdr a)
      (recur rdr a upto)
      (on-ready rdr #(readN % a upto)))
    (> (.-length a) upto) (reader-error! "Reader bug: read too many forms.")
    :else true))

(defn read1 [rdr a]
  (readN rdr a (inc (.-length a))))

(defn read2 [rdr a]
  (readN rdr a (+ 2 (.-length a))))

(defn read-wrap [sym]
  (fn [rdr a _]
    (if (read1 rdr a)
      (do (.push a (list sym (.pop a))) true)
      (on-ready rdr #(do (check %) (.push a (list sym (.pop a))) true)))))

(defn read-discard [rdr a _]
  (if (io/bound-read [*suppress-read* true] (read1 rdr a))
    (do (.pop a) true)
    (on-ready rdr #(do (.pop a) true))))

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
  (if (read2 rdr a)
    (fold-meta a)
    (on-ready rdr #(do (check %) (fold-meta a)))))

(defn- tag-line+col [rdr a line col]
  (let [end-line (.-line rdr)
        end-col (.-col rdr)
        v (.pop a)]
    (.push a
      (if (or (boolean? v) (nil? v)) ; as read-symbol may push them
        v
        (vary-meta v merge {:line line :column col :end-line end-line :end-column end-col})))
    true))

(defn with-line+col
  ([r] (with-line+col r 1))
  ([r offset]
    (fn [rdr a ch]
      (if (instance? io/LineColNumberingReader rdr)
        (let [line (.-line rdr)
              col (- (.-col rdr) offset)]
          (if (r rdr a ch)
            (tag-line+col rdr a line col)
            (on-ready rdr #(do (check %) (tag-line+col % a line col)))))
        (r rdr a ch)))))

(defn preserve-read-cond? []
  (and *reading-cond* (= :preserve *read-cond-mode*)))

(defn- fold-ctor [a]
  (let [form (.pop a)
        tag (.pop a)]
    (when-not (symbol? tag)
      (reader-error! "Reader tag must be a symbol."))
    (if (or (preserve-read-cond?) *suppress-read*)
      (tagged-literal tag form)
      (if-some [[pre post] (re-matches #"(.*)\.([^.]*)" (name tag))]
        (do ; map form only
          (when-not *read-eval*
            (reader-error! "Record construction syntax can only be used when *read-eval* is true"))
          (.push a (list (symbol (str pre "/map->" post)) form)))
        (if-some [f (or (get *data-readers* tag) *default-data-reader-fn*)]
          (f tag form)
          (reader-error! "No reader function for tag " tag))))
    true))

(defn read-ctor [rdr a]
  (unread rdr)
  (if (read2 rdr a)
    (fold-ctor a)
    (on-ready rdr #(do (check %) (fold-ctor a)))))

(def ^:private reserved-features #{:else :none})

(defn has-feature? [feature]
  (when-not (keyword? feature)
    (reader-error! "Feature should be a keyword: " feature))
  (or (= :default feature) (contains? *read-cond-features* feature)))

(defn read-cond-list [^not-native rdr pa a splicing]
  (let [ch (read-char rdr)]
    (cond
      (nil? ch) (on-ready rdr #(read-cond-list % pa a splicing))
      (eof? ch) (eof-error!) 
      (identical? ")" ch) 
      (case (.-length a)
        0 true
        2 (let [form (aget a 1)]
            (if splicing
              (if (sequential? form)
                (.apply js/Array.prototype.push pa form)
                (reader-error! "Spliced form list in read-cond-splicing must be sequential."))
              (.push pa form))
            true)
        (1 3) (reader-error! "read-cond requires an even number of forms."))
      :else (do
              (unread rdr)
              (case (.-length a)
                (0 2)
                (if (read-some rdr a)
                  (recur rdr pa a splicing)
                  (on-ready rdr #(read-cond-list % pa a splicing)))
                1
                (let [feature (aget a 0)]
                  (when (reserved-features feature)
                    (reader-error! "Feature name " feature " is reserved."))
                  (if (if (has-feature? feature)
                        (read1 rdr a)
                        (read-discard rdr (doto a .pop) "_"))
                    (recur rdr pa a splicing)
                    (on-ready rdr #(read-cond-list % pa a splicing))))
                3
                (if (read-discard rdr a "_")
                  (recur rdr pa (doto a .pop) splicing)
                  (on-ready rdr
                    #(read-cond-list % pa (doto a .pop) splicing))))))))

(defrecord ReaderConditional [form splicing?])
(defn reader-conditional [form splicing?] (ReaderConditional. form splicing?))
(defn reader-conditional? [x] (instance? ReaderConditional x))

(defn- fold-reader-conditional [a splicing]
  (let [form (.pop a)]
    (when (odd? (count form))
      (reader-error! "read-cond requires an even number of forms."))
    (.push a (reader-conditional form splicing))
    true))

(defn read-cond-body [^not-native rdr a splicing]
  (let [ch (read-char rdr)]
    (cond
      (nil? ch) (on-ready rdr #(read-cond-body % a splicing))
      (eof? ch) (eof-error!)
      (identical? "(" ch)
      (if (preserve-read-cond?)
        (if (read-list rdr a ch)
          (fold-reader-conditional a splicing)
          (on-ready rdr #(do (check %) (fold-reader-conditional a splicing))))
        (read-cond-list rdr a #js [] splicing))
      :else (reader-error! "read-cond body must be a list."))))

(defn read-cond [^not-native rdr a _]
  (when-not (#{:allow :preserve} *read-cond-mode*)
    (reader-error! "Conditional read not allowed"))
  (let [ch (read-char rdr)]
    (cond
      (nil? ch) (on-ready rdr #(read-cond % a "?"))
      (eof? ch) (eof-error!)
      :else
      (let [splicing (identical? ch "@")]
        (when-not splicing (unread rdr))
        (if (skip rdr whitespace?)
          (io/bound-read [*reading-cond* true] (read-cond-body rdr a splicing))
          (on-ready rdr #(io/bound-read [*reading-cond* true] (read-cond-body % a splicing))))))))

(defn- fold-unquote [a sym]
  (.push a (list sym (.pop a)))
  true)

(defn read-unquote [^not-native rdr a _]
  (let [ch (read-char rdr)]
    (cond
      (eof? ch) (eof-error!)
      (nil? ch) (on-ready rdr #(read-unquote % a nil))
      :else
      (let [sym (if (identical? "@" ch)
                  'clojure.core/unquote-splicing
                  (do (unread rdr) 'clojure.core/unquote))]
        (if (read1 rdr a)
          (fold-unquote a sym)
          (on-ready rdr #(do (check %) (fold-unquote a sym))))))))

(declare syntax-quote)

(defn- listy? [x] (or (list? x) (seq? x)))

(defn unquote-splicing? [x]
  (and (listy? x) (= 'clojure.core/unquote-splicing (first x))))

(defn unquote? [x]
  (and (listy? x) (= 'clojure.core/unquote (first x))))

(defn- unquotable? [x]
  (or (keyword? x) (number? x) (string? x) (nil? x) (boolean? x)
    (and (listy? x) (= 'quote (first x)))))

(defn- unquote [x]
  (if (and (listy? x) (= 'quote (first x)))
    (second x)
    x))

(defn- syntax-quote-expand [f fsym xs]
  (let [chunks #js []
        chunk #js []
        spliced (reduce 
                  (fn [spliced x]
                    (if (unquote-splicing? x)
                      (do
                        (when-not (zero? (.-length chunk))
                          (.push chunks (vec chunk))
                          (set! (.-length chunk) 0))
                        (.push chunks (second x))
                        true)
                      (do (.push chunk (syntax-quote x)) spliced)))
                  false xs)]
    (when (pos? (.-length chunk))
      (.push chunks (vec chunk)))
    (case (.-length chunks)
      0 (f nil)
      1 (cond
          spliced
         `(apply ~fsym ~(aget chunks 0))
         (every? unquotable? (aget chunks 0))
         (list 'quote (f (map unquote (aget chunks 0))))
         :else
         `(~fsym ~@(aget chunks 0)))
      `(apply ~fsym (concat ~@chunks)))))

(defn- syntax-quote-symbol [sym]
  (list 'quote
    (cond
     (and (nil? (namespace sym)) (.endsWith (name sym) "#")) (*autogensym* sym)
     (.endsWith (name sym) ".")
     (let [sym (resolve-symbol (symbol (namespace sym) (subs (name sym) 0 (dec (count (name sym))))))]
       (symbol (namespace sym) (str (name sym) ".")))
     (and (nil? (namespace sym)) (.startsWith (name sym) ".")) sym
     :else (resolve-symbol sym))))

(defn- syntax-quote [x]
  (cond
    (special-symbol? x) (list 'quote x)
    (symbol? x) (syntax-quote-symbol x)
    (or (keyword? x) (number? x) (string? x) (nil? x) (boolean? x)) x
    (unquote? x) (second x)
    (unquote-splicing? x) (reader-error! "splice not in list")
    (record? x) x
    (map? x) (syntax-quote-expand map* `hash-map (mapcat seq x))
    (vector? x) (syntax-quote-expand vec `vector x)
    (set? x) (syntax-quote-expand set `hash-set x)
    (listy? x) (syntax-quote-expand sequence `list x)
    (coll? x) (reader-error! "Unknown Collection type")
    :else  (list 'quote x)))

(defn- autogensym [sym]
  (symbol (str (name (gensym (str (subs (name sym) 0 (dec (count (name sym))))
                               "__"))) "__auto__")))

(defn- fold-syntax-quote [a]
  (.push a (binding [*autogensym* (memoize autogensym)]
             (syntax-quote (.pop a))))
  true)

(defn read-syntax-quote [^not-native rdr a _]
  (if (read1 rdr a)
    (fold-syntax-quote a)
    (on-ready rdr #(do (check %) (fold-syntax-quote a)))))

(defn read-dispatch [^not-native rdr a _]
  (let [ch (read-char rdr)]
    (cond
      (eof? ch) (eof-error!)
      (nil? ch) (on-ready rdr #(read-dispatch % a nil))
      :else
      (let [f (or (aget *dispatch-macros* (.charCodeAt ch 0)) *default-dispatch-macro*)]
        (f rdr a ch)))))

(def ^:dynamic *macros* #js [])

(def ^:dynamic *default-macro* #(reader-error! "No macro for #" %3))

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

(def ^:dynamic *arg-env* nil)

(defn- gen-arg [n]
  (symbol (str (gensym (str (if (neg? n) "rest" (str "p" n)) "__")) "#")))

(defn- register-arg! [a n]
  (.push a
    (or (get *arg-env* n)
      (let [s (gen-arg n)]
        (set! *arg-env* (assoc *arg-env* n s))
        s)))
  true)

(defn- register-some! [rdr a n]
  (let [t (when (< n (.-length a)) (.pop a))]
    (cond
      (= '& t) (register-arg! a -1)
      (number? t) (register-arg! a (int t))
      :else (reader-error! "arg literal must be %, %& or %number"))))

(defn read-arg [rdr a ch]
  (if *arg-env*
    (let [ch (read-char rdr)]
      (cond
        (nil? ch) (-> rdr unread (on-ready #(read-arg % a "%")))
        (eof? ch) (register-arg! a 1)
        :else
        (let [n (.-length a)]
          (unread rdr)
          (cond
            (or (whitespace? ch) (terminating-macros ch)) (register-arg! a 1)
            (read-some rdr a) (register-some! rdr a n)
            :else (on-ready rdr #(do (check %) (register-some! % a n)))))))
    (read-symbol rdr a ch)))

(defn- anon-fn [forms]
  (let [N (or (first (keys (rsubseq *arg-env* > 0))) 0)
        args (into [] (map #(or (*arg-env* %) (gen-arg %))) (range 1 (inc N)))
        args (if-some [r (*arg-env* -1)]
               (conj args '& r)
               args)]
    (set! *arg-env* nil)
    (list 'fn* args (list* forms))))

(defn read-fn [rdr a ch]
  (when *arg-env*
    (reader-error! "Nested #()s are not allowed."))
  (set! *arg-env* (sorted-map))
  (read-delimited rdr a ")" anon-fn #js []))

(defn read-unreadable [rdr a ch]
  (reader-error! "Unreadable form."))

(defn- ns-map* [ns kvs]
  (loop [i 0]
    (when (< i (.-length kvs))
      (let [k (aget kvs i)]
        (aset kvs i
          (cond
            (symbol? k)
            (case (namespace k)
              nil (symbol ns (name k))
              "_" (symbol nil (name k))
              k)
            (keyword? k)
            (case (namespace k)
              nil (keyword ns (name k))
              "_" (keyword nil (name k))
              k)
            :else k))
        (recur (+ 2 i)))))
  (map* kvs))

(defn- read-namespaced-map-map [^not-native rdr a auto]
  (let [ch (read-char rdr)]
    (cond
      (eof? ch) (eof-error!)
      (nil? ch) (on-ready rdr #(read-namespaced-map-map % a auto))
      (whitespace? ch) (if (skip rdr whitespace?)
                         (recur rdr a auto)
                         (on-ready rdr #(read-namespaced-map-map % a auto)))
      (not (identical? "{" ch)) (reader-error! "Namespaced map must specify a map")
      :else 
      (let [tag (.pop a)]
        (when-not (simple-symbol? tag)
          (reader-error! "Namespaced map must specify a valid namespace: " tag))
        (if-some [ns (some-> (if auto (resolve-ns tag) tag) name)]
          (read-delimited rdr a "}" #(ns-map* ns %) #js [])
          (reader-error! "Unknown auto-resolved namespace alias: " tag))))))

(defn- read-namespaced-map-ns [^not-native rdr a auto]
  (let [ch (read-char rdr)]
    (cond
      (eof? ch) (eof-error!)
      (nil? ch) (on-ready rdr #(read-namespaced-map-ns % a auto))
      (or (whitespace? ch) (identical? "{" ch))
      ; TODO; read1 and read2 are more liberal than LispReader because they allow any kinf of form evaluating to a map
      ; use read-delimited instead
      (if auto
        (read-namespaced-map-map (doto rdr unread) (doto a (.push (ns-name *ns*))) false)
        (reader-error! "Namespaced map must specify a namespace"))
      (read1 (doto rdr unread) a) (read-namespaced-map-map rdr a auto)
      :else
      (on-ready rdr #(read-namespaced-map-map % a auto)))))

(defn read-namespaced-map [^not-native rdr a ch]
  (let [ch (read-char rdr)]
    (cond
      (eof? ch) (eof-error!)
      (nil? ch) (on-ready rdr #(read-namespaced-map % a ":"))
      :else
      (let [auto (identical? ":" ch)]
        (when-not auto (unread rdr))
        (read-namespaced-map-ns rdr a auto)))))

;; root readers
(defn macros-table [m]
  (let [a (object-array 128)]
    (doseq [[ch f] m]
      (aset a (.charCodeAt ch 0) f))
    a))

(def cljs-dispatch-macros
  {"^" (with-line+col read-meta 2)
   "'" (with-line+col (read-wrap 'var) 2)
   "\"" read-regex
   "(" read-fn
   "{" (with-line+col read-set 2)
   "=" (fn [_ _ _] (reader-error! "read-eval is not supported.")) ; read-eval
   "!" read-comment
   "<" read-unreadable
   "_" read-discard
   "?" read-cond
   ":" read-namespaced-map})

(def cljs-macros
  (->
    {"\"" read-stringlit 
     ";" read-comment
     "'" (with-line+col (read-wrap 'quote))
     "@" (with-line+col (read-wrap 'deref))
     "^" (with-line+col read-meta)
     "`" read-syntax-quote
     "~" read-unquote
     "(" (with-line+col read-list)
     ")" read-unbalanced
     "[" (with-line+col read-vector)
     "]" read-unbalanced
     "{" (with-line+col read-map)
     "}" read-unbalanced
     "\\" read-charlit
     "%" read-arg
     "#" read-dispatch
     ":" read-keyword
     "+" read-sym-or-num
     "-" read-sym-or-num}
    (into (map vector "0123456789" (repeat read-number)))
    (into (map vector "\t\n\r ," (repeat read-space)))))

(def cljs-terminating-macros
  (reduce dissoc cljs-macros ":+-0123456789#'%"))

(defn cljs-resolve [type x]
  (case type
    :symbol (ana/resolve-symbol x)
    :ns (let [alias-or-nsname (if (string? x) (symbol x) x)]
          (if (ana/get-namespace alias-or-nsname)
            alias-or-nsname
            (ana/resolve-ns-alias (assoc @env/*compiler* :ns (ana/get-namespace ana/*cljs-ns*)) alias-or-nsname nil)))))

(def ^:private cljs-read-opts
  {:eof :eofthrow
   :read-cond :allow
   :features #{:cljs}
   ::resolve cljs-resolve
   ::macros (macros-table cljs-macros)
   ::default-macro (with-line+col read-symbol)
   ::terminating-macros (macros-table cljs-terminating-macros)
   ::dispatch-macros (macros-table cljs-dispatch-macros)
   ::default-dispatch-macro read-ctor
   ::read-eval false})

(defn readtop [^not-native rdr a eof-value cb]
  (case (.-length a)
    0 (let [ch (read-char rdr)]
        (cond
          (eof? ch) (do (.push a eof-value) (recur rdr a eof-value cb))
          (nil? ch) (on-ready rdr #(readtop % a eof-value cb))
          :else (if-some [r (macros ch)]
                  (if (r rdr a ch)
                    (recur rdr a eof-value cb)
                    (on-ready rdr #(readtop % a eof-value cb)))
                  (reader-error! "Unexpected character: " (pr-str ch)))))
    1 (do (try (cb (.pop a)) (catch :default e)) true)
    (reader-error! "Reader bug: read too many forms.")))

(defn read
  "Like usual read but takes an additional last argument: a callback.
   The callback takes two arguments value and error. It will be called when a value is read or
   an error thrown."
  ([cb] (read {} io/*in* cb))
  ([rdr cb]
    (read {} rdr cb))
  ([opts rdr cb]
    (let [{:keys [eof read-cond features
                  ::macros ::default-macro ::terminating-macros
                  ::dispatch-macros ::default-dispatch-macro
                  ::resolve ::read-eval]}
          (into cljs-read-opts opts)
          eof (if (= eof :eofthrow) #js {} eof)]
      (dyn/binding [*read-cond-mode* read-cond
                    *read-cond-features* features
                    *macros* macros
                    *default-macro* default-macro
                    *terminating-macros* terminating-macros
                    *dispatch-macros* dispatch-macros
                    *default-dispatch-macro* default-dispatch-macro
                    *error-data* #(io/-info rdr)
                    *arg-env* nil
                    *resolve* resolve
                    *read-eval* read-eval]
        (readtop rdr #js [] eof (fn [v]
                                  (if (identical? v eof)
                                    (eof-error!)
                                    (cb v nil)))))))
  ([rdr cb eof-error? eof-value]
    (read {:eof (if eof-error? :eofthrow eof-value)} rdr cb)))

(defn read-string
  ([s] (read-string {} s))
  ([opts s]
    (let [in (-> s io/string-reader io/line-col-reader)
          ret #js [nil nil]]
      (read opts in (fn [v ex] (doto ret (aset 0 v) (aset 1 ex))))
      (if-some [ex (aget ret 1)]
        (throw ex)
        (aget ret 0)))))

;; EDN

(def edn-dispatch-macros
  {"{" (with-line+col read-set 2)
   "_" read-discard
   ":" read-namespaced-map}) ; not really part of edn, left in the name of Postel's loww

(defn- unsupported [msg]
  (fn [_ _ _]
    (reader-error! msg)))

(def edn-macros
  (->
    {"\"" read-stringlit 
     ";" read-comment
     "'" (unsupported "No quote in EDN")
     "@" (unsupported "No deref in EDN")
     "^" (unsupported "No meta in EDN")
     "`" (unsupported "No syntax quote in EDN")
     "~" (unsupported "No unquote in EDN")
     "(" (with-line+col read-list)
     ")" read-unbalanced
     "[" (with-line+col read-vector)
     "]" read-unbalanced
     "{" (with-line+col read-map)
     "}" read-unbalanced
     "\\" read-charlit
     "#" read-dispatch
     ":" read-keyword
     "+" read-sym-or-num
     "-" read-sym-or-num}
    (into (map vector "0123456789" (repeat read-number)))
    (into (map vector "\t\n\r ," (repeat read-space)))))

(def edn-terminating-macros
  (reduce dissoc cljs-macros ":+-0123456789#'%"))

(def ^:private edn-read-opts
  {:eof :eofthrow
   :read-cond nil ; the dispatch macro isn't ther anyway
   ::resolve (fn [_ _] (reader-error! "Aliases are not allowed in EDN."))
   ::macros (macros-table edn-macros)
   ::default-macro (with-line+col read-symbol)
   ::terminating-macros (macros-table edn-terminating-macros)
   ::dispatch-macros (macros-table edn-dispatch-macros)
   ::default-dispatch-macro read-ctor
   ::read-eval false})

(defn edn-read
  "Like usual read but takes an additional last argument: a callback.
   The callback takes two arguments value and error. It will be called when a value is read or
   an error thrown."
  ([cb] (edn-read {} io/*in* cb))
  ([rdr cb]
    (edn-read {} rdr cb))
  ([opts rdr cb]
    (read (into edn-read-opts opts) rdr cb))
  ([rdr cb eof-error? eof-value]
    (edn-read {:eof (if eof-error? :eofthrow eof-value)} rdr cb)))

(defn edn-read-string
  ([s] (edn-read-string {} s))
  ([opts s]
    (let [in (-> s io/string-reader io/line-col-reader)
          ret #js [nil nil]]
      (edn-read opts in (fn [v ex] (doto ret (aset 0 v) (aset 1 ex))))
      (if-some [ex (aget ret 1)]
        (throw ex)
        (aget ret 0)))))
