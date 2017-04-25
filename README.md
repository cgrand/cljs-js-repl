# cljs-js-repl

An effort to make self-hosted cljs repls upgradeable.

To achieve this goal some subgoals must be met:

 * input stream abstraction (done)
 * async reader (done)
 * dynamic binding conveyance (done)
 * expose `eval` (somehow done)

Hopefully some of this work will find its way into lumo/plank or clojurescript proper. 

## Usage

(with `lumo`)

```clj
cljs.user=> (require '[net.cgrand.lumo.socket-repl :as r])
nil
cljs.user=> (r/start-server 5678)
#object[Server [object Object]]
```

Then from another terminal:
```
~$ nc localhost 5678
cljs.user=>(require '[net.cgrand.cljs.js.repl.main :as m])
nil
cljs.user=>(m/repl :eval r/eval
                   :prompt #(print"nested=>")
                   :read (fn [rp re cb]
                           (m/repl-read rp re 
                              (fn [form e]
                                (cb ({'exit re} form form) e)))))
nested=>nested=>true
12 ;input
12
nested=>(+ 2 3)
5
nested=>exit
cljs.user=>
```

## License

Copyright © 2017 Christophe Grand

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
