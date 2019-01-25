# malhala

Run [Mal](https://github.com/kanaka/mal) on [WASM](https://webassembly.org/) directly in your [CLJS](https://clojurescript.org/) repl.

mal is a Clojure inspired Lisp interpreter, implemented in 74 languages.

mal is a learning tool. Similar to [Linux From Scratch](http://www.linuxfromscratch.org/), mal is a set of documentation that guides you, step by step, through implementing a basic lisp in the target language of your choosing.

The creator of mal, Joel Martin (kanaka), recently completed his WASM implementation of mal.

This project is an effort to port mal/wasm to clojurescript for execution in the browser.

This project leverages [tau.alpha](https://github.com/johnmn3/tau.alpha) for blocking reads in order to enable REPL interaction.

mal is licensed under MPL 2.0, except in files where otherwise noted.

```
git clone https://github.com/johnmn3/malhala.git
cd malhala
clj -m cljs.main -co build.edn -ro '{:launch-browser false}' -c malhala.core -r
```

Then launch your favorite browser. (only tested in Chrome so far... Safari has issues with [tau.alpha](https://github.com/johnmn3/tau.alpha) at the moment) Open the browser's developer console and, within a few seconds, you should see `:mal-compiled` printed.

Then start playing at the repl:
```
cljs.user=> (require '[malhala.core :refer [mal>]])
nil
cljs.user=> (mal> (+ 1 2))
nil
```
In the developer console, we can see the printed results: `Error: unexpected EOF`

Well, that wasn't supposed to happen. I'm still working out the kinks (PRs welcome).

Let's try that last one again:

```
(mal> (+ 1 2))
```

Browser console: `3`

Nice! Now go and port CLJS to WASM!!!
