# malhala

Run [Mal](https://github.com/kanaka/mal) on [WASM](https://webassembly.org/) directly in your [CLJS](https://clojurescript.org/) repl.

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
