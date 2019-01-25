(ns malhala.mal)

;; Copyright Joel Martin <github@martintribe.org>
;; Licensed under MPL-2.0 (see ./LICENSE)
;; https://github.com/kanaka/wam

(def _fn?
  '(def! _fn?
     (fn* [x]
       (if (fn? x)
         (if (get (meta x) "ismacro")
           false
           true)
         false))))

(def macro?
  '(def! macro?
     (fn* [x]
       (if (fn? x)
         (if (get (meta x) "ismacro")
           true
           false)
         false))))

(def core_ns
  '(def! core_ns
    [["=" =]
     ["throw" throw]
     ["nil?" nil?]
     ["true?" true?]
     ["false?" false?]
     ["number?" number?]
     ["string?" string?]
     ["symbol" symbol]
     ["symbol?" symbol?]
     ["keyword" keyword]
     ["keyword?" keyword?]
     ["fn?" _fn?]
     ["macro?" macro?]

     ["pr-str" pr-str]
     ["str" str]
     ["prn" prn]
     ["println" println]
     ["readline" readline]
     ["read-string" read-string]
     ["slurp" slurp]
     ["<" <]
     ["<=" <=]
     [">" >]
     [">=" >=]
     ["+" +]
     ["-" -]
     ["*" *]
     ["/" /]
     ["time-ms" time-ms]

     ["list" list]
     ["list?" list?]
     ["vector" vector]
     ["vector?" vector?]
     ["hash-map" hash-map]
     ["map?" map?]
     ["assoc" assoc]
     ["dissoc" dissoc]
     ["get" get]
     ["contains?" contains?]
     ["keys" keys]
     ["vals" vals]

     ["sequential?" sequential?]
     ["cons" cons]
     ["concat" concat]
     ["nth" nth]
     ["first" first]
     ["rest" rest]
     ["empty?" empty?]
     ["count" count]
     ["apply" apply]
     ["map" map]

     ["conj" conj]
     ["seq" seq]

     ["with-meta" with-meta]
     ["meta" meta]
     ["atom" atom]
     ["atom?" atom?]
     ["deref" deref]
     ["reset!" reset!]
     ["swap!" swap!]
     ["clojure.core/deref" deref]]))

(def core
  (str
    _fn?
    macro?
    core_ns
    '(prn "loaded core.mal")))

(def bind-env
  '(def! bind-env
     (fn* [env b e]
       (if (empty? b)
         env

         (if (= "&" (str (first b)))
           (assoc env (str (nth b 1)) e)

           (bind-env (assoc env (str (first b)) (first e))
                     (rest b) (rest e)))))))

(def new-env
  '(def! new-env
     (fn* [& args]
       (if (<= (count args) 1)
         (atom {"--outer--" (first args)})
         (atom (bind-env {"--outer--" (first args)}
                         (nth args 1) (nth args 2)))))))

(def env-find
  "(def! env-find
     (fn* [env k]
       (let* [ks (str k)
              data @env]
         (if (contains? data ks)
           env
           (if (get data \"--outer--\")
             (env-find (get data \"--outer--\") ks)
             nil)))))")

(def env-get
  "(def! env-get
     (fn* [env k]
       (let* [ks (str k)
              e (env-find env ks)]
         (if e
           (get @e ks)
           (throw (str \"'\" ks \"' not found\"))))))")

(def env-set
  '(def! env-set
     (fn* [env k v]
       (do
         (swap! env assoc (str k) v)
         v))))

; (def env
;   (str
;     bind-env
;     new-env
;     env-find
;     env-get
;     env-set
;     core
;     '(prn "loaded env.mal")))

(def main
  '(do
     ;(prn "loading mal")
     ; (load-file "./env.mal")
     ; (load-file "./core.mal")

     ;; read
     (def! READ
       (fn* [strng]
         (read-string strng)))


     ;; eval
     (def! is-pair
       (fn* [x]
         (if (sequential? x)
           (if (> (count x) 0)
             true))))

     ;(prn "defining quasiquote")

     (def! QUASIQUOTE
       (fn* [an-ast]
         (cond
           (not (is-pair an-ast))
           (list 'quote an-ast)

           (= 'unquote (first an-ast))
           (nth an-ast 1)

           (if (is-pair (first an-ast))
             (if (= 'splice-unquote (first (first an-ast)))
               true))
           (list 'concat (nth (first an-ast) 1) (QUASIQUOTE (rest an-ast)))

           "else"
           (list 'cons (QUASIQUOTE (first an-ast)) (QUASIQUOTE (rest an-ast))))))

     ;(prn "done defining quasiquote")

     (def! is-macro-call
       (fn* [ast env]
         (if (list? ast)
           (let* [a0 (first ast)]
             (if (symbol? a0)
               (if (env-find env a0)
                 (let* [m (meta (env-get env a0))]
                   (if m
                     (if (get m "ismacro")
                       true)))))))))

     (def! MACROEXPAND
       (fn* [ast env]
         (if (is-macro-call ast env)
           (let* [mac (env-get env (first ast))]
             (MACROEXPAND (apply mac (rest ast)) env))
           ast)))

     (def! eval-ast
       (fn* [ast env]
         (do
           ;;(do (prn "eval-ast" ast "/" (keys env)) )
           (cond
             (symbol? ast) (env-get env ast)

             (list? ast)   (map (fn* [exp] (EVAL exp env)) ast)

             (vector? ast) (apply vector (map (fn* [exp] (EVAL exp env)) ast))

             (map? ast)    (apply hash-map
                               (apply concat
                                 (map (fn* [k] [k (EVAL (get ast k) env)])
                                      (keys ast))))

             "else"        ast))))

     (def! LET
       (fn* [env args]
         (if (> (count args) 0)
           (do
             (env-set env (nth args 0) (EVAL (nth args 1) env))
             (LET env (rest (rest args)))))))

     (def! EVAL
       (fn* [ast env]
         (do (prn "EVAL" ast)
           (if (not (list? ast))
             (eval-ast ast env)

             ;; apply list
             (let* [ast (MACROEXPAND ast env)]
               (if (not (list? ast))
                 (eval-ast ast env)
                 (do (println "running cond")
                   (let* [a0 (first ast)]
                     (cond
                       (nil? a0)
                       (do (println "got: nil?")
                         ast)

                       (= 'def! a0)
                       (do (println "got: def!")
                         (env-set env (nth ast 1) (EVAL (nth ast 2) env)))

                       (= 'let* a0)
                       (do (println "got: let*"))
                       (let* [let-env (new-env env)]
                         (do
                           (LET let-env (nth ast 1))
                           (EVAL (nth ast 2) let-env)))

                       (= 'quote a0)
                       (do (println "got: quote")
                         (nth ast 1))

                       (= 'quasiquote a0)
                       (do (println "got: quasiquote")
                         (let* [a1 (nth ast 1)]
                           (EVAL (QUASIQUOTE a1) env)))

                       (= 'defmacro! a0)
                       (do (println "got: defmacro!")
                         (let* [a1 (nth ast 1)
                                a2 (nth ast 2)
                                f (EVAL a2 env)
                                m (or (meta f) {})
                                mac (with-meta f (assoc m "ismacro" true))]
                           (env-set env a1 mac)))

                       (= 'macroexpand a0)
                       (do (println "got: macroexpand")
                         (let* [a1 (nth ast 1)]
                           (MACROEXPAND a1 env)))

                       (= 'try* a0)
                       (do (println "got: try*")
                         (if (= 'catch* (nth (nth ast 2) 0))
                           (try*
                             (EVAL (nth ast 1) env)
                             (catch* exc
                               (EVAL (nth (nth ast 2) 2)
                                     (new-env env
                                              [(nth (nth ast 2)1)]
                                              [exc]))))
                           (EVAL (nth ast 1) env)))

                       (= 'do a0)
                       (do (println "got: do")
                         (let* [el (eval-ast (rest ast) env)]
                           (nth el (- (count el) 1))))

                       (= 'if a0)
                       (do (println "got: if")
                         (let* [cond (EVAL (nth ast 1) env)]
                           (if (or (= cond nil) (= cond false))
                             (if (> (count ast) 3)
                               (EVAL (nth ast 3) env)
                               nil)
                             (EVAL (nth ast 2) env))))

                       (= 'fn* a0)
                       (do (println "got: fn*")
                         (fn* [& args]
                           (EVAL (nth ast 2) (new-env env (nth ast 1) args))))

                       "else"
                       (do (println "EVAL took \"else\" branch")
                         (let* [el (eval-ast ast env)
                                f (first el)
                                args (rest el)]
                           (apply f args))))))))))))


     ;; print
     (def! PRINT (fn* [exp] (pr-str exp)))

     ;; repl
     (def! repl-env (new-env))
     (def! rep
       (fn* [strng]
         (PRINT (EVAL (READ strng) repl-env))))

     ;; core.mal: defined directly using mal
     (map (fn* [data] (env-set repl-env (nth data 0) (nth data 1))) core_ns)
     (env-set repl-env 'eval (fn* [ast] (EVAL ast repl-env)))
     (env-set repl-env '*ARGV* (rest *ARGV*))

     ;; core.mal: defined using the new language itself
     (rep "(defmacro! cond (fn* [& xs] (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
     (rep (str "(def! *host-language* \"" *host-language* "-mal\")"))
     (rep "(def! not (fn* (a) (if a false true)))")
     (rep "(def! load-file (fn* [f] (eval (read-string (str \"(do \" (slurp f) \")\")))))")
     (rep "(def! *gensym-counter* (atom 0))")
     (rep "(def! gensym (fn* [] (symbol (str \"G__\" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))")
     (rep "(defmacro! or (fn* [& xs] (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs)) (if ~condvar ~condvar (or ~@(rest xs)))))))))")

     ;; repl loop
     (def! repl-loop
       ;(println "running repl-loop")
       (fn* []
         (do (println "inside repl-loop")
           (let* [line (readline "mal-user> ")]
             (if line
               (do
                 (if (not (= "" line))
                   (try*
                     (println (rep line))
                     (catch* exc
                       (println "Uncaught exception:" exc))))
                 (repl-loop)))))))

     (def! -main
       ;(println "running -main")
       (fn* [& args]
         (do (println "in -main fn body")
           (if (> (count args) 0)
             (do (println :got-args)
               (prn args)
               (rep (str "(load-file \"" (first args) "\")")))
             (do
               (rep "(println (str \"Mal [\" *host-language* \"]\"))")
               (repl-loop))))))

     (-main)))

     ; (apply -main *ARGV*)))

(def mal
  (str
    '(def! clojure.core/deref (fn* [x] (deref x)))
    bind-env
    new-env
    env-find
    env-get
    env-set
    core
    ; cond
    main
    '(prn "mal loaded")))
