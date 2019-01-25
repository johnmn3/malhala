(ns malhala.core
  (:require [cljs.repl :refer [repl-read]]))

(defmacro mal> [aform]
  (let [astr (pr-str aform)]
    `(do (malhala.core/write-line ~astr))))

;; still broken, but let's try to get this working...
(defmacro mal-repl []
  `(do
     ~(print "mal=> ")
     ~(flush)
     (loop []
       (when-let [x# ~(str (repl-read "mal> " ":mal/quit"))]
         (when (not= x# ":mal/quit")
           (println "got input!" x#)
           (malhala.core/write-line x#)
           (when (tau.alpha.core/on-screen?)
             (recur)))))))
