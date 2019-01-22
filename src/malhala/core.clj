(ns malhala.core)

(defmacro mal> [aform]
  (let [astr (pr-str aform)]
    `(do (malhala.core/write-line ~astr))))
