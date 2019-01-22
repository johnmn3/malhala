(ns malhala.core
  (:require
   [malhala.zero :as zero]
   [malhala.one :as one]
   [malhala.two :as two]
   [malhala.three :as three]
   [malhala.step :as step]
   [mal-wam :as m]
   [web-assembly.Wabt :as wabt]
   [tau.alpha.core :refer [set-conf! on-screen? tauon tau]]
   [tau.alpha.tau :refer [wait]])
  (:require-macros
   [malhala.core :refer [mal>]]
   [tau.alpha.macros :refer [on]]))

(set-conf! {:main "malhala.core"
            :log? true})


(def wabt-module (js/WabtModule))

(def read-wasm (.-readWasm wabt-module))

(def pw (.-parseWat wabt-module))

(defn parse-wat [wat]
  (pw "wat>" wat))

(defn wam->wast [strs]
  (let [asts (map m/read-str strs)
        emc (m/empty-ctx)
        r-asts (map #(m/wam-eval % emc) asts)
        wast (m/emit-module (clj->js r-asts) emc #js {:memorySize 256})]
    wast))

(def flags
  #js
  {:exceptions false
   :mutable_globals true
   :sat_float_to_int false
   :sign_extension false
   :simd false
   :threads false
   :multi_value false
   :tail_call false})

(defn resolve-names [x]
  (.resolveNames x)
  x)

(defn wam->wasm [& wams]
  (-> (apply wam->wast wams)
    parse-wat
    resolve-names
    (.toBinary #js {:write_debug_names true})
    .-buffer))

(def files (atom {}))

(defn load-files []
  (swap! files assoc
    "platform_os.wam" zero/platform-os
    "string.wam" zero/string
    "printf.wam" zero/printf
    "types.wam" one/types
    "mem.wam" one/mem
    "debug.wam" one/debug
    "reader.wam" one/reader
    "printer.wam" one/printer
    "env.wam" two/mal-env
    "core.wam" three/core
    "repl0.wam" step/repl0
    "repl1.wam" step/repl1
    "setpA_mal.wam" step/stepA-mal))

;(load-files)
(defn read-file-native [s]
  (get @files s))

(defn ->u-int-8-array [buf]
  (let [u (js/Uint8Array. (.-length buf))]
    (doall
      (map-indexed
       #(aset u %1 %2)
       (array-seq buf)))
    u))

(defn get-string [mem addr]
  (let [u8 (js/Uint8Array. (.-buffer mem) addr)
        len (.findIndex u8 #(= 0 %))
        bytes (js/Uint8Array. (.-buffer mem) addr len)
        s (-> (js/TextDecoder. "utf8") (.decode bytes))]
    s))

(defn put-string [mem addr s max-length]
  (let [u8 (js/Uint8Array. (.-buffer mem) addr)
        bytes (-> (js/TextEncoder. "utf8") (.encode s))
        bytes (if (and max-length (> (.-length bytes) max-length))
                (.slice bytes 0 max-length)
                bytes)]
    (.set u8 bytes 0)
    (aset u8 (.-length bytes) 0)
    (inc (.-length bytes))))

(defn marshal-argv [mem offset args]
  (let [view (js/DataView. (.-buffer mem) offset)
        buf8 (js/Uint8Array. (.-buffer mem) offset)
        string-start (atom (* 4 (inc (count args))))]
    (doall
     (map-indexed
      (fn [i arg]
        (let [len (put-string mem @string-start arg nil)]
          (.setUint32 view (* i 4) @string-start true)
          (swap! string-start (partial + len))))
      args))
    (+ @string-start offset)))

(defn printline [mem addr stream]
  (println (.replace (get-string mem addr) #"\n$" "")))

(def tau-container (atom (atom {:waiting false})))

(def t (tauon))

(defn read-line []
  (let [atau @tau-container
        _ (swap! atau assoc :waiting true)
        res (-> atau wait :res)]
    (swap! atau assoc :res nil :waiting false)
    (or res 0)))

;; Should return addr on success and -1 on failure
;; Truncate to max_length
(defn readline [mem prompt addr max-length]
  (when-let [line (read-line #_ (get-string mem prompt))]
    (put-string mem addr (or line 0) max-length)))

;;; file reading not yet tested
(def fs {})

(defn read-file-sync [an-fs path encoding]
  (get an-fs path))

(defn read-file [mem path-addr buf]
  (let [path (get-string mem path-addr)
        contents (read-file-sync fs path "utf8")]
    (put-string mem buf contents nil)))

(defn get-time-ms []
  (- (.getTime (js/Date.)) 0x38640900))

(defn run []
  (let [[argc argv instance] (-> @files :main)
        _ (-> instance .-exports (.__post_instantiate argc argv))
        res (-> instance .-exports (._main argc argv))]
    (println :res res)
    res))

(defn load-wam [filenames args]
  (let [atau (tau {:waiting false :res nil})
        _ (swap! tau-container (constantly atau))
        _ (on "screen" [atau] (swap! tau-container (constantly atau)))
        the-files (mapv read-file-native filenames)
        wbin (wam->wasm the-files)
        memory (js/WebAssembly.Memory. #js {:initial 256})
        memory-start 0
        memory-base (marshal-argv memory memory-start args)
        memory-base (+ memory-base (- 8 (mod memory-base 8)))
        memory-table (js/WebAssembly.Table. #js {:initial 0 :element "anyfunc"})
        import-obj #js {:env
                        #js {:exit #(println :exit)
                             :printline (partial printline memory)
                             :readline (partial readline memory)
                             :read_file (partial read-file memory)
                             :get_time_ms get-time-ms
                             :stdout 0
                             :fputs (partial printline memory)
                             :memory memory
                             :memoryBase memory-base
                             :table memory-table
                             :tableBase 0}}]
    (swap! files assoc :import-obj import-obj)
    (.then (js/WebAssembly.instantiateStreaming
            (js/Response. wbin #js {:status 200
                                    :headers
                                    #js {:content-type "application/wasm"}})
            import-obj)
      #(do
        (->> % .-instance
          (conj [(count args) 0])
          (swap! files assoc :main))
        (run)))))

(defn compile []
  (load-files)
  (load-wam ["platform_os.wam"
             "string.wam"
             "printf.wam"
             "types.wam"
             "mem.wam"
             "debug.wam"
             "reader.wam"
             "printer.wam"
             "env.wam"
             "core.wam"
             "setpA_mal.wam"]
            [])
  (println :mal-compiled!))

(when (on-screen?)
  (on t (compile)))

(defn write-line [aline]
  (if (-> @@tau-container :waiting)
    (do (swap! @tau-container assoc :res aline) nil)
    (println :not-listening)))
