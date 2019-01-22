(ns malhala.step)

(def repl0
  "(module $step0_repl

  ;; READ
  (func $READ (param $str i32) (result i32)
    $str
  )

  (func $EVAL (param $ast i32) (param $env i32) (result i32)
    $ast
  )

  ;; PRINT
  (func $PRINT (param $ast i32) (result i32)
    $ast
  )

  ;; REPL
  (func $rep (param $line i32) (result i32)
    ($PRINT ($EVAL ($READ $line) 0))
  )

  (func $main (result i32)
    ;; Constant location/value definitions
    (LET $line (STATIC_ARRAY 201))

    ;; DEBUG
    ;;($printf_1 \"memoryBase: 0x%x\\n\" (global.get $memoryBase))

    ;; Start REPL
    (block $repl_done
      (loop $repl_loop
        (br_if $repl_done (i32.eqz ($readline \"user> \" $line)))
        (br_if $repl_loop (i32.eq (i32.load8_u $line) 0))
        ($printf_1 \"%s\\n\" ($rep $line))
        (br $repl_loop)
      )
    )

    ($print \"\\n\")
    0
  )


  (export \"_main\" (func $main))
  (export \"__post_instantiate\" (func $init_printf_mem))
)")

(def repl1
  "(module $step1_read_print

  ;; READ
  (func $READ (param $str i32) (result i32)
    ($read_str $str)
  )

  ;; EVAL
  (func $EVAL (param $ast i32 $env i32) (result i32)
    $ast
  )

  ;; PRINT
  (func $PRINT (param $ast i32) (result i32)
    ($pr_str $ast 1)
  )

  ;; REPL
  (func $REP (param $line i32 $env i32) (result i32)
    (LET $mv1 0 $mv2 0 $ms 0)
    (block $done
      (local.set $mv1 ($READ $line))
      (br_if $done (global.get $error_type))

      (local.set $mv2 ($EVAL $mv1 $env))
      (br_if $done (global.get $error_type))

;;      ($PR_MEMORY -1 -1)
      (local.set $ms ($PRINT $mv2))
    )

    ;; release memory from MAL_READ
    ($RELEASE $mv1)
    $ms
  )

  (func $main (result i32)
    (LET $line (STATIC_ARRAY 201)
         $res  0)

    ;; DEBUG
;;    ($printf_1 \"memoryBase: 0x%x\\n\" (global.get $memoryBase))
;;    ($printf_1 \"heap_start: 0x%x\\n\" (global.get $heap_start))
;;    ($printf_1 \"heap_end: 0x%x\\n\" (global.get $heap_end))
;;    ($printf_1 \"mem: 0x%x\\n\" (global.get $mem))
;;    ($printf_1 \"string_mem: %d\\n\" (global.get $string_mem))

;;    ($PR_MEMORY_RAW
;;      (global.get $mem) (i32.add (global.get $mem)
;;                                 (i32.mul (global.get $mem_unused_start) 4)))

    (drop ($STRING (global.get $STRING_T) \"uvw\"))
    (drop ($STRING (global.get $STRING_T) \"xyz\"))

    ;;($PR_MEMORY -1 -1)

    ;; Start REPL
    (block $repl_done
      (loop $repl_loop
        (br_if $repl_done (i32.eqz ($readline \"user> \" $line)))
        (br_if $repl_loop (i32.eq (i32.load8_u $line) 0))
        (local.set $res ($REP $line 0))
        (if (global.get $error_type)
          (then
            ($printf_1 \"Error: %s\\n\" (global.get $error_str))
            (global.set $error_type 0))
          (else
            ($printf_1 \"%s\\n\" ($to_String $res))))
        ($RELEASE $res)
        ;;($PR_MEMORY_SUMMARY_SMALL)
        (br $repl_loop)
      )
    )

    ($print \"\\n\")
    ;;($PR_MEMORY -1 -1)
    0
  )


  (export \"_main\" (func $main))
  (export \"__post_instantiate\" (func $init_memory))
)
")

(def stepA-mal
  "(module $stepA_mal

  (global $repl_env (mut i32) (i32.const 0))

  ;; READ
  (func $READ (param $str i32) (result i32)
    ($read_str $str)
  )

  ;; EVAL
  (func $is_pair (param $ast i32) (result i32)
    (LET $type ($TYPE $ast))
    (AND (OR (i32.eq $type (global.get $LIST_T))
             (i32.eq $type (global.get $VECTOR_T)))
         (i32.ne ($VAL0 $ast) 0))
  )

  (func $QUASIQUOTE (param $ast i32) (result i32)
    (LET $res 0 $sym 0 $second 0 $third 0)
    (if (i32.eqz ($is_pair $ast)) ;; QQ_QUOTE
      (then
        (local.set $sym ($STRING (global.get $SYMBOL_T) \"quote\"))
        ;; ['quote ast]
        (local.set $res ($LIST2 $sym $ast))
        ($RELEASE $sym))
      (else
        (local.set $res ($MEM_VAL1_ptr $ast))
        (if (AND (i32.eq ($TYPE $res) (global.get $SYMBOL_T))
                 (i32.eqz ($strcmp \"unquote\" ($to_String $res))))
          (then
            ;; ast[1]
            (local.set $res ($INC_REF ($MEM_VAL1_ptr ($MEM_VAL0_ptr $ast)))))
        (else (if (AND ($is_pair $res)
                       (i32.eq ($TYPE ($MEM_VAL1_ptr $res))
                               (global.get $SYMBOL_T))
                       (i32.eqz ($strcmp \"splice-unquote\"
                                         ($to_String ($MEM_VAL1_ptr $res)))))
          (then
            ;; ['concat, ast[0][1], quasiquote(ast[1..])]
            (local.set $sym ($STRING (global.get $SYMBOL_T) \"concat\"))
            (local.set $second
                       ($MEM_VAL1_ptr ($MEM_VAL0_ptr ($MEM_VAL1_ptr $ast))))
            (local.set $third ($QUASIQUOTE ($MEM_VAL0_ptr $ast)))
            (local.set $res ($LIST3 $sym $second $third))
            ;; release inner quasiquoted since outer list take ownership
            ($RELEASE $third)
            ($RELEASE $sym))
          (else
            ;; ['cons, quasiquote(ast[0]), quasiquote(ast[1..])]
            (local.set $sym ($STRING (global.get $SYMBOL_T) \"cons\"))
            (local.set $second ($QUASIQUOTE ($MEM_VAL1_ptr $ast)))
            (local.set $third ($QUASIQUOTE ($MEM_VAL0_ptr $ast)))
            (local.set $res ($LIST3 $sym $second $third))
            ;; release inner quasiquoted since outer list takes ownership
            ($RELEASE $third)
            ($RELEASE $second)
            ($RELEASE $sym)))))))
    $res
  )

  (global $mac_stack (mut i32) (i32.const 0))
  (global $mac_stack_top (mut i32) (i32.const -1))

  (func $MACROEXPAND (param $orig_ast i32 $env i32) (result i32)
    (local $mac_env i64)
    (LET $ast $orig_ast
         $mac 0)
    (global.set $mac_stack (STATIC_ARRAY 1024)) ;; 256 * 4, TODO: move to init
    (block $done
      (loop $loop
        (br_if $done
               (OR (i32.ne ($TYPE $ast) (global.get $LIST_T)) ;; a list
                   (i32.eqz ($VAL0 $ast))                     ;; non-empty
                   (i32.ne ($TYPE ($MEM_VAL1_ptr $ast))       ;; leading symbol
                           (global.get $SYMBOL_T))))
	(local.set $mac_env ($ENV_FIND $env ($MEM_VAL1_ptr $ast)))
	(local.set $mac (i32.wrap_i64 (i64.shr_u $mac_env (i64.const 32))))
        (br_if $done (OR (i32.eqz (i32.wrap_i64 $mac_env))    ;; defined in env
                         (i32.ne ($TYPE $mac)                 ;; a macro
                                 (global.get $MACRO_T))))

        (local.set $ast ($APPLY $mac ($MEM_VAL0_ptr $ast)))
        ;; PEND_A_LV
        ;; if ast is not the first ast that was passed in, then add it
        ;; to the pending release list.
        (if (i32.ne $ast $orig_ast)
          (then
            (global.set $mac_stack_top
                        (i32.add (global.get $mac_stack_top) 1))
            (if (i32.ge_s (i32.mul (global.get $mac_stack_top) 4) 1024) ;; 256 * 4
              ($fatal 7 \"Exhausted mac_stack!\\n\"))
            (i32.store (i32.add
                         (global.get $mac_stack)
                         (i32.mul (global.get $mac_stack_top) 4))
              $ast)))
        (br_if $done (global.get $error_type))

        (br $loop)
      )
    )
    $ast
  )

  (func $EVAL_AST (param $ast i32 $env i32 $skiplast i32) (result i32)
    (LET $res 0 $val2 0 $val3 0 $type 0 $found 0
         $ret 0 $empty 0 $current 0)

    (if (global.get $error_type) (return 0))
    (local.set $type ($TYPE $ast))

    ;;($PR_VALUE \">>> EVAL_AST ast: '%s'\\n\" $ast)

    ;;; switch(type)
    (block $done
      (block $default (block (block
      (br_table 2 2 2 2 2 0 1 1 1 2 2 2 2 2 2 2 $type))
      ;; symbol
      ;; found/res returned as hi 32/lo 32 of i64
      (local.set $res ($ENV_GET $env $ast))
      (br $done))
      ;; list, vector, hashmap
      ;; MAP_LOOP_START
      (local.set $res ($MAP_LOOP_START $type))
      ;; push MAP_LOOP stack
      ;;; empty = current = ret = res
      (local.set $ret $res)
      (local.set $current $res)
      (local.set $empty $res)

      (block $done
        (loop $loop
          ;; check if we are done evaluating the source sequence
          (br_if $done (i32.eq ($VAL0 $ast) 0))

          (if $skiplast
            (br_if $done (i32.eqz ($VAL0 ($MEM_VAL0_ptr $ast)))))

          (if (i32.eq $type (global.get $HASHMAP_T))
            (then
              (local.set $res ($EVAL ($MEM_VAL2_ptr $ast) $env)))
            (else
              (local.set $res ($EVAL ($MEM_VAL1_ptr $ast) $env))))
          (local.set $val2 $res)

          ;; if error, release the unattached element
          (if (global.get $error_type)
            (then
              ($RELEASE $res)
              (local.set $res 0)
              (br $done)))

          ;; for hash-maps, copy the key (inc ref since we are going
          ;; to release it below)
          (if (i32.eq $type (global.get $HASHMAP_T))
            (then
              (local.set $val3 $val2)
              (local.set $val2 ($MEM_VAL1_ptr $ast))
              (drop ($INC_REF $val2))))

          ;; MAP_LOOP_UPDATE
          (local.set $res ($MAP_LOOP_UPDATE $type $empty $current $val2 $val3))
          (if (i32.le_u $current (global.get $EMPTY_HASHMAP))
            ;; if first element, set return to new element
            (local.set $ret $res))
          ;; update current to point to new element
          (local.set $current $res)

          (local.set $ast ($MEM_VAL0_ptr $ast))

          (br $loop)
        )
      )
      ;; MAP_LOOP_DONE
      (local.set $res $ret)
      ;; EVAL_AST_RETURN: nothing to do
      (br $done))
      ;; default
      (local.set $res ($INC_REF $ast))
    )

    $res
  )

  (func $MAL_GET_A1 (param $ast i32) (result i32)
    ($MEM_VAL1_ptr ($MEM_VAL0_ptr $ast)))
  (func $MAL_GET_A2 (param $ast i32) (result i32)
    ($MEM_VAL1_ptr ($MEM_VAL0_ptr ($MEM_VAL0_ptr $ast))))
  (func $MAL_GET_A3 (param $ast i32) (result i32)
    ($MEM_VAL1_ptr ($MEM_VAL0_ptr ($MEM_VAL0_ptr ($MEM_VAL0_ptr $ast)))))

  (func $EVAL (param $orig_ast i32 $orig_env i32) (result i32)
    (LET $ast $orig_ast
         $env $orig_env
         $orig_mac_stack_top (global.get $mac_stack_top)
         $prev_ast 0 $prev_env 0 $res 0 $el 0
         $ftype 0 $f_args 0 $f 0 $args 0
         $a0 0 $a0sym 0 $a1 0 $a2 0
         $err 0)

    (block $EVAL_return
    (loop $TCO_loop

    (local.set $f_args 0)
    (local.set $f 0)
    (local.set $args 0)

    (if (global.get $error_type)
      (then
        (local.set $res 0)
        (br $EVAL_return)))

    ;;($PR_VALUE \">>> EVAL ast: '%s'\\n\" $ast)

    (if (i32.ne ($TYPE $ast) (global.get $LIST_T))
      (then
        (local.set $res ($EVAL_AST $ast $env 0))
        (br $EVAL_return)))

    ;; APPLY_LIST
    (local.set $ast ($MACROEXPAND $ast $env))
    ;;($PR_VALUE \">>> >>> EVAL ast: '%s'\\n\" $ast)

    (if (i32.ne ($TYPE $ast) (global.get $LIST_T))
      (then
        (local.set $res ($EVAL_AST $ast $env 0))
        (br $EVAL_return)))

    (if ($EMPTY_Q $ast)
      (then
        (local.set $res ($INC_REF $ast))
        (br $EVAL_return)))

    (local.set $a0 ($MEM_VAL1_ptr $ast))
    (local.set $a0sym \"\")
    (if (i32.eq ($TYPE $a0) (global.get $SYMBOL_T))
      (local.set $a0sym ($to_String $a0)))

    (if (i32.eqz ($strcmp \"def!\" $a0sym))
      (then
        (local.set $a1 ($MAL_GET_A1 $ast))
        (local.set $a2 ($MAL_GET_A2 $ast))
        (local.set $res ($EVAL $a2 $env))
        (br_if $EVAL_return (global.get $error_type))

        ;; set a1 in env to a2
        (local.set $res ($ENV_SET $env $a1 $res))
        (br $EVAL_return))
    (else (if (i32.eqz ($strcmp \"let*\" $a0sym))
      (then
        (local.set $a1 ($MAL_GET_A1 $ast))
        (local.set $a2 ($MAL_GET_A2 $ast))

        ;; create new environment with outer as current environment
        (local.set $prev_env $env) ;; save env for later release
        (local.set $env ($ENV_NEW $env))

        (block $done
          (loop $loop
            (br_if $done (i32.eqz ($VAL0 $a1)))
            ;; eval current A1 odd element
            (local.set $res ($EVAL ($MEM_VAL1_ptr ($MEM_VAL0_ptr $a1)) $env))

            (br_if $done (global.get $error_type))

            ;; set key/value in the let environment
            (local.set $res ($ENV_SET $env ($MEM_VAL1_ptr $a1) $res))
            ;; release our use, ENV_SET took ownership
            ($RELEASE $res)

            ;; skip to the next pair of a1 elements
            (local.set $a1 ($MEM_VAL0_ptr ($MEM_VAL0_ptr $a1)))
            (br $loop)
          )
        )

        ;; release previous environment if not the current EVAL env
        (if (i32.ne $prev_env $orig_env)
          (then
            ($RELEASE $prev_env)
            (local.set $prev_env 0)))

        (local.set $ast $a2)
        (br $TCO_loop))
    (else (if (i32.eqz ($strcmp \"do\" $a0sym))
      (then
        ;; EVAL the rest through second to last
        (local.set $el ($EVAL_AST ($MEM_VAL0_ptr $ast) $env 1))
        (local.set $ast ($LAST $ast))
        ($RELEASE $ast) ;; we already own it via ast
        ($RELEASE $el)
        (br $TCO_loop))
    (else (if (i32.eqz ($strcmp \"quote\" $a0sym))
      (then
        (local.set $res ($INC_REF ($MEM_VAL1_ptr ($MEM_VAL0_ptr $ast))))
        (br $EVAL_return))
    (else (if (i32.eqz ($strcmp \"quasiquote\" $a0sym))
      (then
        (local.set $ast ($QUASIQUOTE ($MEM_VAL1_ptr ($MEM_VAL0_ptr $ast))))

        ;; if we have already been here via TCO, release previous ast
        (if $prev_ast ($RELEASE $prev_ast))
        (local.set $prev_ast $ast)
        (br $TCO_loop))
    (else (if (i32.eqz ($strcmp \"defmacro!\" $a0sym))
      (then
        (local.set $a1 ($MAL_GET_A1 $ast))
        (local.set $a2 ($MAL_GET_A2 $ast))
        (local.set $res ($EVAL $a2 $env))
        ($SET_TYPE $res (global.get $MACRO_T))
        (br_if $EVAL_return (global.get $error_type))

        ;; set a1 in env to a2
        (local.set $res ($ENV_SET $env $a1 $res))
        (br $EVAL_return))
    (else (if (i32.eqz ($strcmp \"macroexpand\" $a0sym))
      (then
        ;; since we are returning it unevaluated, inc the ref cnt
        (local.set $res ($INC_REF ($MACROEXPAND
                                    ($MEM_VAL1_ptr ($MEM_VAL0_ptr $ast))
                                    $env))))
    (else (if (i32.eqz ($strcmp \"try*\" $a0sym))
      (then
        (local.set $a1 ($MAL_GET_A1 $ast))
        (local.set $res ($EVAL $a1 $env))

        ;; if there is no error, return
        (br_if $EVAL_return (i32.eqz (global.get $error_type)))
        ;; if there is an error and res is set, we need to free it
        ($RELEASE $res)
        ;; if there is no catch block then return
        (br_if $EVAL_return
               (i32.eqz ($VAL0 ($MEM_VAL0_ptr ($MEM_VAL0_ptr $ast)))))

        ;; save the current environment for release
        (local.set $prev_env $env)
        ;; create environment for the catch block eval
        (local.set $env ($ENV_NEW $env))

        ;; set a1 and a2 from the catch block
        (local.set $a1 ($MAL_GET_A1 ($MAL_GET_A2 $ast)))
        (local.set $a2 ($MAL_GET_A2 ($MAL_GET_A2 $ast)))

        ;; create object for string errors
        (if (i32.eq (global.get $error_type) 1)
          (then
            (local.set $err ($STRING (global.get $STRING_T)
                                     (global.get $error_str))))
          (else
            (local.set $err (global.get $error_val))))
        ;; bind the catch symbol to the error object
        (drop ($ENV_SET $env $a1 $err))
        ;; release our use, env took ownership
        ($RELEASE $err)

        ;; unset error for catch eval
        (global.set $error_type 0)
        (i32.store (global.get $error_str) (CHR \"\\x00\"))

        ;; release previous environment if not the current EVAL env
        (if (i32.ne $prev_env $orig_env)
          (then
            ($RELEASE $prev_env)
            (local.set $prev_env 0)))

        (local.set $ast $a2)
        (br $TCO_loop))
    (else (if (i32.eqz ($strcmp \"if\" $a0sym))
      (then
        (local.set $a1 ($MAL_GET_A1 $ast))
        (local.set $res ($EVAL $a1 $env))

        (if (global.get $error_type)
          (then (nop))
        (else (if (OR (i32.eq $res (global.get $NIL))
                      (i32.eq $res (global.get $FALSE)))
          (then
            ($RELEASE $res)
            ;; if no false case (A3), return nil
            (if (i32.lt_u ($COUNT $ast) 4)
              (then
                (local.set $res ($INC_REF (global.get $NIL)))
                (br $EVAL_return))
              (else
                (local.set $ast ($MAL_GET_A3 $ast)))))
        (else
          ($RELEASE $res)
          (local.set $ast ($MAL_GET_A2 $ast))))))
        (br $TCO_loop))
    (else (if (i32.eqz ($strcmp \"fn*\" $a0sym))
      (then
        (local.set $a1 ($MAL_GET_A1 $ast))
        (local.set $a2 ($MAL_GET_A2 $ast))
        (local.set $res ($MALFUNC $a2 $a1 $env))
        (br $EVAL_return))
    (else
      ;; EVAL_INVOKE
      (local.set $res ($EVAL_AST $ast $env 0))
      (local.set $f_args $res)

      ;; if error, return f/args for release by caller
      (if (global.get $error_type)
       (then
         (local.set $res $f_args)
         (br $EVAL_return)))

      (local.set $args ($MEM_VAL0_ptr $f_args)) ;; rest
      (local.set $f ($DEREF_META ($MEM_VAL1_ptr $f_args))) ;; value

      (local.set $ftype ($TYPE $f))
      (if (i32.eq $ftype (global.get $FUNCTION_T))
        (then
          (if (i32.eq ($VAL0 $f) 0) ;; eval
            (then
              (local.set $res ($EVAL ($MEM_VAL1_ptr $args)
                                     (global.get $repl_env))))
            (else
              (local.set $res (call_indirect (type $fnT) $args ($VAL0 $f)))))
          ;; release f/args
          ($RELEASE $f_args)
          (br $EVAL_return))
      (else (if (i32.eq $ftype (global.get $MALFUNC_T))
        (then
          ;; save the current environment for release
          (local.set $prev_env $env)
          ;; create new environment using env and params stored in function
          (local.set $env ($ENV_NEW_BINDS ($MEM_VAL2_ptr $f)
                                          ($MEM_VAL1_ptr $f) $args))

          ;; release previous environment if not the current EVAL env
          ;; because our new env refers to it and we no longer need to
          ;; track it (since we are TCO recurring)
          (if (i32.ne $prev_env $orig_env)
            (then
              ($RELEASE $prev_env)
              (local.set $prev_env 0)))

          ;; claim the AST before releasing the list containing it
          (local.set $ast ($MEM_VAL0_ptr $f))
          (drop ($INC_REF $ast))

          ;; if we have already been here via TCO, release previous
          ;; ast
          ;; PEND_A_LV
          (if $prev_ast ($RELEASE $prev_ast))
          (local.set $prev_ast $ast)

          ;; release f/args
          ($RELEASE $f_args)

          (br $TCO_loop))
      (else
        ($THROW_STR_1 \"apply of non-function type: %d\\n\" $ftype)
        (local.set $res 0)
        ($RELEASE $f_args)
        (br $EVAL_return)))))))))))))))))))))))))

    ) ;; end of TCO_loop
    ) ;; end of EVAL_return

    ;; EVAL_RETURN
    (if (i32.ne $env $orig_env) ($RELEASE $env))
    (if $prev_ast ($RELEASE $prev_ast))

    ;; release memory from MACROEXPAND
    ;; TODO: needs to happen here so self-hosting doesn't leak
    (block $done
      (loop $loop
        (br_if $done (i32.le_s (global.get $mac_stack_top) $orig_mac_stack_top))
        ($RELEASE (i32.load (i32.add
                              (global.get $mac_stack)
                              (i32.mul (global.get $mac_stack_top) 4))))
        (global.set $mac_stack_top
                    (i32.sub (global.get $mac_stack_top) 1))
        (br $loop)
      )
    )

    $res
  )

  ;; PRINT
  (func $PRINT (param $ast i32) (result i32)
    ($pr_str $ast 1)
  )

  ;; REPL
  (func $RE (param $line i32 $env i32) (result i32)
    (LET $mv1 0 $res 0)
    (block $done
      (local.set $mv1 ($READ $line))
      (br_if $done (global.get $error_type))

      (local.set $res ($EVAL $mv1 $env))
    )

    ;; release memory from MAL_READ
    ($RELEASE $mv1)
    $res
  )

  (func $REP (param $line i32 $env i32) (result i32)
    (LET $mv2 0 $ms 0)
    (block $done
      (local.set $mv2 ($RE $line $env))
      (br_if $done (global.get $error_type))

;;      ($PR_MEMORY -1 -1)
      (local.set $ms ($PRINT $mv2))
    )

    ;; release memory from RE
    ($RELEASE $mv2)
    $ms
  )

  (func $main (param $argc i32 $argv i32) (result i32)
    (LET $line (STATIC_ARRAY 201)
         $res 0 $repl_env 0 $ms 0
         ;; argument processing
         $i 0 $ret 0 $empty 0 $current 0 $val2 0)

    ;; DEBUG
;;    ($printf_1 \"argc: 0x%x\\n\" $argc)
;;    ($printf_1 \"memoryBase: 0x%x\\n\" (global.get $memoryBase))
;;    ($printf_1 \"heap_start: 0x%x\\n\" (global.get $heap_start))
;;    ($printf_1 \"heap_end: 0x%x\\n\" (global.get $heap_end))
;;    ($printf_1 \"mem: 0x%x\\n\" (global.get $mem))
;;    ($printf_1 \"string_mem: %d\\n\" (global.get $string_mem))

    (global.set $repl_env ($ENV_NEW (global.get $NIL)))
    (local.set $repl_env (global.get $repl_env))

    ;; core.EXT: defined in wasm
    ($add_core_ns $repl_env)
    (drop ($ENV_SET_S $repl_env \"eval\" ($FUNCTION 0)))

    ($checkpoint_user_memory)

    ;; core.mal: defined using the language itself
    ($RELEASE ($RE \"(def! *host-language* \\\"WebAssembly\\\")\" $repl_env))
    ($RELEASE ($RE \"(def! not (fn* (a) (if a false true)))\" $repl_env))
    ($RELEASE ($RE \"(def! load-file (fn* (f) (eval (read-string (str \\\"(do \\\" (slurp f) \\\")\\\")\" $repl_env))
    ($RELEASE ($RE \"(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \\\"odd number of forms to cond\\\") (cons 'cond (rest (rest xs)))\" $repl_env))
    ($RELEASE ($RE \"(def! *gensym-counter* (atom 0))\" $repl_env))
    ($RELEASE ($RE \"(def! gensym (fn* [] (symbol (str \\\"G__\\\" (swap! *gensym-counter* (fn* [x] (+ 1 x))))\" $repl_env))
    ($RELEASE ($RE \"(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (c (gensym)) `(let* (~c ~(first xs)) (if ~c ~c (or ~@(rest xs)))))))))\" $repl_env))

    ;; Command line arguments
    (local.set $res ($MAP_LOOP_START (global.get $LIST_T)))
    ;; push MAP_LOP stack
    ;; empty = current = ret = res
    (local.set $ret $res)
    (local.set $current $res)
    (local.set $empty $res)

    (local.set $i 2)
    (block $done
      (loop $loop
        (br_if $done (i32.ge_u $i $argc))

	(local.set $val2 ($STRING (global.get $STRING_T)
                                  (i32.load (i32.add $argv (i32.mul $i 4)))))

        ;; MAP_LOOP_UPDATE
        (local.set $res ($MAP_LOOP_UPDATE
                          (global.get $LIST_T) $empty $current $val2 0))
        (if (i32.le_u $current (global.get $EMPTY_HASHMAP))
          ;; if first element, set return to new element
          (local.set $ret $res))
        ;; update current to point to new element
        (local.set $current $res)

        (local.set $i (i32.add $i 1))
        (br $loop)
      )
    )
    (drop ($ENV_SET_S $repl_env \"*ARGV*\" $ret))


    ;;($PR_MEMORY -1 -1)

    (if (i32.gt_u $argc 1)
      (then
        (drop ($ENV_SET_S $repl_env
                          \"*FILE*\" ($STRING (global.get $STRING_T)
                                            (i32.load (i32.add $argv 4)))))
        ($RELEASE ($RE \"(load-file *FILE*)\" $repl_env))
        (if (global.get $error_type)
          (then
            ($printf_1 \"Error: %s\\n\" (global.get $error_str))
            (return 1))
          (else
            (return 0)))))

    ($RELEASE ($RE \"(println (str \\\"Mal [\\\" *host-language* \\\"\\\")\" $repl_env))

    ;; Start REPL
    (block $repl_done
      (loop $repl_loop
        (br_if $repl_done (i32.eqz ($readline \"user> \" $line)))
        (br_if $repl_loop (i32.eq (i32.load8_u $line) 0))
        (local.set $res ($REP $line $repl_env))
        (if (global.get $error_type)
          (then
            (if (i32.eq 2 (global.get $error_type))
              (then
                (local.set $ms ($pr_str (global.get $error_val) 1))
                ($printf_1 \"Error: %s\\n\" ($to_String $ms))
                ($RELEASE $ms)
                ($RELEASE (global.get $error_val)))
              (else
                ($printf_1 \"Error: %s\\n\" (global.get $error_str))))
            (global.set $error_type 0))
          (else
            ($printf_1 \"%s\\n\" ($to_String $res))))
        ($RELEASE $res)
        ;;($PR_MEMORY_SUMMARY_SMALL)
        (br $repl_loop)
      )
    )

    ($print \"\\n\")
    ;;($PR_MEMORY -1 -1)
    0
  )


  (export \"_main\" (func $main))
  (export \"__post_instantiate\" (func $init_memory))
)
")
