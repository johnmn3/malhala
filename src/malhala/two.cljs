(ns malhala.two)

; (def mal-env
;   "(module))")



(def mal-env
  "(module $env

  (func $ENV_NEW (param $outer i32) (result i32)
    (LET $data ($HASHMAP) ;; allocate the data hashmap
         $env  ($ALLOC (global.get $ENVIRONMENT_T) $data $outer 0))
    ;; environment takes ownership
    ($RELEASE $data)
    $env
  )

  (func $ENV_NEW_BINDS (param $outer i32 $binds i32 $exprs i32) (result i32)
    (LET $env ($ENV_NEW $outer)
         $key 0)

    ;; process bindings
    (block $done
      (loop $loop
        (br_if $done (i32.eqz ($VAL0 $binds)))

        ;; get/deref the key from binds
        (local.set $key ($MEM_VAL1_ptr $binds))
        (if (i32.eqz ($strcmp \"&\" ($to_String $key)))
          (then
            ;; ENV_NEW_BIND_VARGS
            ;; get/deref the key from the next element of binds
            (local.set $binds ($MEM_VAL0_ptr $binds))
            (local.set $key ($MEM_VAL1_ptr $binds))
            ;; the value is the remaining list in exprs
            (local.set $exprs ($FORCE_SEQ_TYPE (global.get $LIST_T) $exprs))
            ;; set the binding in the environment data
            (drop ($ENV_SET $env $key $exprs))
            ;; list is owned by the environment
            ($RELEASE $exprs)
            (br $done))
          (else
            ;; ENV_NEW_BIND_1x1
            ;; set the binding in the environment data
            (drop ($ENV_SET $env $key ($MEM_VAL1_ptr $exprs)))
            ;; go to next element of binds and exprs
            (local.set $binds ($MEM_VAL0_ptr $binds))
            (local.set $exprs ($MEM_VAL0_ptr $exprs))))

        (br $loop)
      )
    )
    $env
  )

  (func $ENV_SET (param $env i32 $key i32 $value i32) (result i32)
    (LET $data ($MEM_VAL0_ptr $env))
    (i32.store ($VAL0_ptr $env) ($IDX ($ASSOC1 $data $key $value)))
    $value
  )

  (func $ENV_SET_S (param $env i32 $key i32 $value i32) (result i32)
    (LET $data ($MEM_VAL0_ptr $env))
    (i32.store ($VAL0_ptr $env) ($IDX ($ASSOC1_S $data $key $value)))
    $value
  )

  (func $ENV_FIND (param $env i32 $key i32) (result i64)
    (local $found_res i64)
    (LET $res 0
         $data 0)

    (block $done
      (loop $loop
        (local.set $data ($MEM_VAL0_ptr $env))
        (local.set $found_res ($HASHMAP_GET $data $key))
        ;;; if (found)
        (if (i32.wrap_i64 (i64.shr_u $found_res (i64.const 32)))
          (then
            (local.set $res (i32.wrap_i64 $found_res))
            (br $done)))
        (local.set $env ($MEM_VAL1_ptr $env))
        (if (i32.eq $env (global.get $NIL))
          (then
            (local.set $env 0)
            (br $done)))
        (br $loop)
      )
    )

    ;; combine res/env as hi 32/low 32 of i64
    (i64.or
      (i64.shl (i64.extend_i32_u $res) (i64.const 32))
      (i64.extend_i32_u $env))
  )

  (func $ENV_GET (param $env i32 $key i32) (result i32)
    (local $res_env i64)
    (LET $res 0)

    (local.set $res_env ($ENV_FIND $env $key))
    (local.set $env (i32.wrap_i64 $res_env))
    (local.set $res (i32.wrap_i64 (i64.shr_u $res_env (i64.const 32))))

    (if (i32.eqz $env)
      (then
        ($THROW_STR_1 \"'%s' not found\" ($to_String $key))
        (return $res)))
    (return ($INC_REF $res))
  )
)")
