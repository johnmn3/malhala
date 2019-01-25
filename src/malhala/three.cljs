(ns malhala.three)

(def core
  ";; Copyright Joel Martin <github@martintribe.org>
;; Licensed under MPL-2.0 (see ./LICENSE)
;; https://github.com/kanaka/wam

(module $core


  ;; it would be nice to have this in types.wam but it uses
  ;; ENV_NEW_BINDS which is not available until step3 but types is
  ;; used in step1

  (func $APPLY (param $f i32) (param $args i32) (result i32)
    (local $res i32 $env i32 $ftype i32 $a i32)
    (local.set $f ($DEREF_META $f))
    (local.set $ftype ($TYPE $f))
    (if (i32.eq $ftype (global.get $FUNCTION_T))
      (then
        ;; Must be kept in sync with EVAL's FUNCTION_T evaluation
        (if (i32.eq ($VAL0 $f) 0) ;; eval
          (then
            (local.set $res ($EVAL ($MEM_VAL1_ptr $args)
                                   (global.get $repl_env))))
          (else
            (local.set $res (call_indirect (type $fnT) $args ($VAL0 $f))))))
    (else (if (OR (i32.eq $ftype (global.get $MALFUNC_T))
                  (i32.eq $ftype (global.get $MACRO_T)))
      (then
        ;; create new environment using env and params stored in function
        (local.set $env ($ENV_NEW_BINDS ($MEM_VAL2_ptr $f)
                                        ($MEM_VAL1_ptr $f) $args))

        ;; claim the AST before releasing the list containing it
        (local.set $a ($MEM_VAL0_ptr $f))
        (drop ($INC_REF $a))

        (local.set $res ($EVAL $a $env))

        ($RELEASE $env)
        ($RELEASE $a))
    (else
      ($THROW_STR_1 \"APPLY of non-function type: %d\\n\" $ftype)
      (local.set $res 0)))))
    $res
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; core functions

  (type $fnT (func (param i32) (result i32)))

  (func $equal_Q (param $args i32) (result i32)
    ($TRUE_FALSE ($EQUAL_Q ($MEM_VAL1_ptr $args)
                           ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args)))))

  (func $throw (param $args i32) (result i32)
    (global.set $error_type 2)
    (global.set $error_val ($INC_REF ($MEM_VAL1_ptr $args)))
    0
  )

  (func $nil_Q (param $args i32) (result i32)
    ($TRUE_FALSE (i32.eq ($TYPE ($MEM_VAL1_ptr $args))
                         (global.get $NIL_T))))
  (func $true_Q (param $args i32) (result i32)
    (LET $ast ($MEM_VAL1_ptr $args))
    ($TRUE_FALSE (AND (i32.eq ($TYPE $ast) (global.get $BOOLEAN_T))
                      (i32.eq ($VAL0 $ast) 1)))
  )
  (func $false_Q (param $args i32) (result i32)
    (LET $ast ($MEM_VAL1_ptr $args))
    ($TRUE_FALSE (AND (i32.eq ($TYPE $ast) (global.get $BOOLEAN_T))
                      (i32.eq ($VAL0 $ast) 0)))
  )
  (func $number_Q (param $args i32) (result i32)
    ($TRUE_FALSE (i32.eq ($TYPE ($MEM_VAL1_ptr $args))
                         (global.get $INTEGER_T))))
  (func $string_Q (param $args i32) (result i32)
    (LET $mv ($MEM_VAL1_ptr $args))
    ($TRUE_FALSE (AND (i32.eq ($TYPE $mv) (global.get $STRING_T))
                      (i32.ne (i32.load8_u ($to_String $mv))
                              (CHR \"\\x7f\"))))
  )

  (func $keyword (param $args i32) (result i32)
    (LET $str ($to_String ($MEM_VAL1_ptr $args)))
    (if (result i32) (i32.eq (i32.load8_u $str) (CHR \"\\x7f\"))
      (then ($INC_REF ($MEM_VAL1_ptr $args)))
      (else
        (drop ($sprintf_1 (global.get $printf_buf) \"\\x7f%s\" $str))
        ($STRING (global.get $STRING_T) (global.get $printf_buf))))
  )

  (func $keyword_Q (param $args i32) (result i32)
    (LET $ast ($MEM_VAL1_ptr $args))
    ($TRUE_FALSE (AND (i32.eq ($TYPE $ast) (global.get $STRING_T))
                      (i32.eq (i32.load8_u ($to_String $ast))
                              (CHR \"\\x7f\"))))
  )
  (func $fn_Q (param $args i32) (result i32)
    (LET $type ($TYPE ($MEM_VAL1_ptr $args)))
    ($TRUE_FALSE (OR (i32.eq $type (global.get $FUNCTION_T))
                     (i32.eq $type (global.get $MALFUNC_T)))))
  (func $macro_Q (param $args i32) (result i32)
    ($TRUE_FALSE (i32.eq ($TYPE ($MEM_VAL1_ptr $args))
                         (global.get $MACRO_T))))

  (func $symbol (param $args i32) (result i32)
    ($STRING (global.get $SYMBOL_T) ($to_String ($MEM_VAL1_ptr $args))))

  (func $symbol_Q (param $args i32) (result i32)
    ($TRUE_FALSE (i32.eq ($TYPE ($MEM_VAL1_ptr $args))
                         (global.get $SYMBOL_T))))

  (func $core_pr_str (param $args i32) (result i32)
    ($pr_str_seq $args 1 \" \"))
  (func $str (param $args i32) (result i32)
    ($pr_str_seq $args 0 \"\"))
  (func $prn (param $args i32) (result i32)
    (LET $res ($pr_str_seq $args 1 \" \"))
    ($printf_1 \"%s\\n\" ($to_String $res))
    ($RELEASE $res)
    ($INC_REF (global.get $NIL))
  )
  (func $println (param $args i32) (result i32)
    (LET $res ($pr_str_seq $args 0 \" \"))
    ($printf_1 \"%s\\n\" ($to_String $res))
    ($RELEASE $res)
    ($INC_REF (global.get $NIL))
  )

  (func $core_readline (param $args i32) (result i32)
    (LET $line (STATIC_ARRAY 201)
         $mv 0)
    (if (i32.eqz ($readline ($to_String ($MEM_VAL1_ptr $args)) $line))
      (return ($INC_REF (global.get $NIL))))
    (local.set $mv ($STRING (global.get $STRING_T) $line))
    $mv
  )

  (func $read_string (param $args i32) (result i32)
    ($read_str ($to_String ($MEM_VAL1_ptr $args))))

  (func $slurp (param $args i32) (result i32)
    (LET $mv   ($STRING_INIT (global.get $STRING_T))
         $size ($read_file ($to_String ($MEM_VAL1_ptr $args))
                           ($to_String $mv)))
    (if (i32.eqz $size)
      (then
        ($THROW_STR_1 \"failed to read file '%s'\" ($to_String ($MEM_VAL1_ptr $args)))
        (return ($INC_REF (global.get $NIL)))))
    (local.set $mv ($STRING_FINALIZE $mv $size))
    $mv
  )

  (func $lt (param $args i32) (result i32)
    ($TRUE_FALSE
      (i32.lt_s ($VAL0 ($MEM_VAL1_ptr $args))
                ($VAL0 ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args))))))
  (func $lte (param $args i32) (result i32)
    ($TRUE_FALSE
      (i32.le_s ($VAL0 ($MEM_VAL1_ptr $args))
                ($VAL0 ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args))))))
  (func $gt (param $args i32) (result i32)
    ($TRUE_FALSE
      (i32.gt_s ($VAL0 ($MEM_VAL1_ptr $args))
                ($VAL0 ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args))))))
  (func $gte (param $args i32) (result i32)
    ($TRUE_FALSE
      (i32.ge_s ($VAL0 ($MEM_VAL1_ptr $args))
                ($VAL0 ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args))))))
  (func $add (param $args i32) (result i32)
    ($INTEGER
      (i32.add ($VAL0 ($MEM_VAL1_ptr $args))
               ($VAL0 ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args))))))
  (func $subtract (param $args i32) (result i32)
    ($INTEGER
      (i32.sub ($VAL0 ($MEM_VAL1_ptr $args))
                 ($VAL0 ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args))))))
  (func $multiply (param $args i32) (result i32)
    ($INTEGER
      (i32.mul ($VAL0 ($MEM_VAL1_ptr $args))
                 ($VAL0 ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args))))))
  (func $divide (param $args i32) (result i32)
    ($INTEGER
      (i32.div_s ($VAL0 ($MEM_VAL1_ptr $args))
                 ($VAL0 ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args))))))

  (func $time_ms (param $args i32) (result i32)
    ($INTEGER ($get_time_ms)))

  ;;;

  (func $list (param $args i32) (result i32)
    ($INC_REF $args))

  (func $list_Q (param $args i32) (result i32)
    ($TRUE_FALSE (i32.eq ($TYPE ($DEREF_META ($MEM_VAL1_ptr $args)))
                         (global.get $LIST_T))))

  (func $vector (param $args i32) (result i32)
    ($FORCE_SEQ_TYPE (global.get $VECTOR_T) $args))

  (func $vector_Q (param $args i32) (result i32)
    ($TRUE_FALSE (i32.eq ($TYPE ($DEREF_META ($MEM_VAL1_ptr $args)))
                         (global.get $VECTOR_T))))

  (func $hash_map (param $args i32) (result i32)
    (LET $type    (global.get $HASHMAP_T)
         $res     ($MAP_LOOP_START $type)
         $val2    0
         $val3    0
         $c       0
         ;; push MAP_LOOP stack
         $ret     $res
         $current $res
         $empty   $res)

    ;; READ_SEQ_LOOP
    (block $done
      (loop $loop
        (br_if $done (i32.eqz ($VAL0 $args)))

        (local.set $val2 ($INC_REF ($MEM_VAL1_ptr $args)))
        (local.set $val3 ($INC_REF ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args))))

        ;; skip two
        (local.set $args ($MEM_VAL0_ptr ($MEM_VAL0_ptr $args)))

        ;; update the return sequence structure
        ;; MAP_LOOP_UPDATE
        (local.set $res ($MAP_LOOP_UPDATE $type $empty $current $val2 $val3))
        (if (i32.le_u $current (global.get $EMPTY_HASHMAP))
          ;; if first element, set return to new element
          (local.set $ret $res))
        ;; update current to point to new element
        (local.set $current $res)

        (br $loop)
      )
    )

    ;; MAP_LOOP_DONE
    $ret
  )


  (func $hash_map_Q (param $args i32) (result i32)
    ($TRUE_FALSE (i32.eq ($TYPE ($DEREF_META ($MEM_VAL1_ptr $args)))
                         (global.get $HASHMAP_T))))

  (func $assoc (param $args i32) (result i32)
    (LET $hm ($MEM_VAL1_ptr $args)
         $key 0)
    (local.set $args ($MEM_VAL0_ptr $args))

    (drop ($INC_REF $hm))
    (block $done
      (loop $loop
        (br_if $done (OR (i32.eqz ($VAL0 $args))
                         (i32.eqz ($VAL0 ($MEM_VAL0_ptr $args)))))
        (local.set $hm ($ASSOC1 $hm ($MEM_VAL1_ptr $args)
                                ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args))))
        (local.set $args ($MEM_VAL0_ptr ($MEM_VAL0_ptr $args)))

        (br $loop)
      )
    )
    $hm
  )

  (func $get (param $args i32) (result i32)
    (LET $hm  ($MEM_VAL1_ptr $args)
         $key ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args)))
    (if (result i32) (i32.eq $hm (global.get $NIL))
      (then ($INC_REF (global.get $NIL)))
      (else ($INC_REF (i32.wrap_i64 ($HASHMAP_GET $hm $key)))))
  )

  (func $contains_Q (param $args i32) (result i32)
    (LET $hm  ($MEM_VAL1_ptr $args)
         $key ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args)))
    ($TRUE_FALSE
      (if (result i32) (i32.eq $hm (global.get $NIL))
        (then 0)
        (else (i32.wrap_i64
                (i64.shr_u ($HASHMAP_GET $hm $key) (i64.const 32))))))
  )

  (func $keys_or_vals (param $hm i32 $keys i32) (result i32)
    (LET $res     ($MAP_LOOP_START (global.get $LIST_T))
         $val2    0
         ;; MAP_LOOP stack
         $ret     $res
         $current $res
         $empty   $res)

    (block $done
      (loop $loop
        (br_if $done (i32.eqz ($VAL0 $hm)))

        (if $keys
          (then (local.set $val2 ($INC_REF ($MEM_VAL1_ptr $hm))))
          (else (local.set $val2 ($INC_REF ($MEM_VAL2_ptr $hm)))))

        ;; next element
        (local.set $hm ($MEM_VAL0_ptr $hm))

        ;; update the return sequence structure
        ;; do not release val2 since we are pulling it from the
        ;; arguments and not creating it here
        ;; MAP_LOOP_UPDATE
        (local.set $res ($MAP_LOOP_UPDATE (global.get $LIST_T)
                                          $empty $current $val2 0))
        (if (i32.le_u $current (global.get $EMPTY_HASHMAP))
          ;; if first element, set return to new element
          (local.set $ret $res))
        ;; update current to point to new element
        (local.set $current $res)

	(br $loop)
      )
    )

    ;; MAP_LOOP_DONE
    $ret
  )

  (func $keys (param $args i32) (result i32)
    ($keys_or_vals ($MEM_VAL1_ptr $args) 1))

  (func $vals (param $args i32) (result i32)
    ($keys_or_vals ($MEM_VAL1_ptr $args) 0))

  (func $sequential_Q (param $args i32) (result i32)
    ($TRUE_FALSE (OR (i32.eq ($TYPE ($MEM_VAL1_ptr $args))
                             (global.get $LIST_T))
                     (i32.eq ($TYPE ($MEM_VAL1_ptr $args))
                             (global.get $VECTOR_T)))))

  (func $cons (param $args i32) (result i32)
    ($LIST ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args)) ($MEM_VAL1_ptr $args)))

  (func $concat (param $args i32) (result i32)
    (local $last_sl i64)
    (LET $res     ($INC_REF (global.get $EMPTY_LIST))
         $current $res
         $sl      0
         $last    0
         $arg     0)
    (block $done
      (loop $loop
        (br_if $done (i32.le_u $args (global.get $EMPTY_HASHMAP)))
        (local.set $arg ($MEM_VAL1_ptr $args))
        ;; skip empty elements
        (if (i32.le_s $arg (global.get $EMPTY_HASHMAP))
          (then
            (local.set $args ($MEM_VAL0_ptr $args))
            (br $loop)))
        (local.set $last_sl ($SLICE $arg 0 -1))
        (local.set $sl (i32.wrap_i64 $last_sl))
        (local.set $last (i32.wrap_i64 (i64.shr_u $last_sl (i64.const 32))))
        (if (i32.eq $res (global.get $EMPTY_LIST))
          (then
            ;; if this is the first element, set the return to the slice
            (local.set $res $sl))
          (else
            ;; otherwise attach current to sliced
            (i32.store ($VAL0_ptr $current) ($IDX $sl))))
        ;; update current to end of sliced list
        (local.set $current $last)
        ;; release empty since no longer part of the slice
        ($RELEASE (global.get $EMPTY_LIST))

        (local.set $args ($MEM_VAL0_ptr $args))
        (br $loop)
      )
    )
    $res
  )

  (func $nth (param $args i32) (result i32)
    (LET $a   ($MEM_VAL1_ptr $args)
         $idx ($VAL0 ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args)))
         $i   0)

    (block $done
      (loop $loop
        (br_if $done (OR (i32.ge_s $i $idx) (i32.eqz ($VAL0 $a))))
        (local.set $i (i32.add $i 1))
        (local.set $a ($MEM_VAL0_ptr $a))
        (br $loop)
      )
    )
    (if (i32.eq ($VAL0 $a) 0)
      (then
        ($THROW_STR_0 \"nth: index out of range\")
        (return 0)))

    ($INC_REF ($MEM_VAL1_ptr $a))
  )

  (func $first (param $args i32) (result i32)
    (LET $res (global.get $NIL)
         $a   ($MEM_VAL1_ptr $args))
    (if (AND (i32.ne $a (global.get $NIL))
             (i32.ne ($VAL0 $a) 0))
      (local.set $res ($MEM_VAL1_ptr $a)))
    ($INC_REF $res)
  )

  (func $rest (param $args i32) (result i32)
    (LET $a ($MEM_VAL1_ptr $args))
    (if (i32.eq $a (global.get $NIL))
      (return ($INC_REF (global.get $EMPTY_LIST))))
    (if (i32.ne ($VAL0 $a) 0)
      (local.set $a ($MEM_VAL0_ptr $a)))
    ($FORCE_SEQ_TYPE (global.get $LIST_T) $a)
  )

  ;;;

  (func $empty_Q (param $args i32) (result i32)
    ($TRUE_FALSE ($EMPTY_Q ($MEM_VAL1_ptr $args))))

  (func $count (param $args i32) (result i32)
    ($INTEGER ($COUNT ($MEM_VAL1_ptr $args))))

  (func $apply (param $args i32) (result i32)
    (local $last_sl i64)
    (LET $f          ($MEM_VAL1_ptr $args)
         $f_args     0
         $rest_args  ($MEM_VAL0_ptr $args)
         $rest_count ($COUNT $rest_args)
         $last       0
         $res        0)

    (if (i32.le_s $rest_count 1)
      (then
        ;; no intermediate args
        (if (i32.ne ($TYPE ($MEM_VAL1_ptr $rest_args)) (global.get $LIST_T))
          (then
            ;; not a list, so convert it first
            (local.set $f_args ($FORCE_SEQ_TYPE (global.get $LIST_T)
                                                ($MEM_VAL1_ptr $rest_args))))
          (else
            ;; inc ref since we will release after APPLY
            (local.set $f_args ($INC_REF ($MEM_VAL1_ptr $rest_args))))))
      (else
        ;; 1 or more intermediate args
        (local.set $last_sl ($SLICE $rest_args 0 (i32.sub $rest_count 1)))
        (local.set $f_args (i32.wrap_i64 $last_sl))
        (local.set $last (i32.wrap_i64 (i64.shr_u $last_sl (i64.const 32))))
        ;; release the terminator of the new list (we skip over it)
        ;; we already checked for an empty list above, so $last is
        ;; a real non-empty list
        ($RELEASE ($MEM_VAL0_ptr $last))
        ;; attach end of slice to final args element
        (i32.store ($VAL0_ptr $last) ($IDX ($LAST $rest_args)))
        ))

    (local.set $res ($APPLY $f $f_args))

    ;; release new args
    ($RELEASE $f_args)
    $res
  )

  (func $map (param $args i32) (result i32)
    (LET $f         ($MEM_VAL1_ptr $args)
         $rest_args ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args))
         $f_args    0
         $res       ($MAP_LOOP_START (global.get $LIST_T))
         ;; push MAP_LOOP stack
         $ret       $res
         $current   $res
         $empty     $res)

    (block $done
      (loop $loop
        (br_if $done (i32.eqz ($VAL1 $rest_args)))

        ;; create argument list for apply
        (local.set $f_args ($ALLOC (global.get $LIST_T)
                                   (global.get $EMPTY_LIST)
                                   ($MEM_VAL1_ptr $rest_args)
                                   0))

        (local.set $res ($APPLY $f $f_args))
        ($RELEASE $f_args)

        ;; go to the next element
        (local.set $rest_args ($MEM_VAL0_ptr $rest_args))

        (if (global.get $error_type)
          (then
            ;; if error, release the unattached element
            ($RELEASE $res)
            (br $done)))

        ;; update the return sequence structure
        ;; MAP_LOOP_UPDATE
        (local.set $res ($MAP_LOOP_UPDATE (global.get $LIST_T)
                                          $empty $current $res 0))
        (if (i32.le_u $current (global.get $EMPTY_HASHMAP))
          ;; if first element, set return to new element
          (local.set $ret $res))
        ;; update current to point to new element
        (local.set $current $res)

	(br $loop)
      )
    )

    ;; MAP_LOOP_DONE
    $ret
  )

  ;;;

  (func $with_meta (param $args i32) (result i32)
    (LET $mv   ($MEM_VAL1_ptr $args)
         $meta ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args)))
    ;; remove existing metadata first
    ($ALLOC (global.get $METADATA_T) ($DEREF_META $mv) $meta 0)
  )

  (func $meta (param $args i32) (result i32)
    (if (result i32) (i32.eq ($TYPE ($MEM_VAL1_ptr $args)) (global.get $METADATA_T))
      (then ($INC_REF ($MEM_VAL1_ptr ($MEM_VAL1_ptr $args))))
      (else ($INC_REF (global.get $NIL)))))

  (func $atom (param $args i32) (result i32)
    ($ALLOC_SCALAR (global.get $ATOM_T) ($VAL1 $args)))

  (func $atom_Q (param $args i32) (result i32)
    ($TRUE_FALSE (i32.eq ($TYPE ($MEM_VAL1_ptr $args)) (global.get $ATOM_T))))

  (func $deref (param $args i32) (result i32)
    ($INC_REF ($MEM_VAL0_ptr ($MEM_VAL1_ptr $args))))

  (func $_reset_BANG (param $atom i32 $val i32) (result i32)
    ;; release current value since we are about to overwrite it
    ($RELEASE ($MEM_VAL0_ptr $atom))
    ;; inc ref by 2 for atom ownership and since we are returning it
    (drop ($INC_REF ($INC_REF $val)))
    ;; update the value
    (i32.store ($VAL0_ptr $atom) ($IDX $val))
    $val
  )

  (func $reset_BANG (param $args i32) (result i32)
    (LET $atom ($MEM_VAL1_ptr $args)
         $val  ($MEM_VAL1_ptr ($MEM_VAL0_ptr $args)))
    ($_reset_BANG $atom $val)
  )

  (func $swap_BANG (param $args i32) (result i32)
    (LET $atom      ($MEM_VAL1_ptr $args)
         $f_args    ($MEM_VAL0_ptr $args)
         $rest_args ($MEM_VAL0_ptr $f_args)
         ;; add atom value to front of the args list
         $s_args    ($LIST $rest_args ($MEM_VAL0_ptr $atom)) ;; cons
         $f         ($MEM_VAL1_ptr $f_args)
         $res       ($APPLY $f $s_args))
    ;; release args
    ($RELEASE $s_args)
    ;; use reset to update the value
    (drop ($_reset_BANG $atom $res))
    ;; but decrease the ref cnt of return by 1 (not sure why)
    ($RELEASE $res)
    $res
  )

  ;;;

  (func $pr_memory_summary (param $args i32) (result i32)
    ($PR_MEMORY_SUMMARY_SMALL)
    ($INC_REF (global.get $NIL))
  )

  (func $nop (param $args i32) (result i32)
    ($INC_REF (global.get $NIL)))

  (table
    funcref
    (elem $nop ;; placeholder for eval which will use 0
          $equal_Q
          $throw
          $nil_Q
          $true_Q
          $false_Q
          $number_Q
          $string_Q
          $symbol
          $symbol_Q
          $keyword
          $keyword_Q
          $fn_Q
          $macro_Q

          ;; 14
          $core_pr_str
          $str
          $prn
          $println
          $core_readline
          $read_string
          $slurp
          $lt
          $lte
          $gt
          $gte
          $add
          $subtract
          $multiply
          $divide
          $time_ms

          ;; 30
          $list
          $list_Q
          $vector
          $vector_Q
          $hash_map
          $hash_map_Q
          $assoc
          $nop ;; $dissoc
          $get
          $contains_Q
          $keys
          $vals

          ;; 42
          $sequential_Q
          $cons
          $concat
          $nth
          $first
          $rest
          $empty_Q
          $count
          $apply
          $map
          $nop ;; $conj
          $nop ;; $seq

          ;; 54
          $with_meta
          $meta
          $atom
          $atom_Q
          $deref
          $reset_BANG
          $swap_BANG

          $pr_memory_summary
    )
  )

  (func $add_core_ns (param $env i32)
    ;;(drop ($ENV_SET_S $env \"eval\"        ($FUNCTION 0)))
    (drop ($ENV_SET_S $env \"=\"           ($FUNCTION 1)))
    (drop ($ENV_SET_S $env \"throw\"       ($FUNCTION 2)))
    (drop ($ENV_SET_S $env \"nil?\"        ($FUNCTION 3)))
    (drop ($ENV_SET_S $env \"true?\"       ($FUNCTION 4)))
    (drop ($ENV_SET_S $env \"false?\"      ($FUNCTION 5)))
    (drop ($ENV_SET_S $env \"number?\"     ($FUNCTION 6)))
    (drop ($ENV_SET_S $env \"string?\"     ($FUNCTION 7)))
    (drop ($ENV_SET_S $env \"symbol\"      ($FUNCTION 8)))
    (drop ($ENV_SET_S $env \"symbol?\"     ($FUNCTION 9)))
    (drop ($ENV_SET_S $env \"keyword\"     ($FUNCTION 10)))
    (drop ($ENV_SET_S $env \"keyword?\"    ($FUNCTION 11)))
    (drop ($ENV_SET_S $env \"fn?\"         ($FUNCTION 12)))
    (drop ($ENV_SET_S $env \"macro?\"      ($FUNCTION 13)))

    (drop ($ENV_SET_S $env \"pr-str\"      ($FUNCTION 14)))
    (drop ($ENV_SET_S $env \"str\"         ($FUNCTION 15)))
    (drop ($ENV_SET_S $env \"prn\"         ($FUNCTION 16)))
    (drop ($ENV_SET_S $env \"println\"     ($FUNCTION 17)))
    (drop ($ENV_SET_S $env \"readline\"    ($FUNCTION 18)))
    (drop ($ENV_SET_S $env \"read-string\" ($FUNCTION 19)))
    (drop ($ENV_SET_S $env \"slurp\"       ($FUNCTION 20)))
    (drop ($ENV_SET_S $env \"<\"           ($FUNCTION 21)))
    (drop ($ENV_SET_S $env \"<=\"          ($FUNCTION 22)))
    (drop ($ENV_SET_S $env \">\"           ($FUNCTION 23)))
    (drop ($ENV_SET_S $env \">=\"          ($FUNCTION 24)))
    (drop ($ENV_SET_S $env \"+\"           ($FUNCTION 25)))
    (drop ($ENV_SET_S $env \"-\"           ($FUNCTION 26)))
    (drop ($ENV_SET_S $env \"*\"           ($FUNCTION 27)))
    (drop ($ENV_SET_S $env \"/\"           ($FUNCTION 28)))
    (drop ($ENV_SET_S $env \"time-ms\"     ($FUNCTION 29)))

    (drop ($ENV_SET_S $env \"list\"        ($FUNCTION 30)))
    (drop ($ENV_SET_S $env \"list?\"       ($FUNCTION 31)))
    (drop ($ENV_SET_S $env \"vector\"      ($FUNCTION 32)))
    (drop ($ENV_SET_S $env \"vector?\"     ($FUNCTION 33)))
    (drop ($ENV_SET_S $env \"hash-map\"    ($FUNCTION 34)))
    (drop ($ENV_SET_S $env \"map?\"        ($FUNCTION 35)))
    (drop ($ENV_SET_S $env \"assoc\"       ($FUNCTION 36)))
    (drop ($ENV_SET_S $env \"dissoc\"      ($FUNCTION 37)))
    (drop ($ENV_SET_S $env \"get\"         ($FUNCTION 38)))
    (drop ($ENV_SET_S $env \"contains?\"   ($FUNCTION 39)))
    (drop ($ENV_SET_S $env \"keys\"        ($FUNCTION 40)))
    (drop ($ENV_SET_S $env \"vals\"        ($FUNCTION 41)))

    (drop ($ENV_SET_S $env \"sequential?\" ($FUNCTION 42)))
    (drop ($ENV_SET_S $env \"cons\"        ($FUNCTION 43)))
    (drop ($ENV_SET_S $env \"concat\"      ($FUNCTION 44)))
    (drop ($ENV_SET_S $env \"nth\"         ($FUNCTION 45)))
    (drop ($ENV_SET_S $env \"first\"       ($FUNCTION 46)))
    (drop ($ENV_SET_S $env \"rest\"        ($FUNCTION 47)))
    (drop ($ENV_SET_S $env \"empty?\"      ($FUNCTION 48)))
    (drop ($ENV_SET_S $env \"count\"       ($FUNCTION 49)))
    (drop ($ENV_SET_S $env \"apply\"       ($FUNCTION 50)))
    (drop ($ENV_SET_S $env \"map\"         ($FUNCTION 51)))

    (drop ($ENV_SET_S $env \"conj\"        ($FUNCTION 52)))
    (drop ($ENV_SET_S $env \"seq\"         ($FUNCTION 53)))

    (drop ($ENV_SET_S $env \"with-meta\"   ($FUNCTION 54)))
    (drop ($ENV_SET_S $env \"meta\"        ($FUNCTION 55)))
    (drop ($ENV_SET_S $env \"atom\"        ($FUNCTION 56)))
    (drop ($ENV_SET_S $env \"atom?\"       ($FUNCTION 57)))
    (drop ($ENV_SET_S $env \"deref\"       ($FUNCTION 58)))
    (drop ($ENV_SET_S $env \"reset!\"      ($FUNCTION 59)))
    (drop ($ENV_SET_S $env \"swap!\"       ($FUNCTION 60)))

    (drop ($ENV_SET_S $env \"pr-memory-summary\"       ($FUNCTION 61)))
  )
)")
