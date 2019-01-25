(ns malhala.one)

(def types
  ";; Mal value memory layout
;;   type           words
;;   ----------     ----------
;;   nil            ref/ 0 |  0           |               |
;;   false          ref/ 1 |  0           |               |
;;   true           ref/ 1 |  1           |               |
;;   integer        ref/ 2 | int          |               |
;;   float          ref/ 3 | ???          |               |
;;   string/kw      ref/ 4 | string ptr   |               |
;;   symbol         ref/ 5 | string ptr   |               |
;;   list           ref/ 6 | next mem idx | val mem idx   |
;;   vector         ref/ 7 | next mem idx | val mem idx   |
;;   hashmap        ref/ 8 | next mem idx | key mem idx   | val mem idx
;;   function       ref/ 9 | fn idx       |               |
;;   mal function   ref/10 | body mem idx | param mem idx | env mem idx
;;   macro fn       ref/11 | body mem idx | param mem idx | env mem idx
;;   atom           ref/12 | val mem idx  |               |
;;   environment    ref/13 | hmap mem idx | outer mem idx |
;;   metadata       ref/14 | obj mem idx  | meta mem idx  |
;;   FREE            sz/15 | next mem idx |               |

(module $types

  (global $NIL_T                  i32 0)
  (global $BOOLEAN_T              i32 1)
  (global $INTEGER_T              i32 2)
  (global $FLOAT_T                i32 3)
  (global $STRING_T               i32 4)
  (global $SYMBOL_T               i32 5)
  (global $LIST_T                 i32 6)
  (global $VECTOR_T               i32 7)
  (global $HASHMAP_T              i32 8)
  (global $FUNCTION_T             i32 9)
  (global $MALFUNC_T              i32 10)
  (global $MACRO_T                i32 11)
  (global $ATOM_T                 i32 12)
  (global $ENVIRONMENT_T          i32 13)
  (global $METADATA_T             i32 14)
  (global $FREE_T                 i32 15)

  (global $error_type             (mut i32) 0)
  (global $error_val              (mut i32) 0)
  ;; Index into static string memory (static.wast)
  (global $error_str              (mut i32) 0)

  (global $NIL                    (mut i32) 0)
  (global $FALSE                  (mut i32) 0)
  (global $TRUE                   (mut i32) 0)
  (global $EMPTY_LIST             (mut i32) 0)
  (global $EMPTY_VECTOR           (mut i32) 0)
  (global $EMPTY_HASHMAP          (mut i32) 0)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; General functions

  (func $INC_REF (param $mv i32) (result i32)
    (i32.store $mv (i32.add (i32.load $mv) 32))
    $mv
  )

  (func $TRUE_FALSE (param $val i32) (result i32)
    ($INC_REF (if (result i32) $val (global.get $TRUE) (global.get $FALSE)))
  )

  (func $THROW_STR_0 (param $fmt i32)
    (drop ($sprintf_1 (global.get $error_str) $fmt \"\"))
    (global.set $error_type 1)
  )

  (func $THROW_STR_1 (param $fmt i32) (param $v0 i32)
    (drop ($sprintf_1 (global.get $error_str) $fmt $v0))
    (global.set $error_type 1)
  )

  (func $EQUAL_Q (param $a i32 $b i32) (result i32)
    (LET $ta ($TYPE $a)
         $tb ($TYPE $b))

    (if (AND (OR (i32.eq $ta (global.get $LIST_T))
                 (i32.eq $ta (global.get $VECTOR_T)))
             (OR (i32.eq $tb (global.get $LIST_T))
                 (i32.eq $tb (global.get $VECTOR_T))))
      (then
        ;; EQUAL_Q_SEQ
        (block $done
          (loop $loop
            (if (OR (i32.eq ($VAL0 $a) 0) (i32.eq ($VAL0 $b) 0))
              (br $done))
            (if ($EQUAL_Q ($MEM_VAL1_ptr $a) ($MEM_VAL1_ptr $b))
              (then
                (local.set $a ($MEM_VAL0_ptr $a))
                (local.set $b ($MEM_VAL0_ptr $b)))
              (else
                (return 0)))
            (br $loop)
          )
        )
        (return (AND (i32.eq ($VAL0 $a) 0) (i32.eq ($VAL0 $b) 0))))
    (else (if (AND (i32.eq $ta (global.get $HASHMAP_T))
                   (i32.eq $tb (global.get $HASHMAP_T)))
      ;; EQUAL_Q_HM
      (then (return 1))
    ;; TODO: remove this once strings are interned
    (else (if (OR (AND (i32.eq $ta (global.get $STRING_T))
                       (i32.eq $tb (global.get $STRING_T)))
                  (AND (i32.eq $ta (global.get $SYMBOL_T))
                       (i32.eq $tb (global.get $SYMBOL_T))))
      (then (return (i32.eqz ($strcmp ($to_String $a) ($to_String $b)))))
    (else
      (return (AND (i32.eq $ta $tb)
                   (i32.eq ($VAL0 $a) ($VAL0 $b))))))))))
    0 ;; not reachable
  )

  (func $DEREF_META (param $mv i32) (result i32)
    (loop $loop
      (if (i32.eq ($TYPE $mv) (global.get $METADATA_T))
        (then
          (local.set $mv ($MEM_VAL0_ptr $mv))
          (br $loop)))
    )
    $mv
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; string functions

  (func $to_MalString (param $mv i32) (result i32)
    ;; TODO: assert mv is a string/keyword/symbol
    (i32.add (global.get $string_mem) ($VAL0 $mv))
  )

  (func $to_String (param $mv i32) (result i32)
    ;; skip string refcnt and size
    (i32.add 4 ($to_MalString $mv))
  )

  ;; Duplicate regular character array string into a Mal string and
  ;; return the MalVal pointer
  (func $STRING (param $type i32 $str i32) (result i32)
    (LET $ms ($ALLOC_STRING $str ($strlen $str) 1))
    ($ALLOC_SCALAR $type (i32.sub $ms (global.get $string_mem)))
  )

  ;; Find first duplicate (internet) of mv. If one is found, free up
  ;; mv and return the interned version. If no duplicate is found,
  ;; return NULL.
  (func $INTERN_STRING (param $mv i32) (result i32)
    (LET $res         0
         $ms          ($to_MalString $mv)
         $existing_ms ($FIND_STRING (i32.add $ms 4))
         $tmp         0)
    (if (AND $existing_ms (i32.lt_s $existing_ms $ms))
      (then
        (local.set $tmp $mv)
        (local.set $res ($ALLOC_SCALAR (global.get $STRING_T)
                                       (i32.sub $existing_ms
                                                  (global.get $string_mem))))
        (i32.store16 $existing_ms (i32.add (i32.load16_u $existing_ms) 1))
        ($RELEASE $tmp)))
    $res
  )

  (func $STRING_INIT (param $type i32) (result i32)
    (LET $ms ($ALLOC_STRING \"\" 0 0))
    ($ALLOC_SCALAR $type (i32.sub $ms (global.get $string_mem)))
  )

  (func $STRING_FINALIZE (param $mv i32 $size i32) (result i32)
    ;; Check if the new string can be interned.
    (LET $tmp ($INTERN_STRING $mv)
         $ms  ($to_MalString $mv))
    (if $tmp
      (then
        (local.set $mv $tmp))
      (else
        ;;; ms->size = sizeof(MalString) + size + 1
        (i32.store16 (i32.add $ms 2)
                     (i32.add (i32.add 4 $size) 1))
        ;;; string_mem_next = (void *)ms  + ms->size
        (global.set $string_mem_next
                    (i32.add $ms (i32.load16_u (i32.add $ms 2))))))
    $mv
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; numeric functions

  (func $INTEGER (param $val i32) (result i32)
    ($ALLOC_SCALAR (global.get $INTEGER_T) $val)
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; sequence functions

  (func $MAP_LOOP_START (param $type i32) (result i32)
    (LET $res (if (result i32) (i32.eq $type (global.get $LIST_T))
                (then (global.get $EMPTY_LIST))
              (else (if (result i32) (i32.eq $type (global.get $VECTOR_T))
                (then (global.get $EMPTY_VECTOR))
              (else (if (result i32) (i32.eq $type (global.get $HASHMAP_T))
                (then (global.get $EMPTY_HASHMAP))
              (else
                ($THROW_STR_1 \"read_seq invalid type %d\" $type)
                0)))))))

    ($INC_REF $res)
  )

  (func $MAP_LOOP_UPDATE (param $type i32) (param $empty i32)
        (param $current i32) (param $val2 i32) (param $val3 i32)
        (result i32)
    (LET $res ($ALLOC $type $empty $val2 $val3))

    ;; sequence took ownership
    ($RELEASE $empty)
    ($RELEASE $val2)
    (if (i32.eq $type (global.get $HASHMAP_T))
      ($RELEASE $val3))
    (if (i32.gt_u $current (global.get $EMPTY_HASHMAP))
      ;; if not first element, set current next to point to new element
      (i32.store ($VAL0_ptr $current) ($IDX $res)))

    $res
  )

  (func $FORCE_SEQ_TYPE (param $type i32) (param $mv i32) (result i32)
    (LET $res 0)
    ;; if it's already the right type, inc ref cnt and return it
    (if (i32.eq $type ($TYPE $mv)) (return ($INC_REF $mv)))
    ;; if it's empty, return the sequence match
    (if (i32.le_u $mv (global.get $EMPTY_HASHMAP))
      (return ($MAP_LOOP_START $type)))
    ;; otherwise, copy first element to turn it into correct type
    ($ALLOC $type ($MEM_VAL0_ptr $mv) ($MEM_VAL1_ptr $mv) 0)
  )

  (func $LIST (param $seq i32 $first i32) (result i32)
    ($ALLOC (global.get $LIST_T) $seq $first 0)
  )

  (func $LIST2 (param $first i32 $second i32) (result i32)
    ;; last element is empty list
    (LET $tmp ($LIST (global.get $EMPTY_LIST) $second)
         $res ($LIST $tmp $first))
    ($RELEASE $tmp) ;; new list takes ownership of previous
    $res
  )

  (func $LIST3 (param $first i32 $second i32 $third i32) (result i32)
    (LET $tmp ($LIST2 $second $third)
         $res ($LIST $tmp $first))
    ($RELEASE $tmp) ;; new list takes ownership of previous
    $res
  )

  (func $LIST_Q (param $mv i32) (result i32)
    (i32.eq ($TYPE $mv) (global.get $LIST_T))
  )

  (func $EMPTY_Q (param $mv i32) (result i32)
    (i32.eq ($VAL0 $mv) 0)
  )

  (func $COUNT (param $mv i32) (result i32)
    (LET $cnt 0)
    (block $done
      (loop $loop
        (if (i32.eq ($VAL0 $mv) 0) (br $done))
        (local.set $cnt (i32.add $cnt 1))
        (local.set $mv ($MEM_VAL0_ptr $mv))
        (br $loop)
      )
    )
    $cnt
  )

  (func $LAST (param $mv i32) (result i32)
    (LET $cur 0)
    ;; TODO: check that actually a list/vector
    (if (i32.eq ($VAL0 $mv) 0)
      ;; empty seq, return nil
      (return ($INC_REF (global.get $NIL))))
    (block $done
      (loop $loop
        ;; end, return previous value
        (if (i32.eq ($VAL0 $mv) 0) (br $done))
        ;; current becomes previous entry
        (local.set $cur $mv)
        ;; next entry
        (local.set $mv ($MEM_VAL0_ptr $mv))
        (br $loop)
      )
    )
    ($INC_REF ($MEM_VAL1_ptr $cur))
  )

  ;; make a copy of sequence seq from index start to end
  ;; set last to last element of slice before the empty
  ;; set after to element following slice (or original)
  (func $SLICE (param $seq i32) (param $start i32) (param $end i32)
        (result i64)
    (LET $idx  0
         $res  ($INC_REF (global.get $EMPTY_LIST))
         $last 0
         $tmp  $res)
    ;; advance seq to start
    (block $done
      (loop $loop
        (if (OR (i32.ge_s $idx $start)
                (i32.eqz ($VAL0 $seq)))
          (br $done))
        (local.set $seq ($MEM_VAL0_ptr $seq))
        (local.set $idx (i32.add $idx 1))
        (br $loop)
      )
    )
    (block $done
      (loop $loop
	;; if current position is at end, then return or if we reached
        ;; end seq, then return
        (if (OR (AND (i32.ne $end -1)
                     (i32.ge_s $idx $end))
                (i32.eqz ($VAL0 $seq)))
          (then
            (local.set $res $tmp)
            (br $done)))
        ;; allocate new list element with copied value
        (local.set $res ($LIST (global.get $EMPTY_LIST)
                               ($MEM_VAL1_ptr $seq)))
        ;; sequence took ownership
        ($RELEASE (global.get $EMPTY_LIST))
        (if (i32.eqz $last)
          (then
            ;; if first element, set return value to new element
            (local.set $tmp $res))
          (else
            ;; if not the first element, set return value to new element
            (i32.store ($VAL0_ptr $last) ($IDX $res))))
        (local.set $last $res) ;; update last list element
        ;; advance to next element of seq
        (local.set $seq ($MEM_VAL0_ptr $seq))
        (local.set $idx (i32.add $idx 1))
        (br $loop)
      )
    )

    ;; combine last/res as hi 32/low 32 of i64
    (i64.or
      (i64.shl (i64.extend_i32_u $last) (i64.const 32))
      (i64.extend_i32_u $res))
  )

  (func $HASHMAP (result i32)
    ;; just point to static empty hash-map
    ($INC_REF (global.get $EMPTY_HASHMAP))
  )

  (func $ASSOC1 (param $hm i32 $k i32 $v i32) (result i32)
    (LET $res ($ALLOC (global.get $HASHMAP_T) $hm $k $v))
    ;; we took ownership of previous release
    ($RELEASE $hm)
    $res
  )

  (func $ASSOC1_S (param $hm i32 $k i32 $v i32) (result i32)
    (LET $kmv ($STRING (global.get $STRING_T) $k)
         $res ($ASSOC1 $hm $kmv $v))
    ;; map took ownership of key
    ($RELEASE $kmv)
    $res
  )

  (func $HASHMAP_GET (param $hm i32) (param $key_mv i32) (result i64)
    (LET $key         ($to_String $key_mv)
         $found       0
         $res         0
         $test_key_mv 0)

    (block $done
      (loop $loop
        ;;; if (VAL0(hm) == 0)
        (if (i32.eq ($VAL0 $hm) 0)
          (then
            (local.set $res (global.get $NIL))
            (br $done)))
        ;;; test_key_mv = MEM_VAL1(hm)
        (local.set $test_key_mv ($MEM_VAL1_ptr $hm))
        ;;; if (strcmp(key, to_String(test_key_mv)) == 0)
        (if (i32.eq ($strcmp $key ($to_String $test_key_mv)) 0)
          (then
            (local.set $found 1)
            (local.set $res ($MEM_VAL2_ptr $hm))
            (br $done)))
        (local.set $hm ($MEM_VAL0_ptr $hm))

        (br $loop)
      )
    )

    ;; combine found/res as hi 32/low 32 of i64
    (i64.or (i64.shl (i64.extend_i32_u $found) (i64.const 32))
            (i64.extend_i32_u $res))
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; function functions

  (func $FUNCTION (param $index i32) (result i32)
    ($ALLOC_SCALAR (global.get $FUNCTION_T) $index)
  )

  (func $MALFUNC (param $ast i32 $params i32 $env i32) (result i32)
    ($ALLOC (global.get $MALFUNC_T) $ast $params $env)
  )

)")















(def mem
  "(module $mem
  (global $MEM_SIZE               i32 1048576)
  (global $STRING_MEM_SIZE        i32 1048576)

  (global $heap_start             (mut i32) 0)
  (global $heap_end               (mut i32) 0)

  (global $mem                    (mut i32) 0)
  (global $mem_unused_start       (mut i32) 0)
  (global $mem_free_list          (mut i32) 0)
  (global $mem_user_start         (mut i32) 0)

  (global $string_mem             (mut i32) 0)
  (global $string_mem_next        (mut i32) 0)
  (global $string_mem_user_start  (mut i32) 0)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; General type storage/pointer functions

  (func $VAL0_ptr (param $mv i32) (result i32)
    (i32.add $mv 4))
  (func $VAL1_ptr (param $mv i32) (result i32)
    (i32.add $mv 8))

  (func $VAL0 (param $mv i32) (result i32)
    (i32.load (i32.add $mv 4)))
  (func $VAL1 (param $mv i32) (result i32)
    (i32.load (i32.add $mv 8)))


  (func $MEM_VAL0_ptr (param $mv i32) (result i32)
    (i32.add (global.get $mem)
             (i32.mul (i32.load (i32.add $mv 4)) 4)))
  (func $MEM_VAL1_ptr (param $mv i32) (result i32)
    (i32.add (global.get $mem)
             (i32.mul (i32.load (i32.add $mv 8)) 4)))
  (func $MEM_VAL2_ptr (param $mv i32) (result i32)
    (i32.add (global.get $mem)
             (i32.mul (i32.load (i32.add $mv 12)) 4)))

  ;; Returns the memory index mem of mv
  ;; Will usually be used with a load or store by the caller
  (func $IDX (param $mv i32) (result i32)
    ;; MalVal memory 64 bit (2 * i32) aligned
    (i32.div_u (i32.sub $mv (global.get $mem)) 4))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Returns the address of 'mem[mv_idx]'
  (func $MalVal_ptr (param $mv_idx i32) (result i32)
    ;; MalVal memory 64 bit (2 * i32) aligned
    ;;; mem[mv_idx].refcnt_type
    (i32.add (global.get $mem) (i32.mul $mv_idx 4)))

  ;; Returns the address of 'mem[mv_idx].refcnt_type'
  (func $MalVal_refcnt_type (param $mv_idx i32) (result i32)
    (i32.load ($MalVal_ptr $mv_idx)))

  (func $TYPE (param $mv i32) (result i32)
    ;;; type = mv->refcnt_type & 31
    (i32.and (i32.load $mv) 0x1f)) ;; 0x1f == 31

  (func $SET_TYPE (param $mv i32 $type i32)
    ;;; type = mv->refcnt_type & 31
    ;;; mv->refcnt_type += - (mv->refcnt_type & 31) + type
    (i32.store $mv (i32.or
                     (i32.and $type 0x1f) ;; 0x1f == 31
                     (i32.and (i32.load $mv) 0xffffffe1)))
  )


  (func $REFS (param $mv i32) (result i32)
    ;;; type = mv->refcnt_type & 31
    (i32.shr_u (i32.load $mv) 5)) ;; / 32

  ;; Returns the address of 'mem[mv_idx].val[val]'
  ;; Will usually be used with a load or store by the caller
  (func $MalVal_val_ptr (param $mv_idx i32 $val i32) (result i32)
    (i32.add (i32.add ($MalVal_ptr $mv_idx) 4)
             (i32.mul $val 4)))

  ;; Returns the value of 'mem[mv_idx].val[val]'
  (func $MalVal_val (param $mv_idx i32 $val i32) (result i32)
    (i32.load ($MalVal_val_ptr $mv_idx $val)))

  (func $MalType_size (param $type i32) (result i32)
    ;;; if (type <= 5 || type == 9 || type == 12)
    (if (result i32) (OR (i32.le_u $type 5)
                (i32.eq $type 9)
                (i32.eq $type 12))
      (then 2)
      (else
        ;;; else if (type == 8 || type == 10 || type == 11)
        (if (result i32) (OR (i32.eq $type 8)
                    (i32.eq $type 10)
                    (i32.eq $type 11))
          (then 4)
          (else 3)))))

  (func $MalVal_size (param $mv i32) (result i32)
    (LET $type ($TYPE $mv))
    ;; if (type == FREE_T)
    (if (result i32) (i32.eq $type (global.get $FREE_T))
      (then
        ;;; return (mv->refcnt_type & 0xffe0)>>5
        (i32.shr_u (i32.and (i32.load $mv) 0xffe0) 5)) ;;; / 32
      (else
        ;;; return MalType_size(type)
        ($MalType_size $type))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; init_memory

  (func $init_memory
    (LET $heap_size 0)

;;    ($print \">>> init_memory\\n\")

    ($init_printf_mem)

    ;; error_str string buffer
    (global.set $error_str (STATIC_ARRAY 100))
    ;; reader token string buffer
    (global.set $token_buf (STATIC_ARRAY 256))
    ;; printer string buffer
    (global.set $printer_buf (STATIC_ARRAY 4096))

    (local.set $heap_size (i32.add (global.get $MEM_SIZE)
                                   (global.get $STRING_MEM_SIZE)))
    (global.set $heap_start (i32.add (global.get $memoryBase)
                                     (global.get $S_STRING_END)))
    (global.set $heap_end (i32.add (global.get $heap_start)
                                   $heap_size))

    (global.set $mem (global.get $heap_start))
    (global.set $mem_unused_start 0)
    (global.set $mem_free_list 0)

    (global.set $string_mem (i32.add (global.get $heap_start)
                                     (global.get $MEM_SIZE)))
    (global.set $string_mem_next (global.get $string_mem))

    (global.set $mem_user_start (global.get $mem_unused_start))
    (global.set $string_mem_user_start (global.get $string_mem_next))

    ;; Empty values
    (global.set $NIL
                ($ALLOC_SCALAR (global.get $NIL_T) 0))
    (global.set $FALSE
                ($ALLOC_SCALAR (global.get $BOOLEAN_T) 0))
    (global.set $TRUE
                ($ALLOC_SCALAR (global.get $BOOLEAN_T) 1))
    (global.set $EMPTY_LIST
                ($ALLOC (global.get $LIST_T)
                      (global.get $NIL) (global.get $NIL) (global.get $NIL)))
    (global.set $EMPTY_VECTOR
                ($ALLOC (global.get $VECTOR_T)
                      (global.get $NIL) (global.get $NIL) (global.get $NIL)))
    (global.set $EMPTY_HASHMAP
                ($ALLOC (global.get $HASHMAP_T)
                      (global.get $NIL) (global.get $NIL) (global.get $NIL)))

;;    ($print \"<<< init_memory\\n\")

  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; memory management

  (func $ALLOC_INTERNAL (param $type i32
                               $val1 i32 $val2 i32 $val3 i32) (result i32)
    (LET $prev (global.get $mem_free_list)
         $res  (global.get $mem_free_list)
         $size ($MalType_size $type))

    (block $loop_done
      (loop $loop
        ;; res == mem_unused_start
        (if (i32.eq $res (global.get $mem_unused_start))
          (then
            ;; ALLOC_UNUSED
            ;;; if (res + size > MEM_SIZE)
            (if (i32.gt_u (i32.add $res $size) (global.get $MEM_SIZE))
              ;; Out of memory, exit
              ($fatal 7 \"Out of mal memory!\\n\"))
            ;;; if (mem_unused_start += size)
            (global.set $mem_unused_start
                        (i32.add (global.get $mem_unused_start) $size))
            ;;; if (prev == res)
            (if (i32.eq $prev $res)
              (then
                (global.set $mem_free_list (global.get $mem_unused_start)))
              (else
                ;;; mem[prev].val[0] = mem_unused_start
                (i32.store
                  ($MalVal_val_ptr $prev 0)
                  (global.get $mem_unused_start))))
            (br $loop_done)))
        ;; if (MalVal_size(mem+res) == size)
        (if (i32.eq ($MalVal_size ($MalVal_ptr $res))
                    $size)
          (then
            ;; ALLOC_MIDDLE
            ;;; if (res == mem_free_list)
            (if (i32.eq $res (global.get $mem_free_list))
              ;; set free pointer (mem_free_list) to next free
              ;;; mem_free_list = mem[res].val[0];
              (global.set $mem_free_list ($MalVal_val $res 0)))
            ;;  if (res != mem_free_list)
            (if (i32.ne $res (global.get $mem_free_list))
              ;; set previous free to next free
              ;;; mem[prev].val[0] = mem[res].val[0]
              (i32.store ($MalVal_val_ptr $prev 0) ($MalVal_val $res 0)))
            (br $loop_done)))
        ;;; prev = res
        (local.set $prev $res)
        ;;; res = mem[res].val[0]
        (local.set $res ($MalVal_val $res 0))
        (br $loop)
      )
    )
    ;; ALLOC_DONE
    ;;; mem[res].refcnt_type = type + 32
    (i32.store ($MalVal_ptr $res) (i32.add $type 32))
    ;; set val to default val1
    ;;; mem[res].val[0] = val1
    (i32.store ($MalVal_val_ptr $res 0) $val1)
    ;;; if (type > 5 && type != 9)
    (if (AND (i32.gt_u $type 5)
             (i32.ne $type 9))
      (then
        ;; inc refcnt of referenced value
        ;;; mem[val1].refcnt_type += 32
        (i32.store ($MalVal_ptr $val1)
                   (i32.add ($MalVal_refcnt_type $val1) 32))))
    ;;; if (size > 2)
    (if (i32.gt_u $size 2)
      (then
        ;; inc refcnt of referenced value
        ;;; mem[val2].refcnt_type += 32
        (i32.store ($MalVal_ptr $val2)
                   (i32.add ($MalVal_refcnt_type $val2) 32))
        ;;; mem[res].val[1] = val2
        (i32.store ($MalVal_val_ptr $res 1) $val2)))
    ;;; if (size > 3)
    (if (i32.gt_u $size 3)
      (then
        ;; inc refcnt of referenced value
        ;;; mem[val3].refcnt_type += 32
        (i32.store ($MalVal_ptr $val3)
                   (i32.add ($MalVal_refcnt_type $val3) 32))
        ;;; mem[res].val[2] = val3
        (i32.store ($MalVal_val_ptr $res 2) $val3)))

    ;;; return mem + res
    ($MalVal_ptr $res)
  )

  (func $ALLOC_SCALAR (param $type i32 $val1 i32) (result i32)
    ($ALLOC_INTERNAL $type $val1 0 0)
  )

  (func $ALLOC (param $type i32 $val1 i32 $val2 i32 $val3 i32) (result i32)
    ($ALLOC_INTERNAL $type ($IDX $val1) ($IDX $val2) ($IDX $val3))
  )

  (func $RELEASE (param $mv i32)
    (LET $idx 0 $type 0 $size 0)

    ;; Ignore NULLs
    ;;; if (mv == NULL) { return; }
    (if (i32.eqz $mv) (return))
    ;;; idx = mv - mem
    (local.set $idx ($IDX $mv))
    ;;; type = mv->refcnt_type & 31
    (local.set $type (i32.and (i32.load $mv) 0x1f)) ;; 0x1f == 31
    ;;; size = MalType_size(type)
    (local.set $size ($MalType_size $type))

    ;; DEBUG
    ;;; printf(\">>> RELEASE idx: %d, type: %d, size: %d\\n\", idx, type, size)

    (if (i32.eq 0 $mv)
      ($fatal 7 \"RELEASE of NULL!\\n\"))

    (if (i32.eq (global.get $FREE_T) $type)
      (then
        ($printf_2 \"RELEASE of already free mv: 0x%x, idx: 0x%x\\n\" $mv $idx)
        ($fatal 1 \"\")))
    (if (i32.lt_u ($MalVal_refcnt_type $idx) 15)
      (then
        ($printf_2 \"RELEASE of unowned mv: 0x%x, idx: 0x%x\\n\" $mv $idx)
        ($fatal 1 \"\")))

    ;; decrease reference count by one
    (i32.store ($MalVal_ptr $idx)
               (i32.sub ($MalVal_refcnt_type $idx) 32))

    ;; nil, false, true, empty sequences
    (if (i32.le_u $mv (global.get $EMPTY_HASHMAP))
      (then
        (if (i32.lt_u ($MalVal_refcnt_type $idx) 32)
          (then
            ($printf_2 \"RELEASE of unowned mv: 0x%x, idx: 0x%x\\n\" $mv $idx)
            ($fatal 1 \"\")))
        (return)))

    ;; our reference count is not 0, so don't release
    (if (i32.ge_u ($MalVal_refcnt_type $idx) 32)
      (return))

    (block $done
      (block (block (block (block (block (block (block (block (block
      (br_table 0 0 0 0 1 1 2 2 3 0 4 4 5 6 7 8 8 $type))
      ;; nil, boolean, integer, float
      (br $done))
      ;; string, kw, symbol
      ;; release string, then FREE reference
      ($RELEASE_STRING (i32.add (global.get $string_mem) ($VAL0 $mv)))
      (br $done))
      ;; list, vector
      (if (i32.ne ($MalVal_val $idx 0) 0)
        (then
          ;; release next element and value
          ($RELEASE ($MEM_VAL0_ptr $mv))
          ($RELEASE ($MEM_VAL1_ptr $mv))))
      (br $done))
      ;; hashmap
      (if (i32.ne ($MalVal_val $idx 0) 0)
        (then
          ;; release next element, value, and key
          ($RELEASE ($MEM_VAL0_ptr $mv))
          ($RELEASE ($MEM_VAL2_ptr $mv))
          ($RELEASE ($MEM_VAL1_ptr $mv))))
      (br $done))
      ;; mal / macro function
      ;; release ast, params, and environment
      ($RELEASE ($MEM_VAL2_ptr $mv))
      ($RELEASE ($MEM_VAL1_ptr $mv))
      ($RELEASE ($MEM_VAL0_ptr $mv))
      (br $done))
      ;; atom
      ;; release contained/referred value
      ($RELEASE ($MEM_VAL0_ptr $mv))
      (br $done))
      ;; env
      ;; if outer is set then release outer
      (if (i32.ne ($MalVal_val $idx 1) 0)
        ($RELEASE ($MEM_VAL1_ptr $mv)))
      ;; release the env data (hashmap)
      ($RELEASE ($MEM_VAL0_ptr $mv))
      (br $done))
      ;; metadata
      ;; release object and metdata object
      ($RELEASE ($MEM_VAL0_ptr $mv))
      ($RELEASE ($MEM_VAL1_ptr $mv))
      (br $done))
      ;; default/unknown
    )

    ;; FREE, free the current element

    ;; set type(FREE/15) and size
    ;;; mv->refcnt_type = size*32 + FREE_T
    (i32.store $mv (i32.add (i32.mul $size 32) (global.get $FREE_T)))
    (i32.store ($MalVal_val_ptr $idx 0) (global.get $mem_free_list))
    (global.set $mem_free_list $idx)
    (if (i32.ge_u $size 3) (i32.store ($MalVal_val_ptr $idx 1) 0))
    (if (i32.eq $size 4) (i32.store ($MalVal_val_ptr $idx 2) 0))
  )

  ;; find string in string memory or 0 if not found
  (func $FIND_STRING (param $str i32) (result i32)
    (LET $ms (global.get $string_mem))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_s $ms (global.get $string_mem_next)))
        (if (i32.eqz ($strcmp $str (i32.add $ms 4)))
          (return $ms))

        (local.set $ms (i32.add $ms (i32.load16_u (i32.add $ms 2))))
        (br $loop)
      )
    )
    0
  )

  ;; str is a NULL terminated string
  ;; size is number of characters in the string not including the
  ;; trailing NULL
  (func $ALLOC_STRING (param $str i32 $size i32 $intern i32) (result i32)
    (LET $ms 0)

    ;; search for matching string in string_mem
    (if $intern
      (then
        (local.set $ms ($FIND_STRING $str))
        (if $ms
          (then
            ;;; ms->refcnt += 1
            (i32.store16 $ms (i32.add (i32.load16_u $ms) 1))
            (return $ms)))))

    ;; no existing matching string so create a new one
    (local.set $ms (global.get $string_mem_next))
    (i32.store16 $ms 1)
    ;;; ms->size = sizeof(MalString)+size+1
    (i32.store16 offset=2 $ms (i32.add (i32.add 4 $size) 1))
    ($memmove (i32.add $ms 4) $str (i32.add $size 1))
    ;;; string_mem_next = (void *)ms + ms->size
    (global.set $string_mem_next
                ;;(i32.add $ms (i32.load16_u (i32.add $ms 2))))
                (i32.add $ms (i32.load16_u offset=2 $ms)))

;;($printf_2 \"ALLOC_STRING 6 ms 0x%x, refs: %d\\n\" $ms (i32.load16_u $ms))
    $ms
  )

  (func $RELEASE_STRING (param $ms i32)
    (LET $size 0 $next 0 $ms_idx 0 $idx 0 $type 0 $mv 0)

    (if (i32.le_s (i32.load16_u $ms) 0)
      (then
        ($printf_2 \"Release of already free string: %d (0x%x)\\n\"
                   (i32.sub $ms (global.get $string_mem)) $ms)
        ($fatal 1 \"\")))

    ;;; size = ms->size
    (local.set $size (i32.load16_u (i32.add $ms 2)))
    ;;; *next = (void *)ms + size
    (local.set $next (i32.add $ms $size))

    ;;; ms->refcnt -= 1
    (i32.store16 $ms (i32.sub (i32.load16_u $ms) 1))

    (if (i32.eqz (i32.load16_u $ms))
      (then
        (if (i32.gt_s (global.get $string_mem_next) $next)
          (then
            ;; If no more references to this string then free it up by
            ;; shifting up every string afterwards to fill the gap
            ;; (splice).
            ($memmove $ms $next (i32.sub (global.get $string_mem_next)
                                           $next))

            ;; Scan the mem values for string types after the freed
            ;; string and shift their indexes by size
            (local.set $ms_idx (i32.sub $ms (global.get $string_mem)))
            (local.set $idx ($IDX (global.get $EMPTY_HASHMAP)))
            (loop $loop
              (local.set $mv ($MalVal_ptr $idx))
              (local.set $type ($TYPE $mv))
              (if (AND (i32.gt_s ($VAL0 $mv) $ms_idx)
                       (OR (i32.eq $type (global.get $STRING_T))
                           (i32.eq $type (global.get $SYMBOL_T))))
                (i32.store ($VAL0_ptr $mv) (i32.sub ($VAL0 $mv) $size)))
              (local.set $idx (i32.add $idx ($MalVal_size $mv)))

              (br_if $loop (i32.lt_s $idx (global.get $mem_unused_start)))
            )))

        (global.set $string_mem_next
                    (i32.sub (global.get $string_mem_next) $size))))
  )
)")














(def debug
  "(module $debug

  (func $checkpoint_user_memory
    (global.set $mem_user_start (global.get $mem_unused_start))
    (global.set $string_mem_user_start (global.get $string_mem_next))
  )

  (func $CHECK_FREE_LIST (result i32)
    (LET $first (i32.add
                  (global.get $mem)
                  (i32.mul (global.get $mem_free_list) 4))
         $count 0)

    (block $done
      (loop $loop
        (br_if $done
               (i32.ge_s $first
                         (i32.add (global.get $mem)
                                  (i32.mul (global.get $mem_unused_start)
                                             4))))
        (local.set $count (i32.add $count ($MalVal_size $first)))
        (local.set $first (i32.add (global.get $mem) (i32.mul 4 ($VAL0 $first))))
        (br $loop)
      )
    )
    $count
  )

  (func $PR_MEMORY_SUMMARY_SMALL
    (LET $free (i32.sub (global.get $MEM_SIZE)
                          (i32.mul (global.get $mem_unused_start) 4))
         $free_list_count ($CHECK_FREE_LIST)
         $mv              (global.get $NIL)
         $mem_ref_count   0)

    (block $done
      (loop $loop
        (br_if $done (i32.ge_s $mv (i32.add
                                     (global.get $mem)
                                     (i32.mul (global.get $mem_unused_start)
                                                4))))
        (if (i32.ne ($TYPE $mv) (global.get $FREE_T))
          (local.set $mem_ref_count (i32.add $mem_ref_count
                                             (i32.shr_u
                                               (i32.load $mv)
                                               5))))
        (local.set $mv (i32.add $mv (i32.mul 4 ($MalVal_size $mv))))
        (br $loop)
      )
    )

    ($printf_3 \"Free: %d, Values: %d (refs: %d), Emptys: \"
               $free
               (i32.sub
                 (i32.sub (global.get $mem_unused_start) 1)
                 $free_list_count)
               $mem_ref_count)
    (local.set $mv (global.get $NIL))
    (block $done
      (loop $loop
        (br_if $done (i32.gt_s $mv (global.get $TRUE)))
        ($printf_1 \"%d,\" (i32.div_s (i32.load $mv) 32))
        (local.set $mv (i32.add $mv 8))
        (br $loop)
      )
    )
    (local.set $mv (global.get $EMPTY_LIST))
    (block $done
      (loop $loop
        (br_if $done (i32.gt_s $mv (global.get $EMPTY_HASHMAP)))
        ($printf_1 \"%d,\" (i32.div_s (i32.load $mv) 32))
        (local.set $mv (i32.add $mv 12))
        (br $loop)
      )
    )
    ($print \"\\n\")
  )

  (func $PR_VALUE (param $fmt i32 $mv i32)
    (LET $temp ($pr_str $mv 1))
    ($printf_1 $fmt ($to_String $temp))
    ($RELEASE $temp)
  )

  (func $PR_MEMORY_VALUE (param $idx i32) (result i32)
    ;;; mv = mem + idx
    (LET $mv ($MalVal_ptr $idx)
         $type ($TYPE $mv)
         $size ($MalVal_size $mv)
         $val0 ($MalVal_val $idx 0))

    ($printf_2 \"%4d: type %2d\" $idx $type)

    (if (i32.eq $type 15)
      (then ($printf_1 \", size %2d\" $size))
      (else ($printf_1 \", refs %2d\" ($REFS $mv))))

    (if (OR (i32.eq $type (global.get $STRING_T))
            (i32.eq $type (global.get $SYMBOL_T)))
      ;; for strings/symbolx pointers, print hex values
      (then ($printf_2 \" [%4d|%3ds\" ($MalVal_refcnt_type $idx) $val0))
      (else ($printf_2 \" [%4d|%4d\" ($MalVal_refcnt_type $idx) $val0)))

    (if (i32.eq $size 2)
      (then
        ($print \"|----|----]\"))
      (else
        ($printf_1 \"|%4d\" ($MalVal_val $idx 1))
        (if (i32.eq $size 3)
          (then ($print \"|----]\"))
          (else ($printf_1 \"|%4d]\" ($MalVal_val $idx 2))))))

    ;;; printf(\" >> \")
    ($print \" >> \")

    (block $done (block $unknown
      (block (block (block (block (block (block (block (block
      (block (block (block (block (block (block (block (block
        (br_table 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
                  $unknown $type))
      ;; 0: nil
      ($print \"nil\")
      (br $done))
      ;; 1: boolean
      (if (i32.eq $val0 0)
        ;; true
        ($print \"false\")
        ;; false
        ($print \"true\"))
      (br $done))
      ;; 2: integer
      ($printf_1 \"%d\" $val0)
      (br $done))
      ;; 3: float/ERROR
      ($print \" *** GOT FLOAT *** \")
      (br $done))
      ;; 4: string/kw
      ($printf_1 \"'%s'\" ($to_String $mv))
      (br $done))
      ;; 5: symbol
      ($print ($to_String $mv))
      (br $done))
      ;; 6: list
      (if (i32.le_u $mv (global.get $EMPTY_HASHMAP))
        (then
          ($print \"()\"))
        (else
          ;;; printf(\"(... %d ...), next: %d\\n\", mv->val[1], mv->val[0])
          ($printf_2 \"(... %d ...), next: %d\"
                     ($MalVal_val $idx 1)
                     ($MalVal_val $idx 0))))
      (br $done))
      ;; 7: vector
      (if (i32.le_u $mv (global.get $EMPTY_HASHMAP))
        (then
          ($print \"[]\"))
        (else
          ;;; printf(\"[... %d ...], next: %d\\n\", mv->val[1], mv->val[0])val
          ($printf_2 \"[... %d ...], next: %d\"
                     ($MalVal_val $idx 1)
                     ($MalVal_val $idx 0))))
      (br $done))
      ;; 8: hashmap
      (if (i32.le_u $mv (global.get $EMPTY_HASHMAP))
        (then
          ($print \"{}\"))
        (else
          ;;; printf(\"{... '%s'(%d) : %d ...}\\n\",
          ;;         to_String(mem + mv->val[1]), mv->val[1], mv->val[2])
          ($printf_3 \"{... '%s'(%d) : %d ...}\"
                     ($to_String ($MalVal_ptr ($MalVal_val $idx 1)))
                     ($MalVal_val $idx 1)
                     ($MalVal_val $idx 2))))
      (br $done))
      ;; 9: function
      ($print \"function\")
      (br $done))
      ;; 10: mal function
      ($print \"mal function\")
      (br $done))
      ;; 11: macro fn
      ($print \"macro fn\")
      (br $done))
      ;; 12: atom
      ($print \"atom\")
      (br $done))
      ;; 13: environment
      ($print \"environment\")
      (br $done))
      ;; 14: metadata
      ($print \"metadata\")
      (br $done))
      ;; 15: FREE
      ($printf_1 \"FREE next: 0x%x\" $val0)
      (if (i32.eq $idx (global.get $mem_free_list))
        ($print \" (free start)\"))
      (if (i32.eq $val0 (global.get $mem_unused_start))
        ($print \" (free end)\"))
      (br $done))
      ;; 16: unknown
      ($print \"unknown\")
    )

    ($print \"\\n\")

    (i32.add $size $idx)
  )

  (func $PR_STRINGS (param $start i32)
    (LET $ms  0
         $idx 0)
    ($printf_2 \"String - showing %d -> %d:\\n\"
               $start (i32.sub (global.get $string_mem_next)
                                 (global.get $string_mem)))
    (if (i32.le_s (i32.sub (global.get $string_mem_next)
                             (global.get $string_mem))
                  $start)
      (then ($print \" ---\\n\"))
      (else
        (local.set $ms (global.get $string_mem))
        (block $done
          (loop $loop
            (br_if $done (i32.ge_u $ms (global.get $string_mem_next)))
            (local.set $idx (i32.sub $ms (global.get $string_mem)))
            (if (i32.ge_s $idx $start)
              ($printf_4 \"%4d: refs %2d, size %2d >> '%s'\\n\"
                         $idx
                         (i32.load16_u $ms)
                         (i32.load16_u (i32.add $ms 2))
                         (i32.add $ms 4)))

            (local.set $ms (i32.add $ms (i32.load16_u (i32.add $ms 2))))
            (br $loop)
          )
        )))
  )

  (func $PR_MEMORY (param $start i32 $end i32)
    (LET $string_start 0
         $idx          0)
    (if (i32.lt_s $start 0)
      (then
        (local.set $start (global.get $mem_user_start))
        (local.set $string_start (i32.sub (global.get $string_mem_user_start)
                                            (global.get $string_mem)))))
    (if (i32.lt_s $end 0)
      (local.set $end (global.get $mem_unused_start)))
    ;;; printf(\"Values - (mem) showing %d -> %d\", start, end)
    ;;; printf(\" (unused start: %d, free list: %d):\\n\",
    ;;;        mem_unused_start, mem_free_list)
    ($printf_4 \"Values - (mem) showing 0x%x -> 0x%x (unused start: 0x%x, free list: 0x%x):\\n\"
          $start
          $end
          (global.get $mem_unused_start)
          (global.get $mem_free_list))

    (if (i32.le_s $end $start)
      (then
        ($print \"  ---\\n\")
        (local.set $end (global.get $mem_unused_start)))
      (else
        (local.set $idx $start)
        ;;; while (idx < end)
        (block $loopvals_exit
          (loop $loopvals
            (br_if $loopvals_exit (i32.ge_s $idx $end))
            (local.set $idx ($PR_MEMORY_VALUE $idx))
            (br $loopvals)
          )
        )))
    ($PR_STRINGS $string_start)
    ($PR_MEMORY_SUMMARY_SMALL)
  )

  (func $PR_MEMORY_RAW (param $start i32 $end i32)
    (block $loop_exit
      (loop $loop
        (br_if $loop_exit (i32.ge_u $start $end))
        ($printf_2 \"0x%x 0x%x\\n\" $start (i32.load $start))
        (local.set $start (i32.add 4 $start))
        (br $loop)
      )
    )
  )
)")

(def reader
  "(module $reader

  ;; TODO: global warning
  (global $token_buf   (mut i32) 0)
  (global $read_index  (mut i32) 0)

  (func $skip_spaces (param $str i32) (result i32)
    (LET $found 0
         $c (i32.load8_u (i32.add $str (global.get $read_index))))
    (block $done
      (loop $loop
        ;;; while (c == ' ' || c == ',' || c == '\\n')
        (br_if $done (AND (i32.ne $c (CHR \" \"))
                          (i32.ne $c (CHR \",\"))
                          (i32.ne $c (CHR \"\\n\"))))
        (local.set $found 1)
        ;;; c=str[++(*index)]
        (global.set $read_index (i32.add (global.get $read_index) 1))
        (local.set $c (i32.load8_u (i32.add $str (global.get $read_index))))
        (br $loop)
      )
    )
;;    ($debug \">>> skip_spaces:\" $found)
    $found
  )

  (func $skip_to_eol (param $str i32) (result i32)
    (LET $found 0
         $c (i32.load8_u (i32.add $str (global.get $read_index))))
    (if (i32.eq $c (CHR \";\"))
      (then
        (local.set $found 1)
        (block $done
          (loop $loop
            ;;; c=str[++(*index)]
            (global.set $read_index (i32.add (global.get $read_index) 1))
            (local.set $c (i32.load8_u (i32.add $str
                                                (global.get $read_index))))
            ;;; while (c != '\\0' && c != '\\n')
            (br_if $loop (AND (i32.ne $c (CHR \"\\x00\"))
                              (i32.ne $c (CHR \"\\n\"))))
          )
        )))
;;    ($debug \">>> skip_to_eol:\" $found)
    $found
  )

  (func $skip_spaces_comments (param $str i32)
    (loop $loop
      ;; skip spaces
      (br_if $loop ($skip_spaces $str))
      ;; skip comments
      (br_if $loop ($skip_to_eol $str))
    )
  )

  (func $read_token (param $str i32) (result i32)
    (LET $token_index 0
         $instring    0
         $escaped     0
         $c           0)

    ($skip_spaces_comments $str)

    ;; read first character
    ;;; c=str[++(*index)]
    (local.set $c (i32.load8_u (i32.add $str (global.get $read_index))))
    (global.set $read_index (i32.add (global.get $read_index) 1))
    ;; read first character
    ;;; token[token_index++] = c
    (i32.store8 (i32.add (global.get $token_buf) $token_index) $c)
    (local.set $token_index (i32.add $token_index 1))
    ;; single/double character token
    (if (OR (i32.eq $c (CHR \"(\"))
            (i32.eq $c (CHR \")\"))
            (i32.eq $c (CHR \"[\"))
            (i32.eq $c (CHR \"]\"))
            (i32.eq $c (CHR \"{\"))
            (i32.eq $c (CHR \"}\"))
            (i32.eq $c (CHR \"'\"))
            (i32.eq $c (CHR \"`\"))
            (i32.eq $c (CHR \"@\"))
            (AND (i32.eq $c (CHR \"~\"))
                 (i32.ne (i32.load8_u (i32.add $str (global.get $read_index)))
                         (CHR \"@\"))))

      (then
        ;; continue
        (nop))
      (else
        ;;; if (c == '\"') instring = true
        (local.set $instring (i32.eq $c (CHR \"\\\"\")))
        (block $done
          (loop $loop
            ;; peek at next character
            ;;; c = str[*index]
            (local.set $c (i32.load8_u
                            (i32.add $str (global.get $read_index))))
            ;;; if (c == '\\0') break
            (br_if $done (i32.eq $c 0))
            ;;; if (!instring)
            (if (i32.eqz $instring)
              (then
                ;; next character is token delimiter
                (br_if $done (OR (i32.eq $c (CHR \"(\"))
                                 (i32.eq $c (CHR \")\"))
                                 (i32.eq $c (CHR \"[\"))
                                 (i32.eq $c (CHR \"]\"))
                                 (i32.eq $c (CHR \"{\"))
                                 (i32.eq $c (CHR \"}\"))
                                 (i32.eq $c (CHR \" \"))
                                 (i32.eq $c (CHR \",\"))
                                 (i32.eq $c (CHR \"\\n\"))))))
            ;; read next character
            ;;; token[token_index++] = str[(*index)++]
            (i32.store8 (i32.add (global.get $token_buf) $token_index)
                          (i32.load8_u
                            (i32.add $str (global.get $read_index))))
            (local.set $token_index (i32.add $token_index 1))
            (global.set $read_index (i32.add (global.get $read_index) 1))
            ;;; if (token[0] == '~' && token[1] == '@') break
            (br_if $done (AND (i32.eq (i32.load8_u
                                        (i32.add (global.get $token_buf) 0))
                                      (CHR \"~\"))
                              (i32.eq (i32.load8_u
                                        (i32.add (global.get $token_buf) 1))
                                      (CHR \"@\"))))

            ;;; if ((!instring) || escaped)
            (if (OR (i32.eqz $instring) $escaped)
              (then
                (local.set $escaped 0)
                (br $loop)))
            (if (i32.eq $c (CHR \"\\\\\"))
              (local.set $escaped 1))
            (br_if $done (i32.eq $c (CHR \"\\\"\")))
            (br $loop)
          )
        )))

    ;;; token[token_index] = '\\0'
    (i32.store8 (i32.add (global.get $token_buf) $token_index) 0)
    (global.get $token_buf)
  )

  (func $read_seq (param $str i32 $type i32 $end i32) (result i32)
    (LET $res ($MAP_LOOP_START $type)
         $val2 0
         $val3 0
         $c 0
         ;; MAP_LOOP stack
         $ret $res
         $empty $res
         $current $res)

    ;; READ_SEQ_LOOP
    (block $done
      (loop $loop
        ($skip_spaces_comments $str)

        ;; peek at next character
        ;;; c = str[*index]
        (local.set $c (i32.load8_u (i32.add $str (global.get $read_index))))
        (if (i32.eq $c (CHR \"\\x00\"))
          (then
            ($THROW_STR_0 \"unexpected EOF\")
            (br $done)))
        (if (i32.eq $c $end)
          (then
            ;; read next character
            ;;; c = str[(*index)++]
            (local.set $c (i32.load8_u (i32.add $str (global.get $read_index))))
            (global.set $read_index (i32.add (global.get $read_index) 1))
            (br $done)))

        ;; value (or key for hash-maps)
        (local.set $val2 ($read_form $str))

        ;; if error, release the unattached element
        (if (global.get $error_type)
          (then
            ($RELEASE $val2)
            (br $done)))

        ;; if this is a hash-map, READ_FORM again
        (if (i32.eq $type (global.get $HASHMAP_T))
          (local.set $val3 ($read_form $str)))

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

  (func $read_macro (param $str i32 $sym i32 $with_meta i32) (result i32)
    (LET $first ($STRING (global.get $SYMBOL_T) $sym)
         $second ($read_form $str)
         $third 0
         $res $second)
    (if (global.get $error_type) (return $res))
    (if (i32.eqz $with_meta)
      (then
        (local.set $res ($LIST2 $first $second)))
      (else
        (local.set $third ($read_form $str))
        (local.set $res ($LIST3 $first $third $second))
        ;; release values, list has ownership
        ($RELEASE $third)))
    ;; release values, list has ownership
    ($RELEASE $second)
    ($RELEASE $first)
    $res
  )

  (func $read_form (param $str i32) (result i32)
    (LET $tok 0 $c0 0 $c1 0 $res 0 $slen 0)

    (if (global.get $error_type) (return 0))

    (local.set $tok ($read_token $str))
    ;;($printf_1 \">>> read_form 1: %s\\n\" $tok)
    ;;; c0 = token[0]
    (local.set $c0 (i32.load8_u $tok))
    (local.set $c1 (i32.load8_u (i32.add $tok 1)))

    (if (i32.eq $c0 0)
      (then
        (return ($INC_REF (global.get $NIL))))
      (else (if (OR (AND (i32.ge_u $c0 (CHR \"0\"))
                         (i32.le_u $c0 (CHR \"9\")))
                    (AND (i32.eq $c0 (CHR \"-\"))
                         (i32.ge_u $c1 (CHR \"0\"))
                         (i32.le_u $c1 (CHR \"9\"))))
      (then
        (return ($INTEGER ($atoi $tok))))
    (else (if (i32.eq $c0 (CHR \":\"))
      (then
        (i32.store8 $tok (CHR \"\\x7f\"))
        (return ($STRING (global.get $STRING_T) $tok)))
    (else (if (i32.eq $c0 (CHR \"\\\"\"))
      (then
        (local.set $slen ($strlen (i32.add $tok 1)))
        (if (i32.ne (i32.load8_u (i32.add $tok $slen)) (CHR \"\\\"\"))
          (then
            ($THROW_STR_0 \"expected '\\\"'\")
            (return 0))
          (else
            ;; unescape backslashes, quotes, and newlines
            ;; remove the trailing quote
            (i32.store8 (i32.add $tok $slen) (CHR \"\\x00\"))
            (local.set $tok (i32.add $tok 1))
            (drop ($REPLACE3 0 $tok
                             \"\\\\\\\"\" \"\\\"\"
                             \"\\\\n\" \"\\n\"
                             \"\\\\\\\\\" \"\\\\\"))
            (return ($STRING (global.get $STRING_T) $tok)))))
    (else (if (i32.eqz ($strcmp \"nil\" $tok))
      (then (return ($INC_REF (global.get $NIL))))
    (else (if (i32.eqz ($strcmp \"false\" $tok))
      (then (return ($INC_REF (global.get $FALSE))))
    (else (if (i32.eqz ($strcmp \"true\" $tok))
      (then (return ($INC_REF (global.get $TRUE))))
    (else (if (i32.eqz ($strcmp \"'\" $tok))
      (then (return ($read_macro $str \"quote\" 0)))
    (else (if (i32.eqz ($strcmp \"`\" $tok))
      (then (return ($read_macro $str \"quasiquote\" 0)))
    (else (if (i32.eqz ($strcmp \"~@\" $tok))
      (then (return ($read_macro $str \"splice-unquote\" 0)))
    (else (if (i32.eqz ($strcmp \"~\" $tok))
      (then (return ($read_macro $str \"unquote\" 0)))
    (else (if (i32.eqz ($strcmp \"^\" $tok))
      (then (return ($read_macro $str \"with-meta\" 1)))
    (else (if (i32.eqz ($strcmp \"@\" $tok))
      (then (return ($read_macro $str \"deref\" 0)))
    (else (if (i32.eq $c0 (CHR \"(\"))
      (then (return ($read_seq $str (global.get $LIST_T) (CHR \")\"))))
    (else (if (i32.eq $c0 (CHR \"[\"))
      (then (return ($read_seq $str (global.get $VECTOR_T) (CHR \"]\"))))
    (else (if (i32.eq $c0 (CHR \"{\"))
      (then (return ($read_seq $str (global.get $HASHMAP_T) (CHR \"}\"))))
    (else (if (OR (i32.eq $c0 (CHR \")\"))
                  (i32.eq $c0 (CHR \"]\"))
                  (i32.eq $c0 (CHR \"}\")))
      (then
        ($THROW_STR_1 \"unexpected '%c'\" $c0)
        (return 0))
    (else
      (return ($STRING (global.get $SYMBOL_T) $tok))))
    ))))))))))))))))))))))))))))))))
    0 ;; not reachable
  )

  (func $read_str (param $str i32) (result i32)
    (global.set $read_index 0)
    ($read_form $str)
  )

  (export \"read_str\" (func $read_str))

)")



(def printer
  "(module $printer

  (global $printer_buf   (mut i32) 0)

  (func $pr_str_val (param $res i32 $mv i32 $print_readably i32) (result i32)
    (LET $type ($TYPE $mv)
         $val0 ($VAL0 $mv)
         $sval 0)

    ;;; switch(type)
    (block $done
      (block $default
      (block (block (block (block (block (block (block (block
      (block (block (block (block (block (block (block (block
      (br_table 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 $type))
      ;; 0: nil
      ($memmove $res \"nil\" 4)
      (local.set $res (i32.add 3 $res))
      (br $done))
      ;; 1: boolean
      (if (i32.eq $val0 0)
        (then
          ;; false
          ($memmove $res \"false\" 6)
          (local.set $res (i32.add 5 $res)))
        (else
          ;; true
          ($memmove $res \"true\" 5)
          (local.set $res (i32.add 4 $res))))
      (br $done))
      ;; 2: integer
      (local.set $res ($sprintf_1 $res \"%d\" $val0))
      (br $done))
      ;; 3: float/ERROR
      (local.set $res ($sprintf_1 $res \"%d\" \" *** GOT FLOAT *** \"))
      (br $done))
      ;; 4: string/kw
      (local.set $sval ($to_String $mv))
      (if (i32.eq (i32.load8_u $sval) (CHR \"\\x7f\"))
        (then
          (local.set $res ($sprintf_1 $res \":%s\" (i32.add $sval 1))))
      (else (if $print_readably
        (then
          ;; escape backslashes, quotes, and newlines
          (local.set $res ($sprintf_1 $res \"\\\"\" 0))
          (local.set $res (i32.add $res ($REPLACE3 $res ($to_String $mv)
                                                   \"\\\\\" \"\\\\\\\\\"
                                                   \"\\\"\" \"\\\\\\\"\"
                                                   \"\\n\" \"\\\\n\")))
          (local.set $res ($sprintf_1 $res \"\\\"\" 0)))
      (else
        (local.set $res ($sprintf_1 $res \"%s\" $sval))))))
      (br $done))
      ;; 5: symbol
      (local.set $res ($sprintf_1 $res \"%s\" ($to_String $mv)))
      (br $done))
      ;; 6: list, fallthrouogh
      )
      ;; 7: vector, fallthrough
      )
      ;; 8: hashmap
      (local.set
        $res ($sprintf_1 $res \"%c\"
                         (if (result i32) (i32.eq $type (global.get $LIST_T))
                           (then (CHR \"(\"))
                           (else (if (result i32) (i32.eq $type (global.get $VECTOR_T))
                                   (then (CHR \"[\"))
                                   (else (CHR \"{\")))))))
      ;; PR_SEQ_LOOP
      ;;; while (VAL0(mv) != 0)
      (block $done_seq
        (loop $seq_loop
          (br_if $done_seq (i32.eq ($VAL0 $mv) 0))
          ;;; res = pr_str_val(res, MEM_VAL1(mv), print_readably)
          (local.set $res ($pr_str_val $res ($MEM_VAL1_ptr $mv) $print_readably))

          ;; if this is a hash-map, print the next element
          (if (i32.eq $type (global.get $HASHMAP_T))
            (then
              ;;; res += snprintf(res, 2, \" \")
              (local.set $res ($sprintf_1 $res \" \" 0))
              (local.set $res ($pr_str_val $res ($MEM_VAL2_ptr $mv)
                                           $print_readably))))
          ;;; mv = MEM_VAL0(mv)
          (local.set $mv ($MEM_VAL0_ptr $mv))
          ;;; if (VAL0(mv) != 0)
          (if (i32.ne ($VAL0 $mv) 0)
            ;;; res += snprintf(res, 2, \" \")
            (local.set $res ($sprintf_1 $res \" \" 0)))
          (br $seq_loop)
        )
      )

      (local.set
        $res ($sprintf_1 $res \"%c\"
                         (if (result i32) (i32.eq $type (global.get $LIST_T))
                           (then (CHR \")\"))
                           (else (if (result i32) (i32.eq $type (global.get $VECTOR_T))
                                   (then (CHR \"]\"))
                                   (else (CHR \"}\")))))))
      (br $done))
      ;; 9: function
      ($memmove $res \"#<fn ...>\" 10)
      (local.set $res (i32.add 9 $res))
      (br $done))
      ;; 10: mal function
      ($memmove $res \"(fn* \" 6)
      (local.set $res (i32.add 5 $res))
      (local.set $res ($pr_str_val $res ($MEM_VAL1_ptr $mv) $print_readably))
      ($memmove $res \" \" 2)
      (local.set $res (i32.add 1 $res))
      (local.set $res ($pr_str_val $res ($MEM_VAL0_ptr $mv) $print_readably))
      ($memmove $res \")\" 2)
      (local.set $res (i32.add 1 $res))
      (br $done))
      ;; 11: macro fn
      ($memmove $res \"#<macro ...>\" 13)
      (local.set $res (i32.add 12 $res))
      (br $done))
      ;; 12: atom
      ($memmove $res \"(atom \" 7)
      (local.set $res (i32.add 6 $res))
      (local.set $res ($pr_str_val $res ($MEM_VAL0_ptr $mv) $print_readably))
      ($memmove $res \")\" 2)
      (local.set $res (i32.add 1 $res))
      (br $done))
      ;; 13: environment
      ($memmove $res \"#<mem ...>\" 11)
      (local.set $res (i32.add 10 $res))
      (br $done))
      ;; 14: metadata
      ;; recur on object itself
      (local.set $res ($pr_str_val $res ($MEM_VAL0_ptr $mv) $print_readably))
      (br $done))
      ;; 15: FREE
      ($memmove $res \"#<free ...>\" 12)
      (local.set $res (i32.add 11 $res))
      (br $done))
      ;; 16: default
      ($memmove $res \"#<unknown>\" 11)
      (local.set $res (i32.add 10 $res))
    )

    $res
  )

  (func $pr_str_internal (param $seq i32) (param $mv i32)
        (param $print_readably i32) (param $sep i32) (result i32)
    (LET $res     ($STRING_INIT (global.get $STRING_T))
         $res_str ($to_String $res))

    (if $seq
      (then
        (block $done
          (loop $loop
            (br_if $done (i32.eqz ($VAL0 $mv)))
            (local.set $res_str ($pr_str_val $res_str ($MEM_VAL1_ptr $mv) $print_readably))
            (local.set $mv ($MEM_VAL0_ptr $mv))
            (if (i32.ne ($VAL0 $mv) 0)
              (local.set $res_str ($sprintf_1 $res_str \"%s\" $sep)))
            (br $loop)
          )
        ))
      (else
        (local.set $res_str ($pr_str_val $res_str $mv $print_readably))))

    (local.set $res ($STRING_FINALIZE $res (i32.sub $res_str ($to_String $res))))

    $res
  )

  (func $pr_str (param $mv i32 $print_readably i32) (result i32)
    ($pr_str_internal 0 $mv $print_readably \"\")
  )

  (func $pr_str_seq (param $mv i32 $print_readably i32 $sep i32) (result i32)
    ($pr_str_internal 1 $mv $print_readably $sep)
  )

  (export \"pr_str\" (func $pr_str))

)")
