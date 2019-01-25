(ns malhala.zero)

(def platform-os
  ";; Copyright Joel Martin <github@martintribe.org>
;; Licensed under MPL-2.0 (see ./LICENSE)
;; https://github.com/kanaka/wam

(module $platform_os

  (import \"env\" \"exit\"        (func $lib_exit (param i32)))
  (import \"env\" \"stdout\"      (global $lib_stdout i32))
  (import \"env\" \"fputs\"       (func $lib_fputs (param i32 i32) (result i32)))
  (import \"env\" \"readline\"    (func $lib_readline (param i32 i32 i32) (result i32)))
  (import \"env\" \"read_file\"   (func $lib_read_file (param i32 i32) (result i32)))
  (import \"env\" \"get_time_ms\" (func $lib_get_time_ms (result i32)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (func $fatal (param $code i32 $msg i32)
    ($print $msg)
    ($lib_exit $code)
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (func $print (param $addr i32)
    (drop ($lib_fputs $addr (global.get $lib_stdout))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (func $readline (param $prompt i32 $buf i32) (result i32)
    ;; TODO: don't hardcode count to 200
    (LET $res ($lib_readline $prompt $buf 20000))
    $res
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (func $read_file (param $path i32 $buf i32) (result i32)
    (LET $size ($lib_read_file $path $buf))
    ;; Add null to string
    (i32.store8 (i32.add $buf $size) 0)
    (i32.add $size 1)
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (func $get_time_ms (result i32)
    ($lib_get_time_ms)
  )

)")

(def string
  ";; Copyright Joel Martin <github@martintribe.org>
;; Licensed under MPL-2.0 (see ./LICENSE)
;; https://github.com/kanaka/wam

(module $string

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Copy len bytes from src to dst
  ;; Returns len
  (func $memmove  (param $dst i32 $src i32 $len i32)
    (LET $idx 0)
    (loop $copy
          (i32.store8 (i32.add $idx $dst)
                        (i32.load8_u (i32.add $idx $src)))
      (local.set $idx (i32.add 1 $idx))
      (br_if $copy (i32.lt_u $idx $len))
    )
  )

  (func $strlen (param $str i32) (result i32)
    (LET $cur $str)
    (loop $count
      (if (i32.ne 0 (i32.load8_u $cur))
        (then
          (local.set $cur (i32.add $cur 1))
          (br $count)))
    )
    (i32.sub $cur $str)
  )

  ;; This could be made much more efficient
  (func $strstr (param $haystack i32 $needle i32) (result i32)
    (LET $i          0
         $needle_len ($strlen $needle)
         $len        ($strlen $haystack))

    (if (i32.eq $needle_len 0) (return $haystack))

    (local.set $i 0)
    (block $done
      (loop $loop
        (if (i32.gt_s $i (i32.sub $len $needle_len)) (br $done))

        (if (AND (i32.eq (i32.load8_u $haystack)
                         (i32.load8_u $needle))
                 (i32.eqz ($strncmp $haystack $needle $needle_len)))
          (return $haystack))
        (local.set $haystack (i32.add $haystack 1))
        (local.set $i (i32.add $i 1))
        (br $loop)
      )
    )
    0
  )

  (func $atoi (param $str i32) (result i32)
    (LET $acc 0
         $i   0
         $neg 0
         $ch  0)
    (block $done
      (loop $loop
        (local.set $ch (i32.load8_u (i32.add $str $i)))
        (if (AND (i32.ne $ch (CHR \"-\"))
                 (OR (i32.lt_u $ch (CHR \"0\"))
                     (i32.gt_u $ch (CHR \"9\"))))
          (br $done))
        (local.set $i (i32.add $i 1))
        (if (i32.eq $ch (CHR \"-\"))
          (then
            (local.set $neg 1))
          (else
            (local.set $acc (i32.add (i32.mul $acc 10)
                                     (i32.sub $ch (CHR \"0\"))))))
        (br $loop)
      )
    )
    (if (result i32) $neg
      (then (i32.sub 0 $acc))
      (else $acc))
  )

  (func $strcmp (param $s1 i32 $s2 i32) (result i32)
    (block $done
      (loop $loop
        (if (OR (i32.eqz (i32.load8_u $s1)) (i32.eqz (i32.load8_u $s2)))
          (br $done))
        (if (i32.ne (i32.load8_u $s1) (i32.load8_u $s2))
          (br $done))
        (local.set $s1 (i32.add $s1 1))
        (local.set $s2 (i32.add $s2 1))
        (br $loop)
      )
    )
    (if (result i32) (i32.eq (i32.load8_u $s1) (i32.load8_u $s2))
      (then 0)
      (else
        (if (result i32) (i32.lt_u (i32.load8_u $s1) (i32.load8_u $s2))
          (then -1)
          (else 1))))
  )

  (func $strncmp (param $s1 i32 $s2 i32 $len i32) (result i32)
    (LET $i 0)
    (if (i32.eq $len 0) (return 0))
    (block $done
      (loop $loop
        (if (i32.ge_u $i $len) (br $done))
        (if (i32.eqz (i32.load8_u (i32.add $i $s1))) (br $done))
        (if (i32.ne (i32.load8_u (i32.add $i $s1))
                    (i32.load8_u (i32.add $i $s2))) (br $done))
        (local.set $i (i32.add $i 1))
        (br $loop)
      )
    )
    (if (OR (i32.eq $i $len)
            (i32.eq (i32.load8_u (i32.add $i $s1))
                    (i32.load8_u (i32.add $i $s2))))
      (return 0))
    (if (result i32) (i32.lt_u (i32.load8_u (i32.add $i $s1))
                      (i32.load8_u (i32.add $i $s2)))
      (then -1)
      (else 1))
  )

  ;; Writes new string to grass with all needles in haystack replaced.
  ;; If the length of replace is equal to of less than needle then
  ;; grass can be NULL.
  ;; Returns length of grass.
  (func $REPLACE3 (param $grass i32 $haystack i32
                         $needle0 i32 $replace0 i32
                         $needle1 i32 $replace1 i32
                         $needle2 i32 $replace2 i32) (result i32)
    (LET $haystack_len ($strlen $haystack)
         $src_str      $haystack
         $dst_str      $grass
         $s 0 $found_tmp 0 $found 0
         $needle 0 $replace 0 $needle_len 0 $replace_len 0
         $replace_s 0 $replace_len_s 0 $needle_len_s 0)

    ;; in-place
    (if (i32.eqz $grass)
      (then
        ;; check that we aren't expanding in place
        (local.set $s 0)
        (block $done
          (loop $loop
            (if (i32.ge_u $s 3) (br $done))
            (local.set $needle (if (result i32) (i32.eq $s 0)   $needle0
                                 (if (result i32) (i32.eq $s 1) $needle1
                                                       $needle2)))
            (local.set $replace (if (result i32) (i32.eq $s 0)   $replace0
                                  (if (result i32) (i32.eq $s 1) $replace1
                                                        $replace2)))
            (local.set $needle_len ($strlen $needle))
            (local.set $replace_len ($strlen $replace))
            (if (i32.gt_u $replace_len $needle_len)
              ($fatal 7 \"REPLACE: invalid expanding in-place call\\n\"))
            (local.set $s (i32.add $s 1))
            (br $loop)
          )
        )
        (local.set $grass $haystack)
        (local.set $dst_str $grass)))

    (block $done1
      (loop $loop1
        (if (i32.ge_s (i32.sub $src_str $haystack) $haystack_len)
          (br $done1))

        ;; Find the earliest match
        (local.set $found 0)
        (local.set $s 0)
        (block $done2
          (loop $loop2
            (if (i32.ge_u $s 3) (br $done2))
            (local.set $needle (if (result i32) (i32.eq $s 0)   $needle0
                                 (if (result i32) (i32.eq $s 1) $needle1
                                                       $needle2)))
            (local.set $replace (if (result i32) (i32.eq $s 0)   $replace0
                                  (if (result i32) (i32.eq $s 1) $replace1
                                                        $replace2)))
            (local.set $s (i32.add $s 1))
            (local.set $found_tmp ($strstr $src_str $needle))
            (if (i32.eqz $found_tmp) (br $loop2))
            (if (OR (i32.eqz $found) (i32.lt_s $found_tmp $found))
              (then
                (local.set $found $found_tmp)
                (local.set $needle_len_s ($strlen $needle))
                (local.set $replace_s $replace)
                (local.set $replace_len_s ($strlen $replace))))
            (br $loop2)
          )
        )
        (if (i32.eqz $found) (br $done1))
        ;; copy before the match
        ($memmove $dst_str $src_str (i32.add (i32.sub $found $src_str) 1))
        (local.set $dst_str (i32.add $dst_str (i32.sub $found $src_str)))
        ;; add the replace string
        ($memmove $dst_str $replace_s (i32.add $replace_len_s 1))
        (local.set $dst_str (i32.add $dst_str $replace_len_s))
        ;; Move to after the match
        (local.set $src_str (i32.add $found $needle_len_s))
        (br $loop1)
      )
    )

    ;; Copy the left-over
    ($memmove $dst_str $src_str ($strlen $src_str))
    (local.set $dst_str (i32.add $dst_str ($strlen $src_str)))
    (i32.store8 $dst_str (CHR \"\\x00\"))

    (i32.sub $dst_str $grass)
  )

)")

(def printf
  ";; Copyright Joel Martin <github@martintribe.org>
;; Licensed under MPL-2.0 (see ./LICENSE)
;; https://github.com/kanaka/wam

(module $printf

  (global $printf_buf (mut i32) 0)

  (func $init_printf_mem
    ;; sprintf static buffer
    (global.set $printf_buf (STATIC_ARRAY 256))
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (func $printf_1 (param $fmt i32) (param $v0 i32)
    (drop ($sprintf_6 (global.get $printf_buf) $fmt $v0 0 0 0 0 0))
    ($print (global.get $printf_buf))
  )

  (func $printf_2 (param $fmt i32 $v0 i32 $v1 i32)
    (drop ($sprintf_6 (global.get $printf_buf) $fmt $v0 $v1 0 0 0 0))
    ($print (global.get $printf_buf))
  )

  (func $printf_3 (param $fmt i32)
        (param $v0 i32) (param $v1 i32) (param $v2 i32)
    (drop ($sprintf_6 (global.get $printf_buf) $fmt $v0 $v1 $v2 0 0 0))
    ($print (global.get $printf_buf))
  )

  (func $printf_4 (param $fmt i32)
        (param $v0 i32) (param $v1 i32) (param $v2 i32)
        (param $v3 i32)
    (drop ($sprintf_6 (global.get $printf_buf) $fmt $v0 $v1 $v2 $v3 0 0))
    ($print (global.get $printf_buf))
  )

  (func $printf_5 (param $fmt i32)
        (param $v0 i32) (param $v1 i32) (param $v2 i32)
        (param $v3 i32) (param $v4 i32)
    (drop ($sprintf_6 (global.get $printf_buf) $fmt $v0 $v1 $v2 $v3 $v4 0))
    ($print (global.get $printf_buf))
  )

  (func $printf_6 (param $fmt i32)
        (param $v0 i32) (param $v1 i32) (param $v2 i32)
        (param $v3 i32) (param $v4 i32) (param $v5 i32)
    (drop ($sprintf_6 (global.get $printf_buf) $fmt $v0 $v1 $v2 $v3 $v4 $v5))
    ($print (global.get $printf_buf))
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (func $_sprintdigit (param $str i32) (param $num i32) (param $base i32)
    (LET $n  (i32.rem_u $num $base)
         $ch (if (result i32) (i32.lt_u $n 10) 48 55))
    (i32.store8 $str (i32.add $n $ch))
  )

  ;; TODO: add max buf length (i.e. snprintnum)
  (func $_sprintnum (param $buf i32) (param $val i32) (param $radix i32)
        (param $pad_cnt i32) (param $pad_char i32) (result i32)
    (LET $pbuf  $buf
         $neg 0 $i 0 $j 0 $k 0 $len 0 $digit 0)

    (if (AND (i32.lt_s $val 0) (i32.eq $radix 10))
      (then
        (local.set $neg 1)
        (local.set $val (i32.sub 0 $val))))

    ;; Calculate smallest to most significant digit
    (loop $loop
      (local.set $digit (i32.rem_u $val $radix))
      (i32.store8 $pbuf (if (result i32) (i32.lt_u $digit 10)
                            (i32.add (CHR \"0\") $digit)
                            (i32.sub (i32.add (CHR \"A\") $digit) 10)))
      (local.set $pbuf (i32.add $pbuf 1))
      (local.set $val (i32.div_u $val $radix))
      (br_if $loop (i32.gt_u $val 0))
    )

    (local.set $i (i32.sub $pbuf $buf))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_u $i $pad_cnt))
        (i32.store8 $pbuf $pad_char)
        (local.set $pbuf (i32.add $pbuf 1))
        (local.set $i (i32.add $i 1))
        (br $loop)
      )
    )

    (if $neg
      (then
        (i32.store8 $pbuf (CHR \"-\"))
        (local.set $pbuf (i32.add $pbuf 1))))

    (i32.store8 $pbuf (CHR \"\\x00\"))

    ;; now reverse it
    (local.set $len (i32.sub $pbuf $buf))
    (local.set $i 0)
    (block $done
      (loop $loop
        (br_if $done (i32.ge_u $i (i32.div_u $len 2)))

        (local.set $j (i32.load8_u (i32.add $buf $i)))
        (local.set $k (i32.add $buf (i32.sub (i32.sub $len $i) 1)))
        (i32.store8 (i32.add $buf $i) (i32.load8_u $k))
        (i32.store8 $k $j)
        (local.set $i (i32.add $i 1))
        (br $loop)
      )
    )

    (i32.add $buf $len)
  )

  ;; TODO: switch to snprint* (add buffer len)
  (func $sprintf_1 (param $str i32) (param $fmt i32)
        (param $v0 i32) (result i32)
    ($sprintf_6 $str $fmt $v0 0 0 0 0 0)
  )

  (func $sprintf_6 (param $str i32) (param $fmt i32)
        (param $v0 i32) (param $v1 i32) (param $v2 i32)
        (param $v3 i32) (param $v4 i32) (param $v5 i32)
        (result i32)
    (LET $pstr $str
         $vidx 0 $ch 0 $v 0 $len 0 $pad_cnt 0 $pad_char 0)

    (block $done
      (loop $loop
        (block $after_v
          ;; set $v to the current parameter
          (block (block (block (block (block (block
          (br_table 0 1 2 3 4 5 0 $vidx))
          (; 0 ;) (local.set $v $v0) (br $after_v))
          (; 1 ;) (local.set $v $v1) (br $after_v))
          (; 2 ;) (local.set $v $v2) (br $after_v))
          (; 3 ;) (local.set $v $v3) (br $after_v))
          (; 4 ;) (local.set $v $v4) (br $after_v))
          (; 5 ;) (local.set $v $v5) (br $after_v)
        )

        ;;; while ((ch=*(fmt++)))
        (local.set $ch (i32.load8_u $fmt))
        (local.set $fmt (i32.add 1 $fmt))
        (br_if $done (i32.eqz $ch))
        ;; TODO: check buffer length

        (if (i32.ne $ch (CHR \"%\"))
          (then
            ;; TODO: check buffer length
            (i32.store8 $pstr $ch)
            (local.set $pstr (i32.add 1 $pstr))
            (br $loop)))

        ;;; ch=*(fmt++)
        (local.set $ch (i32.load8_u $fmt))
        (local.set $fmt (i32.add 1 $fmt))
        (br_if $done (i32.eqz $ch))

        (local.set $pad_cnt 0)
        (local.set $pad_char (CHR \" \"))
        (if (AND (i32.ge_s $ch (CHR \"0\")) (i32.le_s $ch (CHR \"9\")))
          (then
            ;; padding requested
            (if (i32.eq $ch (CHR \"0\"))
              (then
                ;; zero padding requested
                (local.set $pad_char (CHR \"0\"))
                ;;; ch=*(fmt++)
                (local.set $ch (i32.load8_u $fmt))
                (local.set $fmt (i32.add 1 $fmt))
                (br_if $done (i32.eqz $ch))))
            (loop $loop
              (local.set $pad_cnt (i32.mul $pad_cnt 10))
              (local.set $pad_cnt (i32.add $pad_cnt
                                           (i32.sub $ch (CHR \"0\"))))
              (local.set $ch (i32.load8_u $fmt))
              (local.set $fmt (i32.add 1 $fmt))
              (br_if $loop (AND (i32.ge_s $ch (CHR \"0\"))
                                (i32.le_s $ch (CHR \"9\"))))
            )))

        (if (i32.eq (CHR \"d\") $ch)
          (then
            (local.set $pstr ($_sprintnum $pstr $v 10 $pad_cnt $pad_char)))
        (else (if (i32.eq (CHR \"x\") $ch)
          (then
            (local.set $pstr ($_sprintnum $pstr $v 16 $pad_cnt $pad_char)))
        (else (if (i32.eq (CHR \"s\") $ch)
          (then
            (local.set $len ($strlen $v))
            (block $done
              (loop $loop
                (br_if $done (i32.le_s $pad_cnt $len))
                (i32.store8 $pstr (CHR \" \"))
                (local.set $pstr (i32.add $pstr 1))
                (local.set $pad_cnt (i32.sub $pad_cnt 1))
                (br $loop)
              )
            )
            ($memmove $pstr $v $len)
            (local.set $pstr (i32.add $pstr $len)))
        (else (if (i32.eq (CHR \"c\") $ch)
          (then
            (i32.store8 $pstr $v)
            (local.set $pstr (i32.add $pstr 1)))
        (else (if (i32.eq (CHR \"%\") $ch)
          (then
            (i32.store8 $pstr (CHR \"%\"))
            (local.set $pstr (i32.add $pstr 1))
            (br $loop)) ;; don't increase vidx
        (else
          ($printf_1 \"Illegal format character: '%c'\\n\" $ch)
          ($fatal 3 \"\")))))))))))

        (local.set $vidx (i32.add 1 $vidx))
        (br $loop)
      )
    )

    (i32.store8 $pstr (CHR \"\\x00\"))
    $pstr
  )

)")
