;; Query is a tool to help users 
;; find functions with the help of 
;; input argument and output.

;;
;; example:
;;
;; (query 'lists 
;;        (where 
;;     2 'x '-> '(x x)
;;     3 'x '-> '(x x x)))
;;

;; > (slurp '"mod.lfe") 
;; #(ok mod) 
;; > (query 'lists  
;;          (where  
;;           2 'x -> '(x x))) 
;; #(ok lists duplicate 2) 
;; >  

(defmodule mod
  (import (from lists 
                (keysort 2))
          (from hlp 
                (l2bin 1)
                (is_empty 1)))
  (export (query 2)
          (comp 1)))

(defun query 
  ((mod where) (when (is_list mod))
   (: lists map 
     (lambda (one_mod)
       (on_all_export_call one_mod where))
     mod))
  ((mod where)
   (on_all_export_call mod where)))

(defun on_all_export_call (mod where)
  (on_all_export mod  
                 (: lists flatten (eval where))
                 (fun err_handler 1)))

(defun on_all_export (mod fn_test handler)
  (try
    (let* ((mod_info 
            (call mod 'module_info))
           ((tuple 'exports funcs) 
            (: lists keyfind 'exports 1 
                     mod_info))
           (res_li 
            (lc ((<- te fn_test)      
                 (<- fn funcs))
              (testfn te mod fn)))
           (res_no_crash (delete 'crash res_li))
           (res_no_arity (delete 'arity res_no_crash)))
      (keysort 2 res_no_arity))
    (catch 
      ((tuple x test_res z) 
       (progn
         (: io format '"ERR ~p ~p ~p~n" 
            (list x test_res z))
         (: timer sleep 1000)
         (if (and (== test_res 'undef) 
                  (is_function handler))
           (case (apply handler (list mod)) 
             ((tuple 'module x) 
              (on_all_export mod 
                             fn_test 
                             handler))
             (res res))
           'no_handler)))
           )))

(defun err_handler (mod)
  (: lfeimage recompile mod)
  (: code load_file mod))

(defun testfn (test_fn mod func)
  (let* (((tuple fn ar) func)
         (arg (apply test_fn 
                     (list mod fn ar 'true))))
    (try
      (case (apply test_fn 
                   (list mod fn ar 'false))
        ('true 
         (tuple '_ok_ arg 'SUCCESS))
        (fail 
         (tuple 'fail arg (list 'quote fail))))
      (catch 
        ((tuple m n o) 
         (tuple 'crash arg m))))))

(defun create (mod)
  (let ((file (: io_lib format 
                '"./.esrc/~s.lfe" 
                (list mod))))
    (: file write_file file 
       (l2bin
        (list
         (: io_lib format 
           '"(defmodule ~s~n" (list mod))
         '" (export (start 2)))" 10
         (: lfe_io print1 
           '(defun start (x y)
              (: lists duplicate  x y)))
         )))
    file))
;-------------------------------
(defun comp (mod)
  (: lfe_comp file mod 
     (list 'report 
           (tuple 'outdir '"ebin"))))

(defun filter (key li)
  (if (is_empty li) '()
      (let* ((head (hd li))
             ((tuple key1 _ _ ) head))
        (if (== key1 key)
          (cons head (filter key (tl li)))
          (filter key (tl li))))))

(defun delete (key li)
  (if (is_empty li) '()
      (let* ((head (hd li))
             ((tuple key1 _ _ ) head))
        (if (== key1 key)
          (delete key (tl li))
          (cons head (delete key (tl li)))))))