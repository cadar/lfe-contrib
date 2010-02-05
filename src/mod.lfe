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
  (export (l2bin 1)
          (query 2)
          (comp 1)))

(defun query (mod x)
  (let-function ((err_handler 
                  (lambda (mod)
                    (let ((file (create mod)))
                      (comp file)
                      (: code load_file mod)))))
    (if (is_list mod)
      (: lists map 
        (lambda (one_mod)
          (on_all_export one_mod 
                         (: lists flatten (eval x))
                         (fun err_handler 1) 
                         'normal))
        mod)
      (on_all_export mod 
                     (: lists flatten (eval x))
                     (fun err_handler 1) 
                     'verbose))))

(defun filter (key li)
  (if (== li '()) '()
      (let* ((head (hd li))
             ((tuple status _ _ ) head))
        (if (== status key)
          (cons head (filter key (tl li)))
          (filter key (tl li))))))
(defun delete (key li)
  (if (== li '()) '()
      (let* ((head (hd li))
             ((tuple status _ _ ) head))
        (if (== status key)
          (delete key (tl li))
          (cons head (delete key (tl li)))))))

(defun on_all_export (mod fn_test handler opt)
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
           (res_li2 (delete 'crash res_li))
           (ok (filter '_ok_ res_li2))
           (un (filter 'fail res_li2)))
      (if (== (length ok) 0)
        res_li2
        res_li2))
    (catch 
      ((tuple x y z) 
       (progn
         (: io format '"ERR ~p ~p ~p~n" 
            (list x y z))
         (: timer sleep 1000)
         (if (is_function handler)
           (if (== y 'undef)
             (progn
               (apply handler (list mod))
               (on_all_export mod fn_test handler opt))
             )))))))

(defun testfn (test_fn mod func)
  (let* (((tuple fn ar) func)
        (arg (apply test_fn 
                    (list mod fn 'true))))
    (try
      (case (apply test_fn 
                 (list mod fn 'false))
        ('true (tuple '_ok_ arg  'SUCCESS))
        (fail (tuple 'fail arg (list 'quote fail) )))
      (catch 
        ((tuple m n o) 
         (tuple 'crash arg m))))))

(defun create (mod)
  (let ((file 
         (: io_lib format '"./.esrc/~s.lfe" 
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

(defun l2bin (li)
  (list_to_binary
   (: lists flatten li)))

(defun comp (mod)
  (: lfe_comp file mod 
     (list 'report 
           (tuple 'outdir '"ebin"))))
