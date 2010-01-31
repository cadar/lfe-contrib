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
  (export (query 2)))

(eval-when-compile
(defun perms 
  (('()) '(()))
  ((x) (lc ((<- h x)
            (<- t (perms (-- x (list h)))))
         (cons h t))))
(defun add_quote (li)
  (: lists map (lambda (x) 
                 (list 'quote x))
     li))
; (lambda (mod1 f doc)
;    (if doc ; return doc-string
;      (list '== (cons ': (cons mod1 (cons f '(2 'x2)))) 
;              ''(x x))
;      (let* ((val (call mod1 f 2 'x)) 
;             (res (== val '(x x)))) 
;       (if res 
;           res 
;           val)))))
(defun get_perm (args out)
  (: lists map 
    (lambda (a)
      (list 'list ''lambda ''(mod1 f doc) 
            ; If doc is true return test
            ; description.
            (list 'list ''if ''doc 
                 (list 'quote 
                 (list 'list ''==  
                 (list 'cons '':
                 (list 'cons 'mod1 
                 (list 'cons 'f 
                 (list 'quote a))))
                 (list 'quote  out )))

         (list 'list ''let* 
          (list 'list 
           (list 'list ''val  ; pre-calculate
            (cons 'list 
            (cons ''call 
            (cons ''mod1 
            (cons ''f (add_quote a))))))
           (list 'list ''res 
            (list 'list ''== ''val
            (list 'quote out)))) 
           (list 'list ''if ''res 
                 ''res 
                 ''val))))) 
     (perms args)))
(defun ext_list(args out)
   (cons 'list (cons ''list (get_perm args out))))
)

(defmacro where 
  (unit `(trans start ,@unit)))
(defmacro trans 
   ((e . (''-> . '())) `'(more_arg ,e))
   ((e . (''-> . (e2 . '()))) 
    `(cons ,(ext_list e e2)
           '()))
   ((e . (''-> . (e2 . es)))
    `(cons ,(ext_list e e2)
           (trans restart ,@es)))
   (('start . (e . es))
    `(cons 'list (trans (,e ) ,@es)))
   (('restart . (e . es))
    `(trans (,e ) ,@es))
   ((e . (e2 . es)) 
    `(trans (,e2 ,@e) ,@es))
   ((e . '()) `'(miss ,e))
   (e `'(error ,@e)))

(defun query (mod x)
  (let-function ((err_handler 
                  (lambda (mod)
                    (let ((file (create mod)))
                      (comp file)
                      (: code load_file mod)))))
    (on_all_export mod (: lists flatten (eval x))
                   (fun err_handler 1))))

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
        (if (/= status key)
          (cons head (delete key (tl li)))
          (delete key (tl li))))))


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
               (on_all_export mod fn_test handler))
             )))))))

(defun testfn (test_fn mod func)
  (let* (((tuple fn ar) func)
        (arg (apply test_fn 
                    (list mod fn 'true))))
    (try
      (case (apply test_fn 
                 (list mod fn 'false))
        ('true (tuple '_ok_ arg  mod))
        (fail (tuple 'fail arg (list 'quote fail) )))
      (catch 
        ((tuple m n o) 
         (tuple 'crash arg m))))))

(defun create (mod)
  (let ((file (: io_lib format '"esrc/~s.lfe" 
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


