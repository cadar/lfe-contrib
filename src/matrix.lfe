(defmodule matrix
  (import (from io (format 1)(format 2))
          (from lists (foreach 2) (any 2) (keyfind 3) (member 2))
          (from packages (find_modules 1))
          (from code (load_file 1)))
  (export (start 0) (init 0)))

(defrecord source (func 0) (source 0))

(defun try_func (mod test_fun func)
  (let (((tuple f ar) func))
    (try
      (apply test_fun 
             (list mod f))
      (catch 
        ((tuple _ n o) 1)))))

(defun create (mod)
  (let ((file (: io_lib format '"tmp/~s.lfe" (list mod))))
    (format '"~p~n" (list file))
    (: file write_file file (list_to_binary 
                             (: lists flatten 
                               (list
                                (: io_lib format '"(defmodule ~s~n" (list mod))
                                (: io_lib format '" (export (start 0)))~n" (list))
                                '"(defun start () 14)" 
                                )
                               )))
    file))

(defun comp (mod)
  (: lfe_comp file mod (list 'report (tuple 'outdir '"ebin"))))

(defun on_all_export (mod test_fun)
  (try
    (let* ((mod_info (call mod 'module_info))
           ((tuple 'exports funcs) (keyfind 'exports 1 mod_info)))
      (foreach (lambda (func) 
                 (try_func mod test_fun func))
               funcs))
    (catch 
      ((tuple x y z) 
       (format '"big error ~p ~p ~p~n" (list x y z))
       (let ((file (create mod)))
         (comp file)
         (load_file mod)
         (on_all_export mod test_fun))))))

(defun test1 (mod f)
  (let ((res (call mod f)))
    (if (== res 14)
      (format '"success ~p ~p~n" 
              (list mod f)))))

(defun init ()
  (: mnesia create_schema (list (: erlang node))) 
  (: mnesia start)
  (: mnesia create_table 'source (list (tuple 'attributes (: erlang record_info 'fields 'source)))))

  
(defun start ()
  (: mnesia start)
  (foreach (lambda (mod) 
             (on_all_export (list_to_atom mod) (fun test1 2)))
           (list '"lists" '"aaa123")))


