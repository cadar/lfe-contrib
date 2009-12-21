(defmodule matrix
  (import (from io (format 1)(format 2))
          (from lists (foreach 2) (any 2) (keyfind 3) (member 2))
          (from packages (find_modules 1))
          (from code (load_file 1)))
  (export (start 0)))

(defun try_func (mod test_fun func)
  (let (((tuple f ar) func))
    (try
      (apply test_fun 
             (list mod f))
      (catch 
        ((tuple _ n o) 1)))))

(defun on_all_export (mod test_fun)
  (let* ((mod_info (call mod 
                       'module_info))
         ((tuple 'exports funcs) (keyfind 'exports 
                                          1 
                                          mod_info)))
    (foreach (lambda (func) 
               (try_func mod 
                         test_fun 
                         func))
     funcs)))

;(defun load_m (mod test_fun) 
;    (let ((a_mod ))
;      (print_fun a_mod test_fun)))

;;       (if (not (member a_mod (list 'application 'net_kernel 'filename 'io 'gb_sets 'code 'os 'io_lib)))
;;         (let ((res (load_file a_mod)))
;;           (case res
;;             ((tuple 'module d) (print_fun d test_fun))
;;             ((tuple 'error d) (format '"module ~p is ~p" (list mod d))))))
;;       ))
  
(defun test1 (mod f)
  (let ((res (call mod f (list 1 2 3))))
    (if (== res 3)
      (format '"success ~p ~p~n" 
              (list mod f)))))

(defun start ()
  (foreach (lambda (mod) 
             (on_all_export (list_to_atom mod) 
                            (fun test1 2)))
           (list '"lists")))


