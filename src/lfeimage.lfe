(defmodule lfeimage 
  (import 
   (from io_lib 
         (format 1)
         (format 2)) 
   (from lists 
         (map 2)
         (reverse 1)
         (foreach 2) 
         (any 2) 
         (keyfind 3) 
         (member 2)) 
   (from code 
         (load_file 1))) 
  (export 
   (start 0) 
   (init 0)))

;; Description: Image is a database
;; with stored functions(FUCR). The
;; word image is used since it is
;; modelled after lisp images.
;; Functions are searched with help of
;; unit tests (TBS).


;; Test Based Search (TBS)
;; -------------------------------
;;
;; Functions Using version Control 
;; Recovery (FUCR)
;; -------------------------------
;; Functions have a specific format to
;; be able to store more meta
;; data. This makes it possible to act
;; on failure.  Functions complying to
;; this format is called 'Functions
;; Using version Control Recovery'
;; (FUCR). Fucr tries to recover using
;; next or previos version of the
;; stored function. The most
;; finegranded vc strategy so far.
;;
;; If failure, try my tests on next
;; version. If success, use.
;; Else go back in version 
;; until success.
;;
;; This project tries to be a lisp
;; image containing small fucr:ers.

;; Work flow using image
;; ---------------------
;; Write your code as the function
;; existed. Error?  Query the database
;; and if nothing is found Dbfun will
;; write the first prototype for you.
;; That will keep you coding, don't
;; stop.  Then when done, write more
;; toplevel unit tests.  Watch the low
;; level fail. Write more lowlevel
;; test. Then go back up. More hi
;; level tests.

;; Script example.
;; 1. I need ([1],[5])=[1,5]
;; 2. Query db, what is  
;;       X([1],[5])=[1,5]
;; 3. (q '(1) '(5) '-> '(1 5))
;;    Usage: (q in-args '-> out)
;;
;; 3a. no fun in db create with:
;;     (insert 
;;       (defun XXX (x y) 
;;     ;; <Description>
;;       (list 1 2 3 4 5))
;;     -> (id 'aab234)
;;  > (: aab234 XXX 23 4)
;;  
;;  > (list 
;;  > (trace
;;  > (untrace
;;  > (delete
;;  > (ed
;;  > (test 'aab124) 
;;  > (q '(1) '(5) '-> '(1 5))
;;  > (q '('(1) '(5) '(1 5))
;;       '('(1) '(5) '(1 5))
;;       )
;;  > (q '(1) '(5) '-> '(1 5) 'lists)
;;    searches list module
;;
;; 3b  append will do that.
;; 
;; FUCR format
;; -----------
;; Default module name 'image'
;; From image we can find all used
;; code with help of export at the
;; top. Test are stored as 
;; normal functions. It must be 
;; called test to ne recognized 
;; ass a test function for the 
;; module.

;-------- defrecord hack --------
(eval-when-compile
  (defun def_record_funs () 
    (list '(name 0) 
          '(source 0)
          '(tags 0)
          '(user 0)
          ))
  (defun def_record_tests () 
    (list '(name 0) '(source 0)))
  (defun def_record_log () 
    (list '(name 0) '(source 0)))
  (defun def_record_users () 
    (list '(name 0) '(source 0)))
  (defun name_only (x)
    (if (== x '())
      '()
      (cons (hd (hd x)) 
            (name_only (tl x)))))
)

(defmacro defrecord_funs (x) 
  `(defrecord ,x ,@(def_record_funs)))
(defrecord_funs funs)
(defmacro record_info_fields_funs () 
  `',(name_only (def_record_funs)))

(defmacro defrecord_log (x) 
  `(defrecord ,x ,@(def_record_log)))
(defrecord_log log)
(defmacro record_info_fields_log () 
  `',(name_only (def_record_log)))


(defmacro defrecord_tests (x) 
  `(defrecord ,x ,@(def_record_tests)))
(defrecord_tests tests)
(defmacro record_info_fields_tests () 
  `',(name_only (def_record_tests)))

;-------------------------------------

(defun testfn (test_fn mod func)
  (try
    (let* (((tuple fn ar) func)
           (res 
            (apply test_fn 
                   (list mod fn ar))))
      (if res 
        (: io format '"~p:~p/~p~n" 
           (list mod fn ar))))
    (catch 
      ((tuple _ n o) (tuple n)))))

(defun l2bin (li)
  (list_to_binary
   (: lists flatten li)))

(defun create (mod)
  ;; get fun from mnesia
  ;; create file
  ;; compile
  ;; test
  (let ((file (format '"esrc/~s.lfe" 
                      (list mod))))
    (: file write_file file 
       (l2bin
        (list
         (format '"(defmodule ~s~n" 
                 (list mod))
  '" (export (start 2)))" 10
  '"(defun start (x y) " 10 
  '";; main function" 10
  '"(: io format '\"~p ~p.\"" 10
  '"(list x y))" 10 
  '"(: lists append x y)" 10 
  '")" 10
         )))
    file))

(defun comp (mod)
  (: lfe_comp file mod 
     (list 'report 
           (tuple 'outdir '"ebin"))))

(defun on_all_export (mod fn_test)
  (try
    (let* ((mod_info 
            (call mod 'module_info))
           ((tuple 'exports funcs) 
            (keyfind 'exports 1 
                     mod_info)))
      (map
        (lambda (fn) 
          (testfn fn_test mod fn))
        funcs))
    (catch 
      ((tuple x y z) 
       (: io format '"ERR ~p ~p ~p~n" 
          (list x y z))
       (let ((file (create mod)))
         (comp file)
         (load_file mod)
         (on_all_export mod fn_test))))))
;;------------------------------------

(defun query (mod x)
  (on_all_export mod (eval x)))

(defmacro where 
  (unit `(tran start ,@unit)))
(defmacro tran 
   ((e . ('-> . '())) `'(more_arg ,e))
   ((e . ('-> . (e2 . '()))) 
   `(cons (list '== 
                '(call mod f 
                   ,@(reverse e)) 
                ',e2) 
          '()))
   ((e . ('-> . (e2 . es)))
   `(cons (list '== 
                '(call mod f 
                   ,@(reverse e)) 
                ',e2) 
          (tran restart ,@es)))
   (('start . (e . es))
    `(list 'lambda '(mod f arity) 
           (cons 'andalso 
                 (tran (,e ) ,@es))))
   (('restart . (e . es))
    `(tran (,e )     ,@es))
   ((e . (e2 . es)) 
    `(tran (,e2 ,@e) ,@es))
   ((e . '()) `'(miss ,e))
   (e `'(error ,@e)))

;;------------------------------------
(defun init ()
  (: mnesia create_schema 
    (list (: erlang node))) 
  (: mnesia start)
  (: mnesia create_table 'funs 
     (list 
      (tuple 'disc_copies 
             (list (: erlang node)))
      (tuple 'attributes 
            (record_info_fields_funs)))))

(defun qq (ast)
  (on_all_export 
   'lists 
   (lambda (mod f arity)
     (andalso 
      (== (call mod f '(1) '(3)) '(1 3))
      (== (call mod f '(4) '(5)) '(4 5))))))

(defun start ()
    (: mnesia start)
    (foreach (lambda (mod) 
               (qq mod)
               )
             (list 'lists 'aaa123)))



;;; Local Variables: ***
;;; mode:lfe ***
;;; fill-column: 38 ***
;;; comment-column:0 ***
;;; comment-start: ";; "  ***
;;; comment-end:"***" ***
;;; End: ***