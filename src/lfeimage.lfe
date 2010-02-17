(defmodule lfeimage 
  (import (from db 
                (all_modules 1))
          (from lists 
                (flatten 1)
                (map 2))
          (from hlp (is_empty 1))
          (from mod (query 2)))
  (export (start_link 0)
          (init 1) 
          (handle_call 3) 
          (handle_cast 2)
          (handle_info 2) 
          (terminate 2) 
          (code_change 3)
          (init_db 1)
          (lookup 1)
          (rebeam 0)
          (recompile 1)
          (all 0)
          (all_mod 0)
          (stop 0)
          (query 1)
          (query 2))
  (behaviour gen_server))

;; Description: Lfeimage is a database
;; with stored functions.  Functions
;; are searched with unit tests.

;; Function Using version Control
;; Recovery (FUCR)
;; ----------------------------------
;; Functions have a specific format to
;; be able to store meta data. This
;; makes it possible to act on
;; failure. Functions complying to
;; this format is called 'Function
;; Using version Control Recovery'.
;; FUCR tries to recover using the
;; previous version of the stored
;; function.

;; Test Based Search (TBS)
;; ----------------------------------
;; To search the database two input
;; arguments is used in the search
;; function.  1. test arguments and
;; 2. expected result.  The output is
;; list of functions that produce a
;; result. When arguments and do not
;; match. The result of the function
;; is returnd making it easier to spot
;; the problem.

;; In short
;; --------
;; This project is a database with
;; small canonical fucr:ers.

;; Work flow using image
;; -------------------------------
;; Write your code as the function
;; existed. Error? Missing function?
;; Query the database and if nothing
;; is found a unit test and first
;; prototype will be suggested.

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

;; FUCR format
;; ----------------------------------
;; A module is in FUCR format if it
;; contains one function and one test.

(eval-when-compile
(defun lookup_fn_mod (image fun)
  (let ((res (: dets foldr 
               (lambda (a es) 
                 (let (((tuple fn _ _ _ _) a))
                   (if (== fn fun)
                     (cons a es)
                     es)))
               '() image)))
    (if (== res '())
      'missing
      (hd res))))

(defun add (x)
  (: gen_server call 'lfeimage_proc 
     (tuple 'add x)))
)

(defmacro def
  (es `(add '(defun ,@es))))


(defmacro =>
  ((e . es) 
   `(let ((modules (lookup ',e)))
      (if (is_empty modules)
        'missing
        (let (((tuple fn mod _ _ _) (hd modules)))
          (call mod ',e ,@es))))))

;; -----------------------------------
;; ------ interface to mod.lfe -------
;; -----------------------------------

(defun query (where)
  (flatten 
   (: mod query (all_mod) where)))
(defun query (module where)
  (case (: mod query module where)
    ((tuple 'error 'nofile) 'missing_module)
    ('() 'no_match)
    (res (flatten res))))

(eval-when-compile
(defun perms 
  (('()) '(()))
  ((x) 
   (lc ((<- h x)
        (<- t (perms (-- x (list h)))))
     (cons h t))))

(defun add_quote (li)
  (: lists map 
    (lambda (x) (list 'quote x))
    li))

; get_perm - create this code.
;
; (lambda (mod1 f doc)
;    (if doc ; return doc-string
;      (list '== (cons ': (cons mod1 (cons f '(2 'x2)))) 
;              ''(x x))
;      (let* ((same_arity (length '(x x)))
;       (if same_arity
;           (let* ((val (call mod1 f 2 'x))
;                  (res (== val '(x x))))
;                (if res 
;                    res
;                    val)
;           'false)))))
(defun get_perm (args out)
  (: lists map 
    (lambda (a)
      (list 'list ''lambda ''(mod1 f fn_arity doc) 
            ; If doc is true return test
            ; description.
            (list 'list ''if ''doc 
                 (list 'quote 
                 (list 'list ''==
                 (list 'cons '':
                 (list 'cons 'mod1 
                 (list 'cons 'f 
                 (list 'quote a))))
                 (list 'quote out)))

         (list 'list ''let* 
          (list 'list 
           (list 'list ''arity (cons 'list 
             (cons ''length (add_quote (add_quote (list a))))))
           (list 'list ''same_arity 
             (cons 'list (cons ''== (cons ''arity (cons ''fn_arity '()))))))
          
        (list 'list ''if ''same_arity
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
                   ''val))
         ''false))))) 
     (perms args)))
(defun ext_list(args out)
   (cons 'list 
         (cons ''list 
               (get_perm args out)))))

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

;; ---------------------------------------------
;; ------- Application code --------------------
;; ---------------------------------------------

;;---- helper ----
(defmacro rpc
  ((fn . '()) 
   `(defun ,fn ()
      (: gen_server call 'lfeimage_proc 
         (tuple ',fn)))))
(defmacro rpc1
  ((fn . '()) 
   `(defun ,fn (x)
      (: gen_server call 'lfeimage_proc 
         (tuple ',fn x)))))

;;---- interface ----
(rpc1 init_db)
(rpc rebeam)
(rpc1 lookup)
(rpc1 recompile)
(rpc all)
(rpc all_mod)
(rpc stop)


(defun start_link ()
  (: gen_server start_link
    (tuple 'local 'lfeimage_proc) 
    'lfeimage (list) (list)))

(defun init (par)
  (let ((detsdb (: db init '"image")))
    (tuple 'ok detsdb)))

(defun handle_call 
  (((tuple 'init_db image) from state)
   (let ((new_state (: db init image)))
     (tuple 'reply new_state new_state)))

  (((tuple 'rebeam) from state)
   (let ((res (: db rebeam state)))
     (tuple 'reply res  state)))

  (((tuple 'recompile fun) from state)
   (let ((res (: db recompile_mod state fun)))
     (tuple 'reply res state)))

  (((tuple 'add fun) from state)
   (let ((res (: db insert state fun)))
     (tuple 'reply res state)))

  (((tuple 'all) from state)
   (let ((res (: db all state)))
     (tuple 'reply res  state)))

  (((tuple 'all_mod) from state)
   (let ((res (: db all_modules state)))
     (tuple 'reply res  state)))

  (((tuple 'lookup fun) from state)
   (let ((res (: db lookup_fn state fun)))
     (tuple 'reply res state)))

  (((tuple 'stop) from state)
   (tuple 'stop 'normal state))

  ((req from state)
   (tuple 'reply (tuple 'missing req) state)))
(defun handle_cast (msg state) 
  (tuple 'noreply state))
(defun handle_info (info state)
  (tuple 'noreply state))
(defun terminate (reason state)
  'ok)
(defun code_change (old-vers state extra)
  (tuple 'ok state))

;;; Local Variables: ***
;;; mode:lfe ***
;;; fill-column: 38 ***
;;; comment-column:0 ***
;;; comment-start: ";; "  ***
;;; comment-end:"***" ***
;;; End: ***