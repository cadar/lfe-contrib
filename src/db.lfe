(defmodule db
  (import 
   (from io_lib 
         (format 1))
   (from lists 
         (map 2)
         (reverse 1)
         (foreach 2) 
         (any 2) 
         (keyfind 3) 
         (member 2)))
  (export 
   (init 1)
   (rebeam 1)
   (lookup 2)
   (lookup_fn 2)
   (insert 2)
   (branch 1)
   (make_import 2)
   (all_fucr 1)
   (all 1)))


;-------- defrecord hack --------------
(eval-when-compile
  (defun def_record_funs () 
    (list '(key 0)
          '(name 0) 
          '(source 0)
          '(tags 0)
          '(user 0)
          ))
  (defun def_record_tests () 
    (list '(name 0) '(source 0)))
  (defun name_only (x)
    (if (== x '())
      '()
      (cons (hd (hd x)) 
            (name_only (tl x)))))
)

(defmacro defrecord_funs (x) 
  `(defrecord ,x ,@(def_record_funs)))
(defrecord_funs fun_rec)
(defmacro record_info_fields_funs () 
  `',(name_only (def_record_funs)))


;-------------- interface ------------
(defun init (par)
;  (: io format '"db init ~p~n" (list par))
  (let (((tuple _ detsdb)
         (: dets open_file par
            (list (tuple 'type 'set)))))
    detsdb))


(defun fun_arity (fn)
  (length (hd (tl (tl fn)))))
(defun insert (image fn) 
  (let* ((ref (make_ref))
         (name (hd (tl fn)))
         (prog (tl (tl (tl fn))))
         (functions (branch prog))
         (prev (lookup_fn image name))
         )
    (if(/= prev '())
      (: dets delete image name))
    (let* ((imports (make_import image functions))
           (file (create ref imports fn))
           (err (: mod comp file)))
      (: dets insert image 
         (tuple name (list_to_atom ref) 
                fn (fun_arity fn) prev))
;(list_to_atom ref)
;(++ '".esrc/" (++ ref '".lfe"))
'ok)))
  
(defun all (image)
  (: dets foldl 
    (lambda (a es) 
      (let (((tuple name module fn ar prev) a))
        (cons (list name module fn ar prev) es)))
     '() image))

(defun lookup (image id)
  (: dets lookup image id))


(defun make_import (image functions)
  (let* ((li (: lists foldr
               (lambda (fn as)
                 (let ((a (lookup_fn image fn)))
                   (if (== '() a)
                     as
                     (cons a as))))
               '() functions))
         (import_li
          (: lists map 
            (lambda (x) 
              (let (((tuple name mod _ ar _) (hd x)))
                (list 'from mod 
                      (list name ar))))
            li)))
    (if (/= 0 (length import_li))
      (cons 'import import_li)
      '())))

(defun lookup_fn (image fun)
  (: dets foldr (lambda (a es) 
                  (let (((tuple name module 
                                fn ar prev) a))
                    (if (== name fun)
                      (cons a es)
                      es)))
     '() image))

(defun all_fucr (image)
  (: dets foldr (lambda (a es) 
                  (let (((tuple name module 
                                fn ar prev) a))
                    (cons module es)))
     '() image))


(defun rebeam (image)
  (: os cmd '"rm .esrc/fucr*")
  (: dets foldl 
    (lambda (a es) 
      (let* (((tuple fucr _ _) a)
             (err (: mod comp 
                    (++ '"./.esrc/" fucr))))
        (cons err es)))
    '() image)
  'ok)

(defun stop (image)
  (: dets close image))
;-------------------------------------
(defun create (mod imports fn)
  (let ((file (: io_lib format '"./.esrc/~s.lfe" 
                      (list mod)))
        (fun_name (hd (tl fn)))
        (arity (length (hd (tl (tl fn)))))
        (import_str (if (== '() imports)
                      10
                      (: lists append 
                        (cons 10 
                              (cons 32 
                                    (: lfe_io print1 imports))) (list 10)))))

    (: file write_file file 
       (: mod l2bin
        (list
         (: io_lib format 
         '"(defmodule ~s" (list mod))
            import_str 
         32 (: lfe_io print1 
           (list 'export (list fun_name arity))) 
         41 10
         (: lfe_io print1 fn) 10 10
         )))
    file))


(defun make_ref ()
  (let* ( ((tuple a b c) (now))
          (nr
           (+ (* 1000000
                 (* a 1000000))
              (+ (+ c) 
                 (* b 1000000)))))
    (: lists flatten 
      (: io_lib format '"fucr~.36B" 
         (list (- nr 1263760466202795))))))

(defun branch 
  (( '() ) '()) 
  (( ((he . '()) . '())) (cons he '()))
  (( ((he . li) . '()))  (cons he (branch li)))
  (( ((he . '()) . li2)) (cons he (branch li2)))
  (( ((he . li) . li2))  
   (cons he (branch (: lists append li li2))))
  (( (he . li)  ) (branch li)))


;;; Local Variables: ***
;;; mode:lfe ***
;;; fill-column: 38 ***
;;; comment-column:0 ***
;;; comment-start: ";; "  ***
;;; comment-end:"***" ***
;;; End: ***