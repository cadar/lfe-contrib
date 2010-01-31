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
         ))

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
(defmacro def
  (x `(insert '(defun ,@x))))
;-------------------------------------

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

(defun insert (x)
  (flet ((write_fun1 () 
                     (let* ((k (make_ref))
                            (r (make-fun_rec
                                key k 
                                name x)))
                       (: mnesia write r)
                       k)))
    (: mnesia transaction (fun write_fun1 0))))

(defun get (x)
  (flet ((get_fun1 () 
                   (: mnesia read 
                     (tuple 'fun_rec x))))
    (: mnesia transaction (fun get_fun1 0))))



(defun init (par)
  (: io format '"db init ~p~n" (list par))
  (let (((tuple _ detsdb)
         (: dets open_file '"image" 
            (list))))
    detsdb))


;;; Local Variables: ***
;;; mode:lfe ***
;;; fill-column: 38 ***
;;; comment-column:0 ***
;;; comment-start: ";; "  ***
;;; comment-end:"***" ***
;;; End: ***