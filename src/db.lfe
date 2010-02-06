(defmodule db
  (import 
   (from io_lib 
         (format 1))
   (from dets 
         (delete 2))
   (from lists 
         (map 2)
         (append 2)
         (flatten 1)
         (foldr 3)
         (reverse 1)
         (foreach 2) 
         (any 2) 
         (keyfind 3) 
         (member 2)))
  (export 
   (init 1)
   (rebeam 1)
   (rebeam1 2)
   (lookup 2)
   (lookup_fn 2)
   (insert 2)
   (branch 1)
   (make_import 2)
   (all_fucr 1)
   (all 1)))

(defun init (par)
  (let (((tuple _ detsdb)
         (: dets open_file par
            (list (tuple 'type 'set)))))
    detsdb))

(defun insert (image code) 
  (let* ((ref (make_ref))
         (name (hd (tl code)))
         (prev (lookup_fn image name))
         (a (tuple name 
                   (list_to_atom ref) 
                   code 
                   (fun_arity code) 
                   prev))
         (err (rebeam1 image a)))
    (if(/= prev '())
      (: dets delete image name))
    (: dets insert image a)
    err))

(defun all (image)
  (: dets foldl 
    (lambda (a es) (cons a  es))
    '() image))

(defun all_fucr (image)
  (: dets foldr 
    (lambda (a es) 
      (let (((tuple _ mod _ _ _) a))
        (cons mod es)))
    '() image))

(defun lookup (image id)
  (: dets lookup image id))

(defun lookup_fn (image fun)
  (: dets foldr 
    (lambda (a es) 
      (let (((tuple fn _ _ _ _) a))
        (if (== fn fun)
          (cons a es)
          es)))
    '() image))

(defun rebeam (image)
  (: os cmd '"rm .esrc/fucr*")
  (: dets foldl 
    (lambda (a es) 
      (cons (rebeam1 image a) es))
    '() image))

(defun stop (image)
  (: dets close image))
;-------------------------------------
(defun rebeam1 (image a)
  (let* (((tuple fn fucr prog _ _) a)
         (functions (branch prog))
         (imports (make_import image functions))
         (file (create fucr imports prog)))
    (: mod comp file)))

(defun branch 
  (( '() ) '()) 
  (( ((he . '()) . '())) (cons he '()))
  (( ((he . li) . '()))  (cons he (branch li)))
  (( ((he . '()) . li2)) (cons he (branch li2)))
  (( ((he . li) . li2))  
   (cons he (branch (append li li2))))
  (( (he . li)  ) (branch li)))

(defun make_import (image functions)
  (let* ((li (foldr
              (lambda (fn as)
                (let ((a (lookup_fn image fn)))
                  (if (== '() a)
                    as
                    (cons a as))))
              '() functions))
         (import_li
          (map 
           (lambda (x) 
             (let (((tuple fn mod _ ar _) 
                    (hd x)))
               (list 'from mod 
                     (list fn ar))))
           li)))
    (if (== 0 (length import_li))
      '()
      (cons 'import import_li))))

(defun create (mod imports prog)
  (let ((file (: io_lib format 
                '"./.esrc/~s.lfe" 
                (list mod))))
    (: file write_file file 
       (: mod l2bin
         (list
          (: io_lib format 
            '"(defmodule ~s" (list mod))
          (create_import imports)
          32 
          (create_export prog)
          41 10
          (: lfe_io print1 prog) 
          10 10
          )))
    file))

(defun create_import (imports)
  (if (== '() imports)
    10
    (append 
     (cons 10 
           (cons 32 
                 (: lfe_io print1 imports))) 
     (list 10))))

(defun create_export (prog)
  (: lfe_io print1 
    (list 'export 
          (list (fun_name prog) 
                (fun_arity prog)))))

(defun make_ref ()
  (let* ( ((tuple a b c) (now))
          (nr
           (+ (* 1000000
                 (* a 1000000))
              (+ (+ c) 
                 (* b 1000000)))))
    (flatten 
     (: io_lib format '"fucr~.36B" 
        (list (- nr 1263760466202795))))))



(defun fun_arity (code)
  (length (hd (tl (tl code)))))
(defun fun_body (code)
  (tl (tl (tl code))))
(defun fun_name (code)
  (hd (tl code)))





;;; Local Variables: ***
;;; mode:lfe ***
;;; fill-column: 38 ***
;;; comment-column:0 ***
;;; comment-start: ";; "  ***
;;; comment-end:"***" ***
;;; End: ***