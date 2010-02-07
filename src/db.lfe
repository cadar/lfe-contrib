(defmodule db
  (import 
   (from io_lib 
         (format 1))
   (from dets 
         (delete 2))
   (from hlp 
         (is_empty 1)
         (not_empty 1))
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
   (import_text 1)
   (rebeam 1)
   (rebeam1 2)
   (lookup_fn 2)
   (recompile_mod 2)
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
         (name (fun_name code))
         (prev (lookup_fn image name))
         (a (tuple name 
                   (list_to_atom ref) 
                   code 
                   (fun_arity code) 
                   prev))
         (err (rebeam1 image a)))
    (if (not_empty prev)
      (: dets delete image name))
    (: dets insert image a)
    err))


(defun lookup_fn (image fun)
  (: dets foldr 
    (lambda (a es) 
      (let (((tuple fn _ _ _ _) a))
        (if (== fn fun)
          (cons a es)
          es)))
    '() image))

(defun recompile_mod (image mod)
  (let ((a (lookup_mod image mod)))
    (if (is_empty a) 
      'missing
      (let ((err (rebeam1 image (hd a))))
        err))))

(defun lookup_mod (image mod)
  (: dets foldr 
    (lambda (a es) 
      (let (((tuple _ mod1 _ _ _) a))
        (if (== mod1 mod)
          (cons a es)
          es)))
    '() image))

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
  (let* (((tuple fn module prog _ _) a)
         (functions (branch prog))
         (imports (make_import image functions))
         (file (create module imports prog)))
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
  (let ((from_li (foldr
                  (lambda (fn as)
                    (let ((a (lookup_fn image fn)))
                      (if (is_empty a)
                        as
                        (cons (import_text a) as))))
                  '() functions)))
    (if (is_empty from_li)
      '()
      (cons 'import from_li))))

(defun import_text (a)
  (let (((tuple fn mod _ ar _)
         (hd a)))
    (list 'from mod 
          (list fn ar))))

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