(defmodule lfeimage 
  (export (start_link 0)
          (init 1) 
          (handle_call 3) (handle_cast 2)
          (handle_info 2) (terminate 2) 
          (code_change 3)
          (idefun 0)
          (li 0)
          (init 0)
          (stop 0))
  (behaviour gen_server))

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
;; Else go back.
;;
;; This project tries to be a lisp
;; image with small canonical fucr:ers.

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

;; clean up the code the same as
;; we clean up a database. Go through
;; it one after one, with help of a
;; community!

;; introduce functions 10% watch 
;; statistics, increase 20%. Smooth
;; integration.

;; there is no developing face, it is
;; only production.


;;---- helper ----
(defmacro rpc
  ((fn . '()) 
   `(defun ,fn ()
      (: gen_server call 'lfeimage_proc ',fn))))
;;---- interface ----
(rpc init)
(rpc idefun)
(rpc li)
(rpc stop)
;;---- gen_server ----
(defun start_link ()
  (: gen_server start_link
    (tuple 'local 'lfeimage_proc) 
    'lfeimage (list) (list)))

(defun init (par)
  (let ((detsdb (: db init par)))
    (tuple 'ok (tuple detsdb))))

(defun handle_call 
  (('init from state)
   (let ((new_state (init '())))
     (tuple 'reply new_state new_state)))
  (('add from state)
   (tuple 'reply 'start state))
  (('list from state)
   (tuple 'reply 'start state))
  (('stop from state)
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