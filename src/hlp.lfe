(defmodule hlp
  (export 
   (l2bin 1)
   (is_empty 1)
   (not_empty 1)))

(defun is_empty (x) (== x '()))
(defun not_empty (x) (/= x '()))
(defun l2bin (li)
  (list_to_binary
   (: lists flatten li)))

