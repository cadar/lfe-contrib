(defmodule hlp
  (export 
   (is_empty 1)
   (not_empty 1)))

(defun is_empty (x) (== x '()))
(defun not_empty (x) (/= x '()))
