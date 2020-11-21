;;; lisp/utils/align-regexp-variants.el -*- lexical-binding: t; -*-

(defun align-regexp-= (BEG END)
  "Like `align-regexp' but with the regex specified as '='."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)=" 1 align-default-spacing))
