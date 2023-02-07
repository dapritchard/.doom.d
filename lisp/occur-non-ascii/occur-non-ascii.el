;;; lisp/occur-non-ascii/occur-non-ascii.el -*- lexical-binding: t; -*-

;; From https://www.emacswiki.org/emacs/FindingNonAsciiCharacters
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))
