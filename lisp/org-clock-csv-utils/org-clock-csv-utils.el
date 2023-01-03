;;; lisp/org-clock-csv-utils/org-clock-csv-utils.el -*- lexical-binding: t; -*-

(defun dp-write-worklogs ()
  "Write your worklogs to ~/org/worklogs.csv, overwriting the
existing file if one exists"
  (interactive)
  (org-clock-csv-to-file (expand-file-name "~/Dev/org/worklogs.csv")))
