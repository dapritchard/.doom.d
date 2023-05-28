;;; lisp/org-utilities/org-create-inactive-timestamp-now.el -*- lexical-binding: t; -*-

(defun dp-org-create-inactive-timestamp-now (_)
  ""
  (let ((time (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (org-set-property "CREATION_TIMESTAMP" time)))
