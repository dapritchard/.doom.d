;;; lisp/org-utilities/org-create-inactive-timestamp-now.el -*- lexical-binding: t; -*-

(defun dp-org-create-inactive-timestamp-now ()
  "Create a string representing the current time
The string is presented in the format of an Org mode inactive timestamp"
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

;; (defun dp-org-set-inactive-timestamp-property (_)
;;   (let ((time (dp-org-create-inactive-timestamp-now)))
;;     (org-set-property "CREATION_TIMESTAMP" time)))
