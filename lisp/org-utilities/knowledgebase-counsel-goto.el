;;; lisp/org-utilities/knowledgebase-counsel-goto.el -*- lexical-binding: t; -*-

(defun dp-counsel-knowledgebase-open ()
  (interactive)
  (let ((counsel-outline-settings dp-counsel-knowledgebase-settings))
    (counsel-outline)))

(defvar dp-counsel-knowledgebase-settings
  '((org-agenda-mode
    :outline-regexp "  "
    :outline-title dp-agenda-find-outline-title
    :action dp-org-agenda-counsel-knowledgebase-open
    :path-separator "")))

(defun dp-agenda-find-outline-title ()
  (interactive)
  (message "Called from dp-agenda-find-outline-title")
  (let ((current-line (buffer-substring (point) (line-end-position))))
    (replace-regexp-in-string "[[:space:]]*\\(:[[:alnum:]_@]*::*\\)*$"
                              ""
                              current-line)))

(defun dp-knowledgebase-open--candidate (candidate)
  (goto-char (cdr candidate))
  (dp-org-agenda-knowledgebase-open))
