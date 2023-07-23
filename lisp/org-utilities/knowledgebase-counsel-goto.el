;;; lisp/org-utilities/knowledgebase-counsel-goto.el -*- lexical-binding: t; -*-

(defun dp-counsel-knowledgebase-open ()
  "Invoke `counsel-outline' on a knowledgebase Agenda buffer."
  (interactive)
  (unless (string-equal major-mode
                        "org-agenda-mode")
    (error (concat "'dp-counsel-knowledgebase-open' should only be called in "
                   "org-agenda-mode buffers")))
  (let ((counsel-outline-settings dp-counsel-knowledgebase-settings))
    (counsel-outline)))

(defvar dp-counsel-knowledgebase-settings
  '((org-agenda-mode
    :outline-regexp "  "
    :outline-title dp-org-agenda-knowledgebase-find-outline-title
    :action dp-org-agenda-counsel-knowledgebase-open
    :path-separator ""))
  "A replacement for `counsel-outline-settings'.")

(defun dp-org-agenda-knowledgebase-find-outline-title ()
  "Return the title of a knowledgebase Org Agenda entry.

This is intended to be a suitable replacement for the
`:outline-title' value in the `counsel-outline-settings' plist,
and that typically gets used by `counsel-outline-candidates'."
  (let ((current-line (buffer-substring (point) (line-end-position))))
    (replace-regexp-in-string "[[:space:]]*\\(:[[:alnum:]_@]*::*\\)*$"
                              ""
                              current-line)))

(defun dp-knowledgebase-open-candidate (candidate)
  "Invoke `dp-org-agenda-knowledgebase-open' on the CANDIDATE entry.

This is intended to be a suitable replacement for the `:action'
value in the `counsel-outline-settings' plist, and that typically
gets used by `counsel-outline-candidates'."
  (goto-char (cdr candidate))
  (dp-org-agenda-knowledgebase-open))
