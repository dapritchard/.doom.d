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
    :action dp-knowledgebase-open-candidate
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

(defun dp-define-key-counsel-knowledgebase-open ()
  "Overrides the `delete-other-windows'"
  (general-define-key
   :states 'motion
   :keymaps 'local
   "o" #'dp-counsel-knowledgebase-open))

(defun dp-knowledgebase-read ()
  "Search the knowledgebase"
  (interactive)
  (let* ((knowledgebase-path (file-name-concat org-roam-directory
                                               "20230619210159-knowledgebase.org"))
         (org-agenda-files (list knowledgebase-path))
         (org-agenda-custom-commands dp-agenda-custom-commands-knowledgebase))
    (save-window-excursion
      (org-agenda nil "f"))
    (with-current-buffer "*Org Agenda*"
        (dp-counsel-knowledgebase-open))))
