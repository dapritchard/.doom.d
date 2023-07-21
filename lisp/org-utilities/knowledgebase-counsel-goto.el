;;; lisp/org-utilities/knowledgebase-counsel-goto.el -*- lexical-binding: t; -*-

;; based off of `counsel-outline'

'((emacs-lisp-mode :outline-regexp ";;[;*]+[ 	]+" :outline-level counsel-outline-level-emacs-lisp)
  (org-mode :outline-title counsel-outline-title-org :action counsel-org-goto-action :history counsel-org-goto-history :caller counsel-org-goto)
  (markdown-mode :outline-title counsel-outline-title-markdown)
  (latex-mode :outline-title counsel-outline-title-latex))

(defun dp-agenda-find-outline-title ()
  (interactive)
  (message "Called from dp-agenda-find-outline-title")
  (re-search-forward ".*$")
  (match-string 0))

(defvar my-settings
  '((org-agenda-mode
     :outline-regexp "  "
     ;; :outline-level (lambda () 1)
     :outline-title dp-agenda-find-outline-title
     :path-separator ""
     )))

(defun dp-org-agenda-counsel-knowledgebase-open (candidate)
  (goto-char (cdr candidate))
  (dp-org-agenda-knowledgebase-open))

;; A copy of `counsel-outline' for development purposes
(defun f ()
  (let ((settings (cdr (assq major-mode my-settings))))
    (ivy-read "Outline: "
              (counsel-outline-candidates settings)
              :action dp-org-agenda-counsel-knowledgebase-open
              :preselect (max (1- counsel-outline--preselect) 0)
              ;; :caller 'counsel-outline
              )))
