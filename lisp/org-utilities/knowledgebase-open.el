;;; lisp/org-utilities/org-agenda-knowledgebase-open.el -*- lexical-binding: t; -*-

(defun dp-knowledgebase-open ()
  "Open the Org Attach attachment or follow the 'URL' property link

First check whether the current Org headline has an Org Attach
property, and if so then attempt to open the corresponding
file(s). Otherwise, attempt to follow the 'URL' property link."
  (interactive)
  (if (dp-org-check-attach)
      (org-attach-open)
    (dp-knowledgebase-follow-link)))

(defun dp-org-check-attach ()
  "Check whether the current headline has an Org Attach property"
  (let* ((tags (org-get-tags))
         (has-attach-tag (member "ATTACH" tags)))
    has-attach-tag))

(defun dp-knowledgebase-follow-link ()
  "Follow the 'URL' property link"
  (let ((link-or-url (org-entry-get nil "URL" 'selective)))
    (unless link-or-url
      (error "No 'URL' property found"))
    (org-link-open-from-string link-or-url)))

(defun dp-org-link-p (s)
  "Check whether the string `S' is an Org link"
  (unless (stringp s)
    (error "'s' must be a string"))
  (let* ((link-re "\\[\\[.+\\]\\[.+\\]\\]")
         (is-link (string-match link-re s)))
    is-link))

;; This code is based on `org-agenda-switch-to'
(defun dp-org-agenda-knowledgebase-open ()
  "Run `agenda-knowledgebase-open' from an agenda item"
  (interactive)
  (if (and org-return-follows-link
           (not (org-get-at-bol 'org-marker))
           (org-in-regexp org-link-bracket-re))
      ;; FIXME: what should be done for this codepath?
      ;; (org-link-open-from-string (match-string 0))
      (error "Not yet implemented")
    (let* ((marker (or (org-get-at-bol 'org-marker)
                       (org-agenda-error)))
           (buffer (marker-buffer marker))
           (pos (marker-position marker)))
      (unless buffer (user-error "Trying to switch to non-existent buffer"))
      (with-current-buffer buffer
        (goto-char pos)
        (dp-knowledgebase-open))
      (org-agenda-quit))))

(defun dp-define-key-org-agenda-knowledgebase-open ()
  (general-define-key
   :states 'motion
   :keymaps 'local
   "<return>" #'dp-org-agenda-knowledgebase-open))
