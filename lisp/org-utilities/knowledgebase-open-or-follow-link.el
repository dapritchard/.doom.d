;;; lisp/org-utilities/org-agenda-knowledgebase-open.el -*- lexical-binding: t; -*-

(defun dp-knowledgebase-open-or-follow-link ()
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
