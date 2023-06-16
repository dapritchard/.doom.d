;;; lisp/org-utilities/org-external-link-sharepoint.el -*- lexical-binding: t; -*-

;; https://targetpharmasolutions.sharepoint.com/:x:/r/sites/NoviSci2/Shared%20Documents/Operations/2023%20Effort%20Planning/Effort%20Planning.xlsm?d=wf4d990570ef5417e99ffb3bfafcb4bc6&csf=1&web=1&e=bridvl

(require 'url)

(defun dp-extract-filename-from-url (url-string)
  (let* ((parsed-url (url-generic-parse-url url-string))
         (filename (url-filename parsed-url))
         (unhexed-filename (url-unhex-string filename))
         (prefix "/:x:/r/sites/NoviSci2/Shared Documents/")
         (unprefixed-filename (string-replace prefix "" unhexed-filename))
         (unpostfixed-filename (replace-regexp-in-string "\\?[^?]*" "" unprefixed-filename))
         (newseparators-filename (replace-regexp-in-string "/" " > " unpostfixed-filename)))
    newseparators-filename))
