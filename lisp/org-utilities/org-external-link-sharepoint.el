;;; lisp/org-utilities/org-external-link-sharepoint.el -*- lexical-binding: t; -*-

;; Example data:
;; https://targetpharmasolutions.sharepoint.com/:x:/r/sites/NoviSci2/Shared%20Documents/Operations/2023%20Effort%20Planning/Effort%20Planning.xlsm?d=wf4d990570ef5417e99ffb3bfafcb4bc6&csf=1&web=1&e=bridvl

(require 'url)
(require 'org-cliplink)

(defun dp-org-clip-sharepoint ()
  "Create an Org link from a SharePoint clipboard link."
  (interactive)
  (let* ((url-string (org-cliplink-clipboard-content))
         (sharepoint-path (dp-extract-sharepointpath-from-url url-string))
         (link-string (format "[[%s][%s]]"
                              url-string
                              sharepoint-path)))
    (insert link-string)))

;; TODO: extract `prefix' as a customizeable parameter
;; TODO: throw error if the `:type' returned by `url-generic-parse-url' isn't
;; ="https"=?
(defun dp-extract-sharepointpath-from-url (url-string)
  "Extract the Sharepoint path from a SharePoint URL string."
  (let* ((parsed-url (url-generic-parse-url url-string))
         (filename (url-filename parsed-url))
         (unhexed-filename (url-unhex-string filename))
         (prefix "/:x:/r/sites/NoviSci2/Shared Documents/")
         (unprefixed-filename (string-replace prefix "" unhexed-filename))
         (unpostfixed-filename (replace-regexp-in-string "\\?[^?]*" "" unprefixed-filename))
         (newseparators-filename (replace-regexp-in-string "/" " > " unpostfixed-filename)))
    newseparators-filename))
