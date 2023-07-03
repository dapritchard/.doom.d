;;; lisp/org-utilities/org-agenda-knowledgebase-open.el -*- lexical-binding: t; -*-

;; ;; Potential plan: set org-agenda-after-show-hook and call org-agenda-switch-to
;; (defun dp-org-agenda-knowledgebase-open ()
;;   (interactive)
;;   (let (())))

(defun dp-knowledgebase-open-or-follow-link ()
  (if (dp-org-check-attach)
      (org-attach-open)
    ))

(defun dp-org-check-attach ()
  "Check whether the current headline has an Org Attach attachment"
  (let* ((tags (org-get-tags))
         (has-attach-tag (member "ATTACH" tags)))
    has-attach-tag))

(defun dp-knowledgebase-follow-link ()
  (let ((link-or-url (org-entry-get nil "URL" 'selective)))
    (unless link-or-url
      (error "No 'URL' property found"))
    ))

(org-link-open-from-string "[[https://targetpharmasolutions.sharepoint.com/:x:/r/sites/NoviSci2-StandardizedPHRs/Shared%20Documents/P0076%20-%20Market%20Insights/01_Atopic%20Dermatitis/SAP%20and%20VSF/AD_Test_Plan.xlsx?d=w1f6da75ffec34fa3aae1d7803da38d4b&csf=1&web=1&e=VzZbmC][P0076 - Market Insights > 01_Atopic Dermatitis > SAP and VSF > AD_Test_Plan.xlsx]]")

;; (if (org-link-p "[[https://example.com][Example Link]]")

(defun dp-org-link-p (s)
  (unless (stringp s)
    (error "'s' must be a string"))
  (let* ((link-re "\[\[.+\]\]\[.+\]")
         (is-link (string-match link-re s)))
    is-link))
