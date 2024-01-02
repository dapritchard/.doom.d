;;; lisp/org-roam-utilities/org-roam-utilities.el -*- lexical-binding: t; -*-

(defun dp-capture-knowledgebase ()
  (interactive)
  (let ((node (org-roam-node-create))
        (templates `(("k"
                      "knowledgebase"
                      entry
                      "* %?"
                      :target (file ,dp-knowledgebase-path)))))
    (org-roam-capture- :node node :templates templates)))

(defun dp-expand-snippet-knowledgebase ()
  (when (string= (dp-get-indirect-file-buffer)
                 dp-knowledgebase-path)
    (yas-expand-snippet (yas-lookup-snippet "knowledgebase" 'org-mode))))

(defun dp-get-indirect-file-buffer ()
  (buffer-file-name (or (buffer-base-buffer) (current-buffer))))

(add-hook 'org-capture-mode-hook #'dp-expand-snippet-knowledgebase)
