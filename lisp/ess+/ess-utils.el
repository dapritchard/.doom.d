;; -*- lexical-binding: t -*-

(defun dp-ess-eval-word ()
  "Evaluate the word at point in the inferior R process."
  (interactive)
  (ess-eval-linewise (dp-selected-text-or-word)))

(defun dp-ess-str-word ()
  "Get the 'str' of the word at point in the inferior R process."
  (interactive)
  (let* ((section (dp-selected-text-or-word))
         (str-call (concat "str(" section ")")))
    (ess-eval-linewise str-call)))

(defun dp-ess-word-at-point-to-string ()
  "Find the word at point and return it as a string."
  (save-excursion
    (skip-chars-forward " \t\n")
    (buffer-substring-no-properties
     (+ (point) (skip-chars-backward "a-zA-Z0-9._"))
     (+ (point) (skip-chars-forward "a-zA-Z0-9._")))))

(defun dp-selected-text-or-word ()
  (if (use-region-p)
      (buffer-substring-no-properties (mark) (point))
    (dp-ess-word-at-point-to-string)))
