;;; lisp/ess+/ess-history.el -*- lexical-binding: t; -*-

(require 'comint)

;; these are the two functions (that I am aware of) which ultimately send inputs
;; to the inferior process
(advice-add #'ess-eval-linewise :after #'ess-history-for-eval-linewise)
(advice-add #'ess-send-string :after #'ess-history-for-send-string)

(defun ess-history-for-send-string (process string &optional visibly _message _type)
  "Add a string to the buffer history associated with a process.
This function is intended to be used as advice for
`ess-send-string'. When VISIBLY is t then the work is handed off
to `ess-eval-linewise' so we will allow other handlers to deal
with that case and this function becomes a no-op."
  (unless (eq visibly t)
    (with-current-buffer (process-buffer process)
      (comint-add-to-input-history (propertize (dp-strip-lead-trail-newline string)
                                               'font-lock-face
                                               'comint-highlight-input)))))


(provide 'ess-history)
